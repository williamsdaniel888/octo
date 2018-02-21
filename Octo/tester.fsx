///TACKLING OP2

type Flags = { N: bool; C:bool; Z: bool; V:bool}
    
/// ARM register names
[<Struct>]
type RName = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
                | R9 | R10 | R11 | R12 | R13 | R14 | R15
    
/// ARM state as values of all registers and status bits
type DataPath = {Fl: Flags; Regs:Map<RName,uint32>}
    
/// Map used to convert strings into RName values, 
/// includes register aliasses PC, LR, SP
let regNames = Map.ofList [ 
                        "R0",R0 ; "R1",R1 ; "R2",R2 ; "R3",R3 ; "R4",R4 ; 
                        "R5",R5 ; "R6",R6 ; "R7",R7 ; "R8",R8 ;
                        "R9", R9 ; "R10",R10 ; "R11",R11 ; "R12",R12 ; 
                        "R13",R13 ; "R14",R14 ; "R15",R15 ; 
                        "PC",R15 ; "LR",R14 ; "SP",R13 
                    ] 
    
// various functions used to convert between string, RName, and register number
    
/// Inverse of regNames, used to convert RName values to strings
/// NB The string chosen will always be the register (not alias)
let regStrings = 
    regNames
    |> Map.toList
    |> List.map (fun (s,rn)-> (rn,s)) 
    |> List.filter (fun (_,s:string) -> s.StartsWith "R")
    |> Map.ofList
    
/// Map converts RName into register number (no aliasses)
let regNums = Map.map (fun _ (s:string) -> int (s.[1..])) regStrings
    
/// Map converts register number into RName (no aliasses)
let inverseRegNums = 
    regNums 
    |> Map.toList 
    |> List.map (fun (rn,n)->(n,rn)) 
    |> Map.ofList
    
/// Property on RName to return register number, for convenience
/// Aliasses not included, since they are not RNames
type RName with
    /// Return the number of a register as an integer
    member r.RegNum = regNums.[r]
    
/// Return a register name from an integer
let register n = if 0 <= n && n < 16 
                    then inverseRegNums.[n] 
                    else (failwithf "Register %d does not exist!" n)

let allowedLiterals = 
    [0..2..30] 
    |> List.allPairs [0u..255u] 
    |> List.map (fun (lit,n) -> (lit >>> n) + (lit <<< 32-n))
    |> Set.ofList

let makeLiteral (lit: uint32) = 
    Set.contains lit allowedLiterals
    |> function
        |true -> Some lit
        |false -> None

/// literal value = (K % 256) rotated right by (R &&& 0xF)*2. 
type Literal = uint32 //{K: uint32; R: int}
type Reg = RName
type SVal = NumericValue of int | RValue of Reg
type Shift = Reg*SVal
    
type Op2 = 
    | LiteralData of Literal //remove duplication of uint32 definition if K,R not needed
    | Register of Reg
    | LSL of Shift
    | ASR of Shift
    | LSR of Shift
    | ROR of Shift
    | RRX of Reg

let doROR a n = (a >>> n) ||| (a <<< (32-n))

let doShift (ro: Shift) (cpuData:DataPath) bitOp =
    let rOp2 = fst ro |> fun a -> Map.find a cpuData.Regs
    let sv = snd ro
    let f = fun a b -> bitOp a b
    match sv with 
    |NumericValue s -> LiteralData(uint32(  f rOp2 s))
    |RValue r -> 
        int(Map.find r cpuData.Regs)%32
        |> fun newShift -> LiteralData(uint32(f rOp2 newShift))

let makeRRX (rOp2:Reg) (cpuData:DataPath) =
    let newMSB = if cpuData.Fl.C then 0x80000000u else 0x00000000u
    Map.find rOp2 cpuData.Regs
    |> fun reg -> LiteralData(uint32( newMSB + (reg>>>1)))    

let flexOp2 (op2:Op2) (cpuData:DataPath) = 
    match op2 with
    | LiteralData literalData -> LiteralData literalData
    | Register register -> Register register      
    | LSL shift -> doShift shift cpuData (<<<)
    | ASR shift -> doShift shift cpuData (fun a b -> (int a) >>> b |> uint32)
    | LSR shift -> doShift shift cpuData (>>>)
    | ROR shift -> doShift shift cpuData doROR
    | RRX r -> makeRRX r cpuData

///Testing parameters
let (defaultFlags: Flags) = {N=false;Z=false;C=false;V=false}
let (defaultRegs: Map<RName,uint32>) = Map.map (fun _ (s:string) -> 2u) regStrings
let (cpuData: DataPath) = {Fl = defaultFlags; Regs = defaultRegs}
///

///TOKENIZER

type ErrInstr = string
//let h = "R0,R1,R0;43"
//let removeComment (txt:string) =
//    txt.Split(';')
//    |> function 
//        | [|x|] -> x 
//        | [||] -> "" 
//        | lineWithComment -> lineWithComment.[0]
//let splitIntoWords ( line:string ) =
//        line.Split( ([||] : char array), 
//            System.StringSplitOptions.RemoveEmptyEntries)
//h |> removeComment |> splitIntoWords |> Array.toList |> String.concat ""

type Token = Dest of RName| Op1' of RName | Op2' of Op2
type LexData = {Txt: string; Args: string list}

//Resolve shifts by negative integers and positive integers >31 - VisUAL ignores such shifts?
let (|Reg'|_|) (str:string) = if str.StartsWith("R") then regNames.TryFind str else None
let (|Nmr'|_|) (str:string) = if str.StartsWith("#") then Some (int str.[1..]) else None
let (|ASR'|_|) (str:string) = 
    str.StartsWith("ASR") |> function
    |true -> 
        str.[3..] |> function
            |Nmr' y -> Some (NumericValue y)
            |Reg' y -> Some (RValue y)
            |_ -> None
    |false -> None
let (|LSR'|_|) (str:string) = 
    str.StartsWith("LSR") |> function
    |true -> 
        str.[3..] |> function
            |Nmr' y -> Some (NumericValue y)
            |Reg' y -> Some (RValue y)
            |_ -> None
    |false -> None
let (|LSL'|_|) (str:string) = 
    str.StartsWith("LSL") |> function
    |true -> 
        str.[3..] |> function
            |Nmr' y -> Some (NumericValue y)
            |Reg' y -> Some (RValue y)
            |_ -> None
    |false -> None
let (|ROR'|_|) (str:string) = 
    str.StartsWith("ROR") |> function
    |true -> 
        str.[3..] |> function
            |Nmr' y -> Some (NumericValue y)
            |Reg' y -> Some (RValue y)
            |_ -> None
    |false -> None
let (|RRX'|_|) (str:string) = 
    str.StartsWith("RRX") |> function
    |true -> Some RRX //WATCH OUT FOR THIS CAUSING TROUBLE
    |false -> None
    
let chunker (str:string) = //Convert to Result monad
    str.Split(',')
    |> function
        |[|q1;q2|] -> 
            let s1 = q1 |> function
                |Reg' x -> Op1' x
                |_ -> failwithf "Invalid op1" 
            let s2 = q2 |> function
                |Reg' x -> Op2' (Register x)
                |Nmr' x -> Op2' (LiteralData (uint32 x)) //Immediate is stored as uint32 - must resolve negative immediates w implication for opcode
                |_ -> failwithf "Invalid op2" 
            [s1; s2] //|> Ok
        |[|q0;q1;q2|] ->
            let s0 = q0 |> function
                |Reg' x -> Dest x
                |_ -> failwithf "Invalid dest" 
            let s1 = q1 |> function
                |Reg' x -> Op1' x
                |_ -> failwithf "Invalid op1" 
            let s2 = q2 |> function
                |Reg' x -> Op2' (Register x)
                |Nmr' x -> Op2' (LiteralData (uint32 x)) //Immediate is stored as uint32 - must resolve negative immediates w implication for opcode
                |_ -> failwithf "Invalid op2" 
            [s0; s1; s2] //|> Ok
        |[|q0;q1;q2;q3|] ->  
            let s0 = q0 |> function
                |Reg' x -> Dest x
                |_ -> failwithf "Invalid dest" 
            let s1 = q1 |> function
                |Reg' x -> Op1' x
                |_ -> failwithf "Invalid op1" 
            let s2 = 
                let ro = q2 |> function
                    |Reg' x -> x
                    |_ -> failwithf "Invalid op2 base register" 
                q3 |> function
                    |ASR' x -> Op2' (ASR(ro,x))
                    |LSR' x -> Op2' (LSR(ro,x))
                    |LSL' x -> Op2' (LSL(ro,x))
                    |ROR' x -> Op2' (ROR(ro,x))
                    |RRX' _ -> Op2' (RRX ro)
                    |_ -> failwithf "Invalid op2 shift instruction" 
            [s0; s1; s2] //|> Ok
        |_ -> failwithf "Invalid input"
        