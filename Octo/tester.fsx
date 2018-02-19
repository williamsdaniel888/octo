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

let f = "R0R1R2LSR#5"
let removeComment (txt:string) =
    txt.Split(';')
    |> function 
        | [|x|] -> x 
        | [||] -> "" 
        | lineWithComment -> lineWithComment.[0]
let splitIntoWords ( line:string ) =
        line.Split( ([||] : char array), 
            System.StringSplitOptions.RemoveEmptyEntries)
f |> removeComment |> splitIntoWords |> Array.toList |> String.concat ""

type Token = Dest of RName | Top1 of RName | Top2 of Op2
type LexData = {Txt: string; Args: string list}

open System.Text.RegularExpressions
let (|FirstRegexGroup|_|) pattern input =
    let m = Regex.Match(input,pattern)
    if (m.Success) then Some m.Groups.[0].Value else None  

let nextToken (state:LexData) = 
    match state.Txt with
    | FirstRegexGroup "R[0-9]{1,2}([A-Z]{3}(\#\-?[0-9]{1,3})?(R[0-9]{1,2})?)?" host ->
        let mChars = String.length host
        if mChars = 0 then 
            failwithf "Unexpected 0 character match '%s'" state.Txt
        let state' = {state with Txt = state.Txt.[mChars..]}
        Some (host,state')
    | FirstRegexGroup "\#\-?[0-9]{1,3}" host ->
        let mChars = String.length host
        if mChars = 0 then 
            failwithf "Unexpected 0 character match '%s'" state.Txt
        let state' = {state with Txt = state.Txt.[mChars..]}
        Some (host,state')
    | _ -> None
        
let tokenize str =
    let rec tokenize' (st:LexData) =
        match nextToken st with
        |None -> st
        |Some (nt, st') -> tokenize' {st' with Args = List.append (st'.Args) [nt]}
    tokenize' {Txt=str; Args=[]} |> function |x -> x.Args
tokenize "R0R1R2RRX#-334"