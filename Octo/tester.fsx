//REQUIRED TESTING RESOURCES
/// ARM Status bits
type Flags = { N: bool; C:bool; Z: bool; V:bool}


////////////////////////ARM register names and operations/////////////////////////////
   

/// ARM register names
/// NB R15 is the program counter as read
[<Struct>]
type RName = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 
                | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

   

/// Map used to convert strings into RName values, 
/// includes register aliasses PC, LR, SP
let regNames = 
    Map.ofList [ 
        "R0",R0 ; "R1",R1 ; "R2",R2 ; "R3",R3 ; "R4",R4 ; "R5",R5
        "R6",R6 ; "R7",R7 ; "R8",R8 ; "R9", R9 ; "R10",R10 ; "R11",R11 ; 
        "R12",R12 ; "R13",R13 ; "R14",R14 ; "R15",R15 ; 
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
    regNums |> Map.toList 
    |> List.map (fun (rn,n)->(n,rn)) |> Map.ofList

/// Property on RName to return register number, for convenience
/// Aliasses not included, since they are not RNames
type RName with
    /// Return the number of a register as an integer
    member r.RegNum = regNums.[r]
    
/// Return a register name from an integer
let register n = 
    if 0 <= n && n < 16 
    then inverseRegNums.[n] 
    else (failwithf "Register %d does not exist!" n)

/// Type to represent the contents of one memory location
/// 'INS is a parameter set to the type of an instruction
/// needed because instruction type is only defined
/// at top level.
type MemLoc<'INS> =
    | DataLoc of uint32
    | Code of 'INS

/// type to represent a (word) address
/// there is some ambiguity. Does this contain the real address
/// which is always divisible by 4
/// or does it contain the word number (real address divided by 4)
/// either way multiply/divide by 4 will cause problems!
/// document this well and be consistent.

///For individual project: chosen to be real address
type WAddr = | WA of uint32

/// type to represent memory
type MachineMemory<'INS> = Map<WAddr,MemLoc<'INS>>

/// ARM state as values of all registers and status bits
/// NB PC can be found as R15 - 8. (Pipelining)
type DataPath<'INS> = {
    Fl: Flags; // Flags
    Regs:Map<RName,uint32> // map representing registers. 
                            // Must be correctly initialised
    MM: MachineMemory<'INS> // map showing the contents of all memory
    }    
/// ARM execution conditions
type Condition =

    | Ceq
    | Cne
    | Cmi
    | Cpl
    | Chi
    | Chs
    | Clo
    | Cls
    | Cge
    | Cgt
    | Cle
    | Clt
    | Cvs
    | Cvc
    | Cnv // the "never executed" condition NV - not often used!
    | Cal // the "always executed condition "AL". Used by default on no condition

/// classes of instructions (example, add/change this is needed)
type InstrClass = | DP | MEM

/// specification of set of instructions
type OpSpec = {
    InstrC: InstrClass
    Roots: string list
    Suffixes: string list
}

type SymbolTable = Map<string,uint32>

/// result returned from instruction-specific module parsing
/// an instruction class. If symbol definitions are found in a 
/// symbol table then a complete parse will be output
/// otherwise some fields will be None
type Parse<'INS> = {
        /// value representing instruction. NB type varies with instruction class
        PInstr: 'INS 
        /// name and value of label defined on this line, if one is.
        PLabel: (string * uint32) option 
        /// number of bytes in memory taken up by this instruction
        PSize: uint32 
        /// execution condition for instruction
        PCond: Condition
    }

/// data given to instruction-specific parse function
type LineData = {
    /// memory address this instruction is loaded from? Must be word address
    LoadAddr: WAddr 
    /// name of label defined on this line, if one exists
    Label: string option 
    /// table of symbols with defined values. 
    /// if this is given we are phase 2 and all symbols should be defined
    /// if this is not given we are phase 1 and no symbols are defined
    SymTab: SymbolTable option
    /// opcode string
    OpCode: string
    /// string of all the operands
    Operands: string
}


/// Strings with corresponding execution condition
/// Note some conditions have multiple strings
/// Note "" is a valid condition string (always execute condition)
let condMap = [ "EQ",Ceq ; "NE",Cne ; "MI",Cmi ; "PL",Cpl ; "HI", Chi ; 
                "HS",Chs ; "LO",Clo ; "LS",Cls ; "GE",Cge ; "GT", Cgt ; 
                "LE", Cle ; "LT", Clt ; "VS",Cvs ;  "VC",Cvc ;
                "NV",Cnv ; "AL",Cal ; "",Cal] |> Map.ofList

/// list of all strings representing execution conditions
/// includes ""
let condStrings = 
    condMap
    |> Map.toList
    |> List.map fst
    |> List.distinct    

/// generate all possible opcode strings for given specification
/// each string is paired with info about instruction
/// and the three parts of the opcode
let opCodeExpand (spec: OpSpec) 
    //    opcode    class        root    suffix   instr cond
    : Map<string, InstrClass * (string * string * Condition)> =
    spec.Roots
    |> List.collect (fun r -> 
        spec.Suffixes
        |> List.collect (fun s -> 
            condStrings
            |> List.map (fun c -> r+s+c, (spec.InstrC,(r,s, condMap.[c])))))
            |> Map.ofList

/// function used to change PInstr field of a Result<Parse<'INS>,'E>
/// the output has this field mapped with fMap
/// or if Error has this value changed by fMapE
let pResultInstrMap fMap fMapE paRes =
    match paRes with
    | Ok ({PInstr=ins} as pr) -> 
        // Note subtle point. {pr with Pinst = ...} will not work here
        // That is because applying fmap changes the type of PInstr
        // and therefore the type of the record.
        Ok {
        PInstr = fMap ins 
        PLabel = pr.PLabel
        PCond = pr.PCond
        PSize = pr.PSize
        }
    | Error e -> Error (fMapE e)
/// FLEXIBLE OP2 TOOLS
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

type Literal = uint32 //{K: uint32; R: int} //// literal value = (K % 256) rotated right by (R &&& 0xF)*2. 
type Reg = RName
type SVal = NumericValue of int | RValue of Reg
type Shift = Reg*SVal
    
type Op2 = 
    | LiteralData of Literal //// remove duplication of uint32 definition if K,R not needed
    | Register of Reg
    | LSL of Shift
    | ASR of Shift
    | LSR of Shift
    | ROR of Shift
    | RRX of Reg

let doShift (ro: Shift) (cpuData: DataPath<'INS>) bitOp =
    let rOp2 = fst ro |> fun a -> Map.find a cpuData.Regs
    let sv = snd ro
    let f = fun a b -> bitOp a b
    match sv with 
    |NumericValue s -> LiteralData(uint32(  f rOp2 s))
    |RValue r -> 
        int(Map.find r cpuData.Regs)%32
        |> fun newShift -> LiteralData(uint32(f rOp2 newShift))

let makeRRX (rOp2:Reg) (cpuData:DataPath<'INS>) =
    let newMSB = if cpuData.Fl.C then 0x80000000u else 0x00000000u
    Map.find rOp2 cpuData.Regs
    |> fun reg -> LiteralData(uint32( newMSB + (reg>>>1)))    

let flexOp2 (op2:Op2) (cpuData:DataPath<'INS>) = 
    match op2 with
    | LiteralData literalData -> LiteralData literalData
    | Register register -> Register register      
    | LSL shift -> doShift shift cpuData (<<<)
    | ASR shift -> doShift shift cpuData (fun a b -> (int a) >>> b |> uint32)
    | LSR shift -> doShift shift cpuData (>>>)
    | ROR shift -> doShift shift cpuData (fun a n -> (a >>> n) ||| (a <<< (32-n)))
    | RRX r -> makeRRX r cpuData

//Testing parameters for FlOp2
//let (defaultFlags: Flags) = {N=false;Z=false;C=false;V=false}
//let (defaultRegs: Map<RName,uint32>) = Map.map (fun _ (s:string) -> 2u) regStrings
////let (defaultMM: MachineMemory<'INS>) = 
//let (cpuData: DataPath<'INS>) = {Fl = defaultFlags; Regs = defaultRegs; MM = defaultMM}

///TOKENIZER
type RegParameters = {dest: RName option; op1: RName; op2: Op2}
type Instr = {ic: InstrClass; rt:string; sf:string; cnd:Condition ;ap: RegParameters}
type ErrInstr = string ////Check usefulness of this
type Token = Dest of RName option| Op1' of RName | Op2' of Op2
let removeComment (txt:string) =
    txt.Split(';')
    |> function 
        | [|x|] -> x 
        | [||] -> "" 
        | lineWithComment -> lineWithComment.[0]
let splitIntoWords ( line:string ) =
        line.Split( ([||] : char array), 
            System.StringSplitOptions.RemoveEmptyEntries)
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
    |true -> Some true
    |false -> None
    
let tokenizer (str:string) =     ////Resolve shifts by negative integers and positive integers >31 - VisUAL ignores such shifts?
    removeComment str
    |> splitIntoWords
    |> Array.toList 
    |> String.concat ""
    |> fun s -> s.Split(',')
    |> function
        |[|q0;q1|] ->
            match q0,q1 with
                |Reg' a, Reg' b -> [Dest None; Op1' a; Op2' (Register b)] |> Ok
                |Reg' a, Nmr' b -> [Dest None; Op1' a; Op2' (LiteralData (uint32 b))] |> Ok
                |_ -> Error "Invalid syntax: format must be \"op1, op2\""
        |[|q0;q1;q2|] ->
            match q0,q1,q2 with
                |Reg' a, Reg' b, Reg' c -> [Dest (Some a); Op1' b; Op2' (Register c)] |> Ok
                |Reg' a, Reg' b, Nmr' c -> [Dest (Some a); Op1' b; Op2' (LiteralData (uint32 c))] |> Ok
                |_ -> Error "Invalid syntax: format must be \"dest, op1, op2\""
        |[|q0;q1;q2;q3|] ->  
            match q0,q1,q2,q3 with
                |Reg' a, Reg' b, Reg' c, ASR' d -> [Dest (Some a); Op1' b; Op2' (ASR(c,d))] |> Ok
                |Reg' a, Reg' b, Reg' c, LSR' d -> [Dest (Some a); Op1' b; Op2' (LSR(c,d))] |> Ok
                |Reg' a, Reg' b, Reg' c, LSL' d -> [Dest (Some a); Op1' b; Op2' (LSL(c,d))] |> Ok
                |Reg' a, Reg' b, Reg' c, ROR' d -> [Dest (Some a); Op1' b; Op2' (ROR(c,d))] |> Ok
                |Reg' a, Reg' b, Reg' c, RRX' _ -> [Dest (Some a); Op1' b; Op2' (RRX(c))] |> Ok
                |_ -> Error "Invalid syntax: format must be \"dest, op1, op2, [shift #X]\" or \"dest, op1, op2, [shift Rn]\" "
        |_ -> Error "Invalid syntax: check inputs"
            
/// specification for ARITH set of instructions
let dPSpec = {
    InstrC = DP
    Roots = ["ADD";"ADC";"SUB";"SBC";"RSB";"RSC";"CMP";"CMN"]
    Suffixes = [""; "S"]
}

/// map of all possible opcodes recognised
let opCodes = opCodeExpand dPSpec

/// main function to parse a line of assembler
/// ls contains the line input
/// and other state needed to generate output
/// the result is None if the opcode does not match
/// otherwise it is Ok Parse or Error (parse error string)
let parse (ls: LineData) : Result<Parse<Instr>,string> option =
    ls.Operands
    |> tokenizer 
    |> Result.map (fun (a:Token list) ->
        a 
        |>function
        |[Dest de; Op1' o1; Op2' o2] -> {dest = de; op1 = o1; op2 = o2}
        |_ -> failwithf "Bad output from tokenizer")
    |>function
        |Ok ops ->
            let parse' (instrC, (root,suffix,pCond)) =
                let (WA la) = ls.LoadAddr // memory address this instruction is loaded
                Ok { 
                    // Normal (non-error) return from result monad
                    // This is the instruction determined from opcode, suffix and parsing operands.
                    PInstr = {ic=instrC; rt=root; sf=suffix; cnd=pCond; ap=ops};
                    // This is normally the line label as contained in
                    // ls together with the label's value which is normally
                    // ls.LoadAddr. Some type conversion is needed since the
                    // label value is a number and not necessarily a word address
                    // it does not have to be div by 4, though it usually is
                    PLabel = ls.Label |> Option.map (fun lab -> lab, la) ; 
                    // this is the number of bytes taken by the instruction
                    // word loaded into memory. For arm instructions it is always 4 bytes. 
                    // For data definition DCD etc it is variable.
                    //  For EQU (which does not affect memory) it is 0
                    PSize = 4u; 
                    // the instruction condition is detected in the opcode and opCodeExpand                 
                    // has already calculated condition already in the opcode map.
                    // this part never changes
                    PCond = pCond 
                    }
            Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
            |> Option.map parse' // if unknown keep none, if known parse it.
        |Error x -> Some (Error x)
////Create dummy LineData and test the above.
let p:LineData = 
    {LoadAddr = WA 0u;
    Label = None;
    SymTab = None;
    OpCode = "ADDSNE";
    Operands = "R0,R1,R2k"}
let qq = p|>parse