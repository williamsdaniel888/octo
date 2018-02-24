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
type RotConstant = {K: uint32; R: int} // literal value = (K % 256) rotated right by (R &&& 0xF)*2.
//type ImmConstant = uint32
type Literal = |RC of RotConstant //|Swappable of uint32//IC of ImmConstant  would allow other values permitted in ARM documentation
type Reg = RName
type SVal = NumericValue of int | RValue of Reg
type Shift = Reg*SVal

type Op2 = 
    | LiteralData of Literal
    | Register of Reg
    | LSL of Shift
    | ASR of Shift
    | LSR of Shift
    | ROR of Shift
    | RRX of Reg
    | IMM12 of uint32

type RegParameters = {dest: RName option; op1: RName; op2: Op2}
type Instr = {ic: InstrClass; rt:string; sf:string; cnd:Condition ;ap: RegParameters}
type ErrInstr = string ////Check usefulness of this

//Map of literals created by rotating for all possible values of {K,R}
let allowedLiterals = 
    [0..2..30] 
    |> List.allPairs [0u..255u] 
    |> List.map (fun (lit,n) -> ((lit >>> n) + (lit <<< 32-n)),(lit,n))
    |> Map.ofList

////Active patterns to test whether an immediate is valid
////format: 0x00XY00XY
//let (|ImmP1|_|) (i:uint32) =    
//    let x = i.ToString("X8").[2..2]
//    let y = i.ToString("X8").[3..3]
//    if i.ToString("X8") = ("00"+x+y+"00"+x+y) 
//        then Some i else None
////format: 0xXY00XY00
//let (|ImmP2|_|) (i:uint32) =    
//    let x = i.ToString("X8").[0..0]
//    let y = i.ToString("X8").[1..1]
//    if i.ToString("X8") = (x+y+"00"+x+y+"00") 
//        then Some i else None
////format: 0xXYXYXYXY
//let (|ImmP3|_|) (i:uint32) =    
//    let x = i.ToString("X8").[2..2]
//    let y = i.ToString("X8").[3..3]
//    if i.ToString("X8") = (x+y+x+y+x+y+x+y) 
//        then Some i else None

//verifies whether a uint32 is a valid immediate
let checkOp2Literal (imm: uint32) = 
    Map.containsKey imm allowedLiterals
    |> function
        |true -> 
            allowedLiterals.[imm]
            |> fun a -> {K = fst a; R = snd a} |> RC |> LiteralData |> Ok
        |false -> Error "CO2L: Invalid literal, must be of format N ROR 2M, 0u<=N<=255u, 0<=M<=15"
            //if imm >= 0x8000000u
            //    then imm |> Swappable |> LiteralData |> Ok
            //    |ImmP1 x -> x |> IC |> LiteralData |> Ok
            //    |ImmP2 x -> x |> IC |> LiteralData |> Ok
            //    |ImmP3 x -> x |> IC |> LiteralData |> Ok
            
//verifies whether a register is a valid argument for op2
let checkOp2Register (r:RName) =
    if (r.RegNum>=0 && r.RegNum<=12) || r.RegNum=14 
        then Ok r
        else Error "CO2R: Invalid register value, must be R0-R12 or R14"

//Verify whether input is a valid imm12
let (|Imm12'|_|) (i:uint32) = 
    if i>=0u && i<=4095u then Some i else None
let checkImm12 (r: uint32) =
    match r with
        |Imm12' a -> IMM12 a |> Ok
        |_-> Error "CI12: Invalid imm12 value, must be 0<=n<=4095"

///TOKENIZER
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
let (|NI'|_|) (str:string) = if str.StartsWith("#") then Some ((int str.[1..])|> uint32) else None
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
    
let tokenizer (str:string) =
    removeComment str
    |> splitIntoWords
    |> Array.toList 
    |> String.concat ""
    |> fun s -> s.Split(',')
    |> function
        |[|q0;q1|] ->
            match q0,q1 with
                |Reg' a, Reg' b -> [Dest None; Op1' a; Op2' (Register b)] |> Ok
                |Reg' a, Nmr' b -> 
                    checkOp2Literal(uint32(b)) |> Result.map (fun x -> [Dest None; Op1' a; Op2' x])
                |_ -> Error "TOK: Invalid syntax: format must be \"op1, op2\""
        |[|q0;q1;q2|] ->
            match q0,q1,q2 with
                |Reg' a, Reg' b, Reg' c -> [Dest (Some a); Op1' b; Op2' (Register c)] |> Ok
                |Reg' a, "R15", NI' c |Reg' a, "PC", NI' c ->
                    checkImm12 c |> Result.map (fun x-> [Dest (Some a); Op1' R15; Op2' x]) 
                |Reg' a, Reg' b, Nmr' c ->
                    checkOp2Literal(uint32(c)) |> Result.map (fun x-> [Dest (Some a); Op1' b; Op2' x]) 
                |_ -> Error "TOK: Invalid syntax: format must be \"dest, op1, op2\""
        |[|q0;q1;q2;q3|] ->  
            match q0,q1,q2,q3 with
                |Reg' a, Reg' b, Reg' c, ASR' d -> [Dest (Some a); Op1' b; Op2' (ASR(c,d))] |> Ok
                |Reg' a, Reg' b, Reg' c, LSR' d -> [Dest (Some a); Op1' b; Op2' (LSR(c,d))] |> Ok
                |Reg' a, Reg' b, Reg' c, LSL' d -> [Dest (Some a); Op1' b; Op2' (LSL(c,d))] |> Ok
                |Reg' a, Reg' b, Reg' c, ROR' d -> [Dest (Some a); Op1' b; Op2' (ROR(c,d))] |> Ok
                |Reg' a, Reg' b, Reg' c, RRX' _ -> [Dest (Some a); Op1' b; Op2' (RRX(c))] |> Ok
                |_ -> Error "TOK: Invalid syntax: format must be \"dest, op1, op2, [shift #X]\" or \"dest, op1, op2, [shift Rn]\" "
        |_ -> Error "TOK: Invalid syntax: check inputs"
//tokenizer "R0,PC,#0"
            
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
        )
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

//Flexible op2 evaluation tools

//Checks validity of input Shift, performs shift on register if possible
let doShift (ro: Shift) (cpuData: DataPath<Instr>) bitOp =
    let rvalue = fst ro |> fun a -> Map.find a cpuData.Regs
    let svalue = snd ro
    match svalue with 
    |NumericValue s -> 
        if s>=0 && s<=31 
            then
                bitOp rvalue (s%32)
                |> checkOp2Literal
            //Return an error message if s is out of bounds
            //Diverges from VisUAL behavior, follows TC's Slack guidance
            else Error "DS: Invalid shift, must be between 0<=s<=31" 
    |RValue r ->
        //Calculate unsigned register contents modulo 32, then perform shift
        //Diverges from VisUAL behavior, follows Tick 3 guidance
        checkOp2Register r
        |> Result.map (fun re ->
            int((Map.find re cpuData.Regs) &&& 0x1Fu) 
            |> bitOp rvalue )
        |> Result.bind checkOp2Literal
        
//Perform RRX on a register's contents
let makeRRX (rv:Reg) (cpuData:DataPath<Instr>) =
    let newMSB = if cpuData.Fl.C then 0x80000000u else 0x00000000u
    Map.find rv cpuData.Regs
    |> fun reg -> checkOp2Literal( newMSB + (reg>>>1))

//Evaluates a flexible op2 value, returns uint32
let flexOp2 (op2:Op2) (cpuData:DataPath<Instr>) = 
    match op2 with
    | LiteralData (RC {K=a;R=b}) ->  
            (a >>> b) + (a <<< 32-b)
            |> checkOp2Literal
    //| LiteralData (Swappable x) -> checkOp2Literal x
    | Register register -> 
        checkOp2Register register 
        |> Result.map (fun a -> Map.find a cpuData.Regs)
        |> Result.bind checkOp2Literal
    | LSL shift -> doShift shift cpuData (<<<)
    | ASR shift -> doShift shift cpuData (fun a b -> (int a) >>> b |> uint32)
    | LSR shift -> doShift shift cpuData (>>>)
    | ROR shift -> doShift shift cpuData (fun a n -> (a >>> n) ||| (a <<< (32-n)))
    | RRX r -> makeRRX r cpuData
    //

//ARITH FUNCTIONS
//only called if conditions permit evaluation
//assume arguments are pre-cleared to be sufficient and valid

let getFlags (res:int64) (isAdditive:bool) = 
    let negative = if (0x80000000L &&& res)<>0L then true else false
    let zero = if res &&& 0xffffffffL = 0L then true else false
    let carry =
        if isAdditive 
            then 
                if res>=0x100000000L then true else false
            else 
                if (res &&& 0x8000000000000000L <> 0L) then false else true
    let overflow = 
        if (res >= 2147483648L) || (res < -2147483648L) then true else false ///ISSUE: Overflow not computed correctly
    {N=negative;Z=zero;C=carry;V=overflow}

//HOF for arithmetic functions
let engine (args: Instr) (state: DataPath<Instr>) bitOp=
    let dest' = args.ap.dest
    let op1' = 
        args.ap.op1 
        |> fun a -> Map.find a state.Regs
        |> int64
    let op2' =
        flexOp2 args.ap.op2 state
        |> function
            |Ok x -> 
                match x with 
                |LiteralData (RC rc) -> (rc.K,rc.R) |> fun (lit,n) -> int64((lit >>> n) + (lit <<< 32-n))
                |_ -> failwithf "Bad arguments"
            |Error _-> failwithf "Bad arguments"
    let opsSameSign = ((op1' &&& op2' &&& 0x80000000L)=0L)
    let addness = if args.rt = "ADD" || args.rt = "ADC" || args.rt = "CMN" then true else false
    let result = bitOp op1' op2' state.Fl.C
    let flags' =
        if args.sf ="S"
            then (getFlags result addness)
            else state.Fl
    let regs' = 
        match dest' with
        |None -> state.Regs
        |Some x -> 
            Map.toList state.Regs 
            |> List.map (fun a -> 
                if fst a = x then (fst a, uint32(result)) else (fst a, snd a)
                )
            |> Map.ofList
    let mm' = 
        //let n1 = 
        //    match args.cnd with
        //    | Ceq -> (0x0u)<<<28
        //    | Cne -> (0x1u)<<<28
        //    | Cmi -> (0x4u)<<<28
        //    | Cpl -> (0x5u)<<<28
        //    | Chi -> (0x8u)<<<28
        //    | Chs -> (0x2u)<<<28
        //    | Clo -> (0x3u)<<<28
        //    | Cls -> (0x9u)<<<28
        //    | Cge -> (0xAu)<<<28
        //    | Cgt -> (0xCu)<<<28
        //    | Cle -> (0xDu)<<<28
        //    | Clt -> (0xBu)<<<28
        //    | Cvs -> (0x6u)<<<28
        //    | Cvc -> (0x7u)<<<28
        //    | Cnv -> (0xFu)<<<28
        //    | Cal -> (0xEu)<<<28
        //let n2 = 
        //    match args.rt with
        //    |"SUB" -> (0x2u)<<<21
        //    |"RSB" -> (0x3u)<<<21
        //    |"ADD" -> (0x4u)<<<21
        //    |"ADC" -> (0x5u)<<<21
        //    |"SBC" -> (0x6u)<<<21
        //    |"RSC" -> (0x7u)<<<21
        //    |"CMP" -> (0xAu)<<<21
        //    |"CMN" -> (0xBu)<<<21
        //let n3 = 
        //    match args.sf with
        //    |"S" -> (0x1u)<<<20
        //    |"" -> 0u
        //let n4 = 
        //    args.ap.op1.RegNum
        //    |> uint32
        //    |> fun a -> a<<<16
        //let n5 =
        //    args.ap.dest
        //    |> function 
        //        |Some x -> x.RegNum 
        //        //None corresponds to two cases:
        //        //if CMP, CMN, dest is not needed;
        //        //for the purposes of instruction storage only
        //        //if ADD,ADC,SUB,SBC,RSB,RSC
        //        //the destination is recorded as op1
        //        //Diverges from VisUAL - follows ARM documentation
        //        |None -> args.ap.op1.RegNum 
        //    |> uint32
        //    |> fun a -> a<<<12
        //let n6 = 
        //    args.ap.op2
        //    |> fun a -> flexOp2 a state 
        //    |> function
        //        |Ok x -> match x with |LiteralData (RC x) -> (0x1u<<<25) + ((uint32(x.R))<<<8) + x.K
        //let word = n1+n2+n3+n4+n5+n6
        Map.toList state.MM 
            |> List.map (fun a -> 
                if fst a = WA 0x0u then (fst a, Code args) else (fst a, snd a)
                )
            |> Map.ofList
    {Fl = flags'; Regs = regs'; MM = mm'}

let eval (parsedData:Result<Parse<Instr>,string> option) (state: DataPath<Instr>) =
    let instCond = 
        match parsedData with
            |None -> Error "EV: Invalid opcode"
            |Some (Error e) -> Error e
            |Some (Ok x) -> Ok (x.PInstr,x.PCond)
    let conditionSatisfied =
        let f = state.Fl
        instCond
        |> Result.map (fun a -> snd a)
        |> Result.map (fun b -> 
            match b with
            | Ceq -> if (f.Z = true) then true else false
            | Cne -> if (f.Z = false) then true else false
            | Cmi -> if (f.N = true) then true else false
            | Cpl -> if (f.N = false) then true else false
            | Chi -> if (f.C = true && f.Z = false) then true else false
            | Chs -> if (f.C = true) then true else false
            | Clo -> if (f.C = false) then true else false
            | Cls -> if (f.C = false || f.Z = true) then true else false
            | Cge -> if (f.N = f.V) then true else false
            | Cgt -> if (f.N = f.V && f.Z = false) = true then true else false
            | Cle -> if (f.N <> f.V || f.Z = true) then true else false
            | Clt -> if (f.N <> f.V) then true else false
            | Cvs -> if (f.V = true) then true else false
            | Cvc -> if (f.V = false) then true else false
            | Cnv -> false
            | Cal -> true
            )
    let instruction =
        instCond
        |> Result.map (fun a -> fst a)
    let parameters = 
        instCond
        |> Result.map (fun a -> fst a)
        |> Result.map (fun b -> b.ap)
    let destStatus =
        parameters
        |> Result.map (fun a ->
            match a.dest with
            |Some R15 -> "PC"
            |Some R13 -> "SP"
            |Some _ -> "strict"
            |None -> "n/a"
            )
    let op1Status =
        parameters
        |> Result.map (fun a ->
            match a.op1 with
            |R15 -> "PC"
            |R13 -> "SP"
            |_ -> "strict"
            )
    let op2Status =
        instruction
        |> Result.map (fun a ->
            let a1 =
                match (flexOp2 a.ap.op2 state) with
                |Ok (IMM12 _) -> "imm12"
                |Ok (LiteralData (RC x)) ->
                    match x.R with
                    | 0 -> "v.strict"
                    |_->"strict"
                |Ok (LSL (_,(NumericValue x))) ->
                    match x with
                    |0 |1 |2 |3 -> "v.strict"
                    |_->"strict"
                |Ok _ -> "strict" //covers Op2 LiteralData Swappable as well
                |Error _ -> "n/a"
            let a2 =
                match (flexOp2 a.ap.op2 state) with
                //|Ok (LiteralData (Swappable x)) -> 
                //    match a.rt with
                //    |"RSB"|"RSC" -> "n/a"
                //    |"ADD"|"SUB"|"CMP"|"CMN" ->
                //        if Map.containsKey (~~~x + 1u) allowedLiterals
                //            then "allowed"
                //            else "n/a"
                //    |"ADC" ->
                //        if Map.containsKey (~~~x + 2u) allowedLiterals
                //            then "allowed"
                //            else "n/a"
                //    |"SBC" ->
                //        if Map.containsKey (~~~x) allowedLiterals
                //            then "allowed"
                //            else "n/a"
                //    |_ -> "allowed"
                |Ok (LiteralData (RC _)) -> "allowed"
            a1,a2
        )
    let execOK =
        instCond
        |> Result.map (fun a -> fst a)
        |> Result.map (fun b -> b.rt,b.sf)
        |> Result.map (fun c ->
            match fst c with
            |"CMP"|"CMN" ->
                match destStatus,op1Status,op2Status with
                |Ok "n/a", Ok "strict", Ok ("strict","allowed") -> true
                |_ -> false
            |"ADC"|"SBC" -> 
                match destStatus,op1Status,op2Status with
                |Ok "strict", Ok "strict", Ok ("strict","allowed") -> true
                |_ -> false
            |"RSB"|"RSC" -> 
                match destStatus,op1Status,op2Status with
                |Ok "strict", Ok "strict", Ok ("strict","allowed") -> true
                |_ -> false
            |"SUB" ->
                match destStatus,op1Status,op2Status with
                |Ok "strict", Ok "strict", Ok ("strict","allowed") -> true
                |Ok "strict", Ok "SP", Ok ("strict","allowed") -> true
                |Ok "SP", Ok "SP", Ok ("v.strict","allowed") -> true
                |Ok "strict", Ok "PC", Ok ("imm12","allowed") -> 
                    if snd c = "" then true else false
                |_ -> false
            |"ADD" ->
                match destStatus,op1Status,op2Status with
                |Ok "strict", Ok "strict", Ok ("strict","allowed") -> true
                |Ok "strict", Ok "SP", Ok ("strict","allowed") -> true
                |Ok "SP", Ok "SP", Ok ("v.strict","allowed") -> true
                |Ok "PC", Ok "PC", Ok ("strict","allowed") -> 
                    if snd c = "" then true else false
                |Ok "strict", Ok "PC", Ok ("imm12","allowed") -> 
                    if snd c = "" then true else false
                |_ -> false
        )
    //let iValidated (b:Result<Instr,string>) =
    //    b
    //    |> Result.map (fun a ->
    //        let p = 
    //            match execOK with 
    //            |Ok x -> x 
    //            |Error _ -> false
    //        match p with
    //        |false -> a //invalid instruction, will not be corrected and will not executed
    //        |true -> //valid instruction, will be corrected if swappable
    //            match a.ap.op2 with
    //            |LiteralData (RC {K=k;R=r}) -> a
    //            |LiteralData (Swappable x) -> 
    //                //convert input to valid literal op2
    //                let newop2 imm = 
    //                    Map.containsKey imm allowedLiterals
    //                    |> function 
    //                        |true -> 
    //                            allowedLiterals.[imm]
    //                            |> fun d -> {K = fst d; R = snd d} |> RC |> LiteralData
    //                match a.rt with
    //                |"ADD" -> {a with rt = "SUB"; ap = {a.ap with op2 = newop2 (~~~x + 1u)}}
    //                |"SUB" -> {a with rt = "ADD"; ap = {a.ap with op2 = newop2 (~~~x + 1u)}}
    //                |"ADC" -> {a with rt = "SBC"; ap = {a.ap with op2 = newop2 (~~~x + 2u)}}
    //                |"SBC" -> {a with rt = "ADC"; ap = {a.ap with op2 = newop2 (~~~x)}}
    //                |"CMP" -> {a with rt = "CMN"; ap = {a.ap with op2 = newop2 (~~~x + 1u)}}
    //                |"CMN" -> {a with rt = "CMP"; ap = {a.ap with op2 = newop2 (~~~x + 1u)}}
    //        )

    instruction
    //|> iValidated
    |> Result.map (fun b ->
        let p = 
            match execOK with 
            |Ok x -> x 
            |Error _ -> false
        match p with
            |true ->
                match b.rt with
                |"ADD" -> engine b state (fun x y _ -> x + y) 
                |"SUB" -> engine b state (fun x y _ -> x - y) 
                |"ADC" -> engine b state (fun x y z -> x + y + (if z then 1L else 0L)) 
                |"SBC" -> engine b state (fun x y z -> x - y + (if z then 1L else 0L) - 1L)  
                |"RSB" -> engine b state (fun x y _ -> y - x) 
                |"RSC" -> engine b state (fun x y z -> y - x + (if z then 1L else 0L) - 1L)
                |"CMP" -> engine b state (fun x y _ -> x - y)
                |"CMN" -> engine b state (fun x y _ -> x + y) 
            |false -> state 
    )

////Testing parameters
let defaultFlags: Flags = {N=false;Z=false;C=false;V=false}
let defaultRegs: Map<RName,uint32> = Map.map (fun _ (s:string) -> 0x1u) regStrings
let defaultMM: MachineMemory<Instr> = [0u..4u..0xfcu] |> List.map (fun a -> WA a) |> fun b -> [DataLoc 0u] |> List.allPairs b |> Map.ofList
let (tcpuData: DataPath<Instr>) = {Fl = defaultFlags; Regs = defaultRegs; MM =  defaultMM}

{LoadAddr = WA 0u;
Label = None;
SymTab = None;
OpCode = "SUBS";
Operands = "R0,R1,R1"} 
|> parse 
|> fun a -> eval a tcpuData

