namespace Octo
module DP =
    open Common.CommonData
    open Common.CommonLex

    /// TYPE DEFINITIONS AND TOOLS
    type RotConstant = {K: uint32; R: int} // literal value = (K % 256) rotated right by (R &&& 0xF)*2.
    type Literal = |RC of RotConstant |Swappable of uint32
    type Reg = RName
    type SVal = NumericValue of int | RValue of Reg
    type Shift = Reg*SVal

    type Op2 = 
        | IMM12 of uint32
        | LiteralData of Literal
        | Register of Reg
        | LSL of Shift
        | ASR of Shift
        | LSR of Shift
        | ROR of Shift
        | RRX of Reg

    type RegParameters = {dest: RName option; op1: RName; op2: Op2}
    type Instr = {ic: InstrClass; rt:string; sf:string; cnd:Condition ;ap: RegParameters}
    type ErrInstr = string

    //Map of literals created by rotating for all possible values of {K,R}
    let allowedLiterals = 
        [0..2..30] 
        |> List.allPairs [0u..255u] 
        |> List.map (fun (lit,n) -> ((lit >>> n) + (lit <<< 32-n)),(lit,n)) //)|> List.sort
        |> Map.ofList
    //let bad = [0u..255u] |> List.filter (fun n -> (List.contains n allowedLiterals)<>false)
    //let p = Map.containsKey 65u allowedLiterals
    //[0..256] |> List.filter (fun a -> Map.containsKey (uint32(a)) allowedLiterals)  

    //Verify whether a uint32 is a valid immediate
    let checkOp2Literal (imm: uint32) : Result<Op2,string> = 
        Map.containsKey imm allowedLiterals
        |> function
            |true -> 
                allowedLiterals.[imm]
                |> fun a -> {K = fst a; R = snd a} |> RC |> LiteralData |> Ok
            |false -> 
                imm |> Swappable |> LiteralData |> Ok
            
    //Verify whether a register is a valid argument for op2
    let checkOp2Register (r:RName) =
        if (r.RegNum>=0 && r.RegNum<=12) || r.RegNum=14 
            then Ok r
            else Error "CO2R: Invalid register value, must be R0-R12 or R14"

    //Verify whether input is a valid imm12
    let (|Imm12'|_|) (i:uint32) = 
        if i>=0u && i<=4095u then Some i else None
    let checkImm12 (r: uint32) =
        if r>=0u && r<=4095u then Ok (IMM12 r) else Error "CI12: Invalid imm12 value, must be 0<=n<=4095"

    //TOKENIZATION

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
    
    //Active patterns for Op2 D.U. types
    let (|Reg'|_|) (str:string) = regNames.TryFind str
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

    //Tokenize a string of operands
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
           
    // ARITH INSTRUCTIONS SPECIFICATION
    let dPSpec = {
        InstrC = DP
        Roots = ["ADD";"ADC";"SUB";"SBC";"RSB";"RSC";"CMP";"CMN"]
        Suffixes = [""; "S"]
    }

    // Map of all possible opcodes recognised
    let opCodes = opCodeExpand dPSpec

    [<Struct>]
    type opValStatus = PC |SP |Strict |VStrict |IMT |BLIT |NA

    //FLEXIBLE OP2 EVALUATION TOOLS

    //Checks validity of input Shift, performs shift on register if possible
    let doShift (ro: Shift) (cpuData: DataPathAndMem<Instr>) bitOp =
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
    let makeRRX (rv:Reg) (cpuData:DataPathAndMem<Instr>) =
        let newMSB = if cpuData.Fl.C then 0x80000000u else 0x00000000u
        Map.find rv cpuData.Regs
        |> fun reg -> checkOp2Literal( newMSB + (reg>>>1))

    //Evaluates a flexible op2 value, returns uint32
    let flexOp2 (op2:Op2) (cpuData:DataPathAndMem<Instr>) = 
        match op2 with
        | IMM12 x -> x |> checkImm12
        | LiteralData (RC {K=a;R=b}) -> 
                (a >>> b) + (a <<< 32-b)
                |> checkOp2Literal
        | LiteralData (Swappable x) -> checkOp2Literal x
        | Register register -> 
            checkOp2Register register 
            |> Result.map (fun a -> Map.find a cpuData.Regs)
            |> Result.bind checkOp2Literal
        | LSL shift -> doShift shift cpuData (<<<)
        | ASR shift -> doShift shift cpuData (fun a b -> (int a) >>> b |> uint32)
        | LSR shift -> doShift shift cpuData (>>>)
        | ROR shift -> doShift shift cpuData (fun a n -> (a >>> n) ||| (a <<< (32-n)))
        | RRX r -> makeRRX r cpuData

    //PARSING AND EVALUATION

    /// parser: LineData -> Result<Parse<Instr>,ErrInstr> option
    /// A function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output --- This doesn't make sense
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parser (ls: LineData) : Result<Parse<Instr>,ErrInstr> option =
        ls.Operands
        |> tokenizer 
        |> Result.map (fun (a:Token list) ->
            a 
            |> function |[Dest de; Op1' o1; Op2' o2] -> {dest = de; op1 = o1; op2 = o2}
            )
        |> function
            |Ok ops ->
                let parse' (instrC, (root,suffix,pCond)) = 
                    let destStatus =
                        match ops.dest with
                            |Some R15 -> PC
                            |Some R13 -> SP
                            |Some _ -> Strict
                            |None -> NA
                    let op1Status =
                        match ops.op1 with
                            |R15 -> PC
                            |R13 -> SP
                            |_ -> Strict
                    let op2Status =
                        match ops.op2 with
                        |IMM12 x -> IMT
                        |LiteralData x -> VStrict
                        |Register x -> 
                            let r = checkOp2Register x
                            match r with
                            |Ok R15 -> PC
                            |Ok R13 -> SP
                            |Ok _ -> Strict
                            |_ -> NA
                        |LSL (_,NumericValue x) |LSR (_,NumericValue x) |ASR (_,NumericValue x) |ROR (_,NumericValue x)->
                            if x>=0 && x<32 
                                then 
                                    match x with 
                                    |0 |1 |2 |3 -> VStrict
                                    |_ -> Strict
                                else BLIT
                        |LSL (_,RValue R15) |LSR (_,RValue R15) |ASR (_,RValue R15) |ROR (_,RValue R15) -> NA
                        |LSL (_,RValue _) |LSR (_,RValue _) |ASR (_,RValue _) |ROR (_,RValue _) -> Strict
                        |RRX _ -> Strict
                    let pValMap = 
                        let c:Instr = {ic = instrC; rt = root; sf = suffix; cnd = pCond; ap = ops}
                        match c.rt with
                            |"CMP"|"CMN" ->
                                match destStatus,op1Status,op2Status with
                                |NA, Strict, Strict |NA, Strict, VStrict -> Ok c
                                |NA, Strict, PC -> Error "NP: Op2 can't be R15"
                                |NA, Strict, BLIT -> Error "NP: Shift by literal s requires 0<s<32"
                                |NA, Strict, NA -> Error "NP: Shift by register cannot use R15"
                                |_ -> Error "NP: Invalid operands"
                            |"ADC"|"SBC"|"RSB"|"RSC" -> 
                                match destStatus,op1Status,op2Status with
                                |Strict, Strict, Strict |Strict, Strict, VStrict -> Ok c
                                |Strict, Strict, PC -> Error "NP: Op2 can't be R15"
                                |Strict, Strict, BLIT -> Error "NP: Shift by literal s requires 0<s<32"
                                |Strict, Strict, NA -> Error "NP: Shift by register cannot use R15"
                                |_ -> Error "NP: Invalid operands"
                            |"SUB" ->
                                match destStatus,op1Status,op2Status with
                                |Strict, Strict, Strict |Strict, Strict, VStrict |Strict, SP, Strict |Strict, SP, VStrict -> Ok c
                                |Strict, Strict, PC -> Error "NP: Op2 can't be R15"
                                |Strict, Strict, BLIT -> Error "NP: Shift by literal s requires 0<s<32"
                                |Strict, Strict, NA -> Error "NP: Shift by register cannot use R15"
                                |SP, SP, VStrict ->
                                    match c.sf with
                                    |"S" -> Ok c
                                    |_ -> Error "NP: Instruction must be of form SUB{S}{Cond} SP, SP, op2, where op2 is constant or LSL by up to 3 bits"
                                |Strict, PC, IMT ->
                                    match c.sf with
                                    |"" -> Ok c
                                    |_ -> Error "NP: Instruction must be of form SUB{Cond} dest, PC, imm12, with 0<=imm12<4096"
                                |_ -> Error "NP: Invalid operands"
                            |"ADD" ->
                                match destStatus,op1Status,op2Status with
                                |Strict, Strict, Strict |Strict, Strict, VStrict |Strict, SP, Strict |Strict, SP, VStrict  -> Ok c
                                |Strict, Strict, PC -> Error "NP: Op2 can't be R15"
                                |Strict, Strict, BLIT -> Error "NP: Shift by literal s requires 0<s<32"
                                |Strict, Strict, NA -> Error "NP: Shift by register cannot use R15"
                                |SP, SP, VStrict ->
                                    match c.sf with
                                    |"S" -> Ok c
                                    |_ -> Error "NP: Instruction must be of form ADD{S}{Cond} SP, SP, op2, where op2 is constant or LSL by up to 3 bits"
                                |Strict, PC, IMT ->
                                    match c.sf with
                                    |"" -> Ok c
                                    |_ -> Error "NP: Instruction must be of form ADD{Cond} dest, PC, imm12, with 0<=imm12<4096"
                                |PC,PC,Strict ->
                                    match c.sf with
                                    |"" -> Ok c
                                    |_ -> Error "NP: Instruction must be of form ADD{Cond} PC, PC, Rm, where Rm is not PC and not SP"
                                |_ -> Error "NP: Invalid operands"
                    let (WA la) = ls.LoadAddr // memory address this instruction is loaded
                    match pValMap with
                        |Ok c ->
                            Ok {ic = instrC; rt = root; sf = suffix; cnd = pCond; ap = ops}
                            |> Result.map (fun a ->
                                { 
                                    // Normal (non-error) return from result monad
                                    // This is the instruction determined from opcode, suffix and parsing operands.
                                    PInstr = a;
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
                                )
                        |Error e -> Error e
                Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
                |> Option.map parse' // if unknown keep none, if known parse it.
            |Error x -> None

    // Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parser

    // Update CSPR flags if necessary
    let getFlags (res:int64) (root:string) (op1MSBset:bool) (bothOpsZ:bool) = 
        let negative = if (0x80000000L &&& res)<>0L then true else false
        let zero = if res &&& 0xffffffffL = 0L then true else false
        let carry =
            match root with
            |"ADD" |"ADC" |"CMN" -> 
                if res>=0x100000000L then true else false
            |"SUB" |"SBC" |"CMP" |"RSB" |"RSC" -> 
                if res>=0L then true else false
                //if (res &&& 0x8000000000000000L <> 0L) then false else true
        let overflow = 
            match root with
            |"ADD" |"ADC" |"CMN" -> 
                match op1MSBset with
                |true -> false
                |false -> 
                    if bothOpsZ
                        then false
                        else if res <= 0L then true else false
            |"SUB" |"SBC" |"CMP" ->
                match op1MSBset with
                |true -> if res >= 0L then true else false
                |false -> false
            |"RSB" |"RSC" -> 
                match op1MSBset with
                |true -> if res <= 0L then true else false
                |false -> false
        {N=negative;Z=zero;C=carry;V=overflow}

    // HOF for arithmetic functions
    let engine (args: Instr) (state: DataPathAndMem<Instr>) bitOp=
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
        let op1MSBset = op1' &&& 0x100000000L <>0L
        let root = args.rt
        let result = bitOp op1' op2' state.Fl.C
        let flags' =
            if args.sf ="S"
                then (getFlags result root op1MSBset (op1'=0L && op2'=0L))
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
            Map.toList state.MM 
                |> List.map (fun a -> 
                    if fst a = WA 0x0u then (fst a, Code args) else (fst a, snd a)
                    )
                |> Map.ofList
        {Fl = flags'; Regs = regs'; MM = mm'}

    type evalIn = {pd : Result<Parse<Instr>,ErrInstr> option; st : DataPathAndMem<Instr>}

    /// eval: evalIn -> Result<Parse<Instr>,ErrInstr>
    /// Evaluate a parsed instruction of unknown validity
    let eval (x:evalIn): Result<DataPathAndMem<Instr>, ErrInstr> =
        let instCond = 
            match x.pd with
                |None -> Error "EV: Invalid opcode"
                |Some (Error e) -> Error e
                |Some (Ok x) -> Ok (x.PInstr,x.PCond)
        let conditionMet =
            let f = x.st.Fl
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
        instCond
        |> Result.map (fun a -> fst a)
        |> Result.map (fun b ->
            let op2Status =
                match (flexOp2 b.ap.op2 x.st) with
                |Ok (LiteralData (Swappable p)) -> 
                    match b.rt with
                    |"RSB"|"RSC" -> Some (Error  "O2S: This value cannot be converted into a valid immediate")
                    |"ADD"|"SUB"|"CMP"|"CMN" -> 
                        if Map.containsKey (~~~p + 1u) allowedLiterals
                        then Some(Ok (~~~p + 1u)) else Some (Error "O2S: This value cannot be converted into a valid immediate")
                    |"ADC" -> 
                        if Map.containsKey (~~~p + 2u) allowedLiterals
                        then Some(Ok (~~~p + 2u)) else Some (Error "O2S: This value cannot be converted into a valid immediate")
                    |"SBC" ->
                        if Map.containsKey (~~~p) allowedLiterals
                        then Some(Ok (~~~p)) else Some (Error "O2S: This value cannot be converted into a valid immediate")
                    |_ -> Some (Error "O2S: Root not recognized")    
                |Ok (LiteralData (RC _)) -> None
                |_ -> Some (Error "O2S: Bad output from flexOp2")
            let newop2 imm = 
                Map.containsKey imm allowedLiterals
                |> function 
                    |true -> 
                        allowedLiterals.[imm]
                        |> fun d -> {K = fst d; R = snd d} |> RC |> LiteralData
            match b.rt with
                |"CMP"|"CMN" ->
                    let rt' = b.rt |> function |"CMP" -> "CMN" |"CMN" -> "CMP"
                    match op2Status with
                    |Some(Ok p) -> Ok {b with rt = rt'; ap = {b.ap with op2 = newop2 p}}
                    |None -> Ok b
                    |Some(Error e) -> Error e
                |"ADC"|"SBC" ->
                    let rt' = b.rt |> function |"ADC" -> "CMN" |"SBC" -> "CMP"
                    match op2Status with
                    |Some(Ok p) -> Ok {b with rt = rt'; ap = {b.ap with op2 = newop2 p}}
                    |None -> Ok b
                    |Some(Error e) -> Error e
                |"RSB"|"RSC" -> 
                    match op2Status with
                    |None -> Ok b
                    |Some(Error e) -> Error e
                |"SUB" ->
                    match op2Status with
                    |Some(Ok p) -> Ok {b with rt = "ADD"; ap = {b.ap with op2 = newop2 p}}
                    |None -> Ok b
                    |Some(Error e) -> Error e
                |"ADD" ->
                    match b.ap.dest, b.ap.op1 with
                    |Some R15, R15 -> 
                        match op2Status with
                        |Some(Ok p) -> Error "NP: If using {ADD PC,PC,Rm} Rm must be a valid positive literal"
                        |None -> Ok b
                        |Some(Error e) -> Error e
                    |_ ->
                        match op2Status with
                        |Some(Ok p) -> Ok {b with rt = "SUB"; ap = {b.ap with op2 = newop2 p}}
                        |None -> Ok b
                        |Some(Error e) -> Error e
            )
        |> Result.bind (fun b ->
            match b with
                |Ok p -> 
                    match conditionMet with
                        |Ok true ->
                            match p.rt with
                            |"ADD" -> engine p x.st (fun x y _ -> x + y) |> Ok
                            |"SUB" -> engine p x.st (fun x y _ -> x - y) |> Ok
                            |"ADC" -> engine p x.st (fun x y z -> x + y + (if z then 1L else 0L)) |> Ok
                            |"SBC" -> engine p x.st (fun x y z -> x - y + (if z then 1L else 0L) - 1L)  |> Ok
                            |"RSB" -> engine p x.st (fun x y _ -> y - x) |> Ok
                            |"RSC" -> engine p x.st (fun x y z -> y - x + (if z then 1L else 0L) - 1L) |> Ok
                            |"CMP" -> engine p x.st (fun x y _ -> x - y) |> Ok
                            |"CMN" -> engine p x.st (fun x y _ -> x + y) |> Ok
                        |_ -> 
                            let mm' = 
                                Map.toList x.st.MM 
                                |> List.map (fun a -> 
                                    if fst a = WA 0x0u then (fst a, Code p) else (fst a, snd a)
                                    )
                                |> Map.ofList
                            {x.st with MM = mm'} |> Ok
                |Error e -> Error e
        )

    //For convenience: parse and evaluate line
    let parse_eval (x:LineData) (state:DataPathAndMem<Instr>) =
        x  
        |> parser
        |> fun a -> {pd = a; st = state}
        |> eval