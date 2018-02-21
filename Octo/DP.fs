namespace Octo
module DP =
    open CommonData
    open CommonLex

    //REQUIRED TESTING RESOURCES

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
    type ErrInstr = Er of string ////Check usefulness of this
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
                        PSize = 4u; 
                        // the instruction condition is detected in the opcode and opCodeExpand                 
                        // has already calculated condition already in the opcode map.
                        // this part never changes
                        PCond = pCond 
                        }
                Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
                |> Option.map parse' // if unknown keep none, if known parse it.
            |Error x -> Some (Error x)

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse