namespace Octo
module DP =
    open CommonData
    open CommonLex

    //Flexible Op2 type definitions and tools
    type RotConstant = {K: uint32; R: int} // literal value = (K % 256) rotated right by (R &&& 0xF)*2.
    type Literal = |RC of RotConstant
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
    type ErrInstr = string 

    //Map of literals created by rotating for all possible values of {K,R}
    let allowedLiterals = 
        [0..2..30] 
        |> List.allPairs [0u..255u] 
        |> List.map (fun (lit,n) -> ((lit >>> n) + (lit <<< 32-n)),(lit,n))
        |> Map.ofList

    //verifies whether a uint32 is a valid immediate
    let checkOp2Literal (imm: uint32) = 
        Map.containsKey imm allowedLiterals
        |> function
            |true -> 
                allowedLiterals.[imm]
                |> fun a -> {K = fst a; R = snd a} |> RC |> LiteralData |> Ok
            |false -> Error "CO2L: Invalid literal, must be of format N ROR 2M, 0u<=N<=255u, 0<=M<=15"
                
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

    //Tokenizer type definitions, active patterns and tools
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
            
    // Specification for ARITH set of instructions and parser

    let dPSpec = {
        InstrC = DP
        Roots = ["ADD";"ADC";"SUB";"SBC";"RSB";"RSC";"CMP";"CMN"]
        Suffixes = [""; "S"]
    }

    //Map of all possible opcodes recognised
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
                    let (WA la) = ls.LoadAddr
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

    //ARITH tools and evaluation
    //Called iff conditions permit evaluation
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

    //HOF for performing arithmetic functions
    let engine (args: Instr) (state: DataPath<Instr>) bitOp=
        //Extract dest, op1, op2 registers
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
                |Error _-> failwithf "Bad arguments"
        let addness = if args.rt = "ADD" || args.rt = "ADC" || args.rt = "CMN" then true else false
        //perform operation
        let result = bitOp op1' op2' state.Fl.C
        //update registers, CSPR and memory
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
            Map.toList state.MM 
                |> List.map (fun a -> 
                    if fst a = WA 0x0u then (fst a, Code args) else (fst a, snd a)
                    )
                |> Map.ofList
        {Fl = flags'; Regs = regs'; MM = mm'}
    
    //Evaluate parsed data, return the machine's state
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
                    |Ok _ -> "strict"
                    |Error _ -> "n/a"
                a1
            )
        let execOK =
            instCond
            |> Result.map (fun a -> fst a)
            |> Result.map (fun b -> b.rt,b.sf)
            |> Result.map (fun c ->
                match fst c with
                |"CMP"|"CMN" ->
                    match destStatus,op1Status,op2Status with
                    |Ok "n/a", Ok "strict", Ok ("strict") -> true
                    |_ -> false
                |"ADC"|"SBC" -> 
                    match destStatus,op1Status,op2Status with
                    |Ok "strict", Ok "strict", Ok ("strict") -> true
                    |_ -> false
                |"RSB"|"RSC" -> 
                    match destStatus,op1Status,op2Status with
                    |Ok "strict", Ok "strict", Ok ("strict") -> true
                    |_ -> false
                |"SUB" ->
                    match destStatus,op1Status,op2Status with
                    |Ok "strict", Ok "strict", Ok ("strict") -> true
                    |Ok "strict", Ok "SP", Ok ("strict") -> true
                    |Ok "SP", Ok "SP", Ok ("v.strict") -> true
                    |Ok "strict", Ok "PC", Ok ("imm12") -> 
                        if snd c = "" then true else false
                    |_ -> false
                |"ADD" ->
                    match destStatus,op1Status,op2Status with
                    |Ok "strict", Ok "strict", Ok ("strict") -> true
                    |Ok "strict", Ok "SP", Ok ("strict") -> true
                    |Ok "SP", Ok "SP", Ok ("v.strict") -> true
                    |Ok "PC", Ok "PC", Ok ("strict") -> 
                        if snd c = "" then true else false
                    |Ok "strict", Ok "PC", Ok ("imm12") -> 
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

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse