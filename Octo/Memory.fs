namespace Memory

/// ***************MEMORY MODULE ***************

module Memory =
    open System
    open Common.CommonData
    open Common.CommonLex
    open EEExtensions

    let debug = false

    let LitParErrRor8 = Error("LiteralParseError: Literal must be producible by rotating right an 8-bit word")
    let LitParErrInvalid = Error("LiteralParseError: Literal must be a valid signed int32 number")

    let InvalidSynComma = Error("InvalidSyntaxError: Operands must be comma seperated")

    //valid aka avail opcodes
    type OpCode =
        | OpLDR
        | OpSTR
        // add more
    
    type Suffix =
        | BSuff
        | NoneSuff

    //map each string to a valid opcode
    let OpCodeMap = [ "LDR",OpLDR ; "STR",OpSTR ] |> Map.ofList ;

    let SuffixMap = [ "B", BSuff ; "", NoneSuff ] |> Map.ofList ;
    
    /// literal value = (K % 256) rotated right by (R % 16)*2
    type Literal = {K: uint32; R: int; I: bool} // best practice, see later
    //reverse of OpCodeMap and extract strings, used for namespec
    let TStrings map = 
        map
        |> Map.toList
        |> List.map fst
        |> List.distinct
    /// sample specification for set of instructions

    type AddrScheme =
         | NORM //standard offset addressing
         | PRE  //increase R1 by offset (R1'=R1+OFFSET, then look for word in mem[r1']
         | POST //Look for word in mem[r1] then increase r1 (r1'=r1+offset)

    type OffScheme =
         | LIT | REG
    //only literals and reg are supported now, shifted types in group stage as shifts are handled by other members
    type Offset =
         | Lit of Literal
         | Reg of RName
    // change these types as required

    /// instruction (dummy: must change) MUSTTTT
    type Operand = {Op1: RName; Op2: RName; OpAddr: AddrScheme; OpOff: Offset option;}
    type Instr =  {InsOpCodeRoot: OpCode; InsOpCodeSuffix: Suffix; InsOperand: Operand; InsClass: InstrClass}

    /// parse error (dummy, but will do)
    type ErrInstr = string

    
    //this is main memspec for ldr/str only, for ldm/stm create another...
    let memSpec = {
        InstrC = MEMSINGLE
        Roots = TStrings (OpCodeMap)
        Suffixes = TStrings (SuffixMap)
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand memSpec

    /// main function to parse a line of assembler
    /// ld contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)

    type Token = | OP of string
                 | OFFSET of string //<--need to support literal data for this!!
                 | LBRA //[
                 | RBRA //]
                 | EXCL //!
                 | HASH //#
                 | COMMA //,
                 | END
                 | ERROR of string
    //type LexerState = OP1ST | OP2ST | OFFST | ENDST
    type LexData = { Txt: string; Numb: int}
    (*
        Accepted token list for opcodes 
        OP1 ; LBRA ; OP2 ; RBRA                      => ldr op1, [op2]
        OP1 ; LBRA ; OP2; OFFSET ; RBRA        => ldr op1, [op2, #offset]
        OP1 ; LBRA ; OP2; OFFSET ; RBRA ; EXCL => ldr op1, [op2, #offset]!
        OP1 ; LBRA ; OP2; RBRA; OFFSET         => ldr op1, [op2] #offset
    *)

    //helper function to initial dataPath
    let defDataPath =
        {
            Fl={
                N=false;
                C=false;
                V=false;
                Z=false;
            };
            Regs=
                regNums //returns list of all regs with their integer couterpart
                |> Map.toList
                |> List.map (fun (rname, _) -> (rname, 0u)) //initialise all to zero
                |> Map.ofList
        }

    //this is a generic empty map which can be copied to new maps with added key,values etc
    let emptyMachMemMap : MachineMemory<Instr> = Map.empty<WAddr,MemLoc<Instr>>
    
    

    let makeErrorString (errType : string) (errMsg : string) (errArg: 't option) : Result<'T,string> =
        match errArg with
        | Some(arg) ->  (String.replace "\""  "" (errType + (sprintf(@": '%A' ") arg) + errMsg))
                        |> Error
        | None -> Error(sprintf ("%s: %s") errType errMsg)


    let (|LexMatch|_|) regex state =
        //let debug = true

        match String.regexMatch regex state.Txt with
        | None -> if debug 
                  then printfn "[DEBUG] [PARSE] Match of '%s' with '%s' failed." state.Txt regex; 
                  None
        | Some (mStr, _) -> 
            let mChars = String.length mStr
            if mChars = 0 then 
                failwithf "What? Unexpected 0 character match in LexMatch '%s'" regex
            if debug then
                printfn "[DEBUG] [PARSE] Match of '%s' with '%s' OK: match is '%s" state.Txt regex mStr; 
            let state' = {state with Txt = state.Txt.[mChars..]}
            Some (mStr,state')


    /// Returns next token Option, and new state, given state lData
    /// If it returns None for token, with a changed state
    /// It will be called again with new state
    /// It must never be called with Txt=""
    let nextToken lData =
        let incr st = {st with Numb = st.Numb+1}
        let retTag tag ld = Some(tag), incr(ld)
        match lData with 
        | LexMatch "^," (_,sta) -> retTag COMMA sta
        | LexMatch @"^(?i)(R\d{1,2}|LR)" (sym, sta) -> retTag (Token.OP (sym)) sta  //make everything lowercase, capture r0-r99 | lr, pc and sp not allowed
        | LexMatch @"^\[" (_,sta) -> retTag LBRA sta
        | LexMatch @"^\]" (_, sta) -> retTag RBRA sta
        | LexMatch "^#" (_,sta) -> retTag HASH sta
        | LexMatch "^!" (_,sta) -> retTag EXCL sta
        | LexMatch @"^0x[0-9a-fA-F]+|\d+" (sym, sta) -> retTag (Token.OFFSET sym) sta
        | _ -> (None, lData)

    /// Repeatedly calls nextToken
    /// to do lexical analysis
    let tokenize str : Result<Token list, string>  =
        let rec tokenize' st : Token list =
            match st.Txt with
            | "" -> [END]
            | _ -> let nt,st' = nextToken st
                   match nt with
                   | None -> [ERROR(sprintf "LexerMatchFailed at: '%s'" st.Txt)]
                   | Some tok -> tok :: tokenize' st'
        
        tokenize' {Txt=str;Numb=0}
        |> (fun lst -> (lst, List.head (List.rev lst))) //retrieve last token, should be an error or end
        |> function
           | (_, Token.ERROR(s)) -> Error(s)
           | (lst, Token.END) -> Ok(lst)
           | (_, x) -> Error(sprintf "LexerMatchFailed with final token: %A, expected: ERROR or END" x)

    //here we use uint32 when we parse we will parse int32
    let allowedLiterals = 
        [0..2..30] 
        |> List.allPairs [0u..1u..255u] 
        |> List.map (fun (lit,n) -> (lit >>> n) ||| (lit <<< 32-n), {K=lit; R=n/2; I=false})
        |> List.collect (fun (allowedLitUint32, literal) -> [(allowedLitUint32, literal) ; (~~~(allowedLitUint32), {literal with I=true})]) //generate mvn or arm compatible valid literals
        |> Map.ofList
        //|> Map.filter (fun key _ -> (key % 4u = 0u)) //for mem offset, val must be divisible by four! ***NOTE: This is now done during exec cause strb has no prob with it
    
    let litValue {K=k ; R=r; I=i} =
        let rotVal2 u r =
            let n = (r &&& 0xF)*2 //(keep n >= 0, % would not do this)
            let res = (u % 256u >>> n) ||| (u % 256u <<< 32 - n)
            match i with
            | true -> ~~~res    //invert result because this was originally the mvn compatible value
            | false -> res
        rotVal2 k r

    let makeLiteral (lit: string) = 
        
        let tryFind(num) = match (Map.tryFind (num) allowedLiterals) with //direct conversion
                           | Some(x) -> Ok(x)
                           | None -> LitParErrRor8
        try
            Ok(uint32(int32(lit)))
        with
        | :? System.FormatException -> Error("LiteralParseError: Literal must be a valid signed int32 number")
        | :? OverflowException -> Error("LiteralParseError: Literal must be a valid signed int32 number")
        | _ -> Error("LiteralParseError: Unknown errors during parsing function")
        |> function
           | Ok(x) -> (*printfn "lit-string: %A\t\tuint32-val: %A" lit x ;*) tryFind(x) //(printfn "unsigned value is %A %A" x (tryFind(x))) ; 
           | Error(x) -> Error(x)


    //let makeOffset = 

    //first it valids a token list 
    //if the token list is in valid format
    //it trys to convert each string type token to a d.u type and end result is an operand record
    //the operand record is then combined with opcodes and other data in the parse function to make an
    //instruction record which can then be sent to an execution engine
    let makeOperands (tokListR : Result<Token list, string>) : Result<Operand, string> =
        //only call this if valid general format
        let makeOperands' op1Str op2Str (addrType) (offStr : (string * OffScheme) option) : Result<Operand, string> =
            match ((Map.tryFind op1Str regNames), (Map.tryFind op2Str regNames), addrType) with
            | (Some(op1Reg), Some(op2Reg), _) 
                    -> match (addrType, offStr) with
                       | (NORM, None) 
                            -> Ok({Op1=op1Reg; Op2=op2Reg; OpAddr=NORM; OpOff=None})
                       | (_, Some(opOff,offType)) 
                            -> match (offType) with
                               | LIT -> match makeLiteral(opOff) with
                                        | Ok(lit) -> Ok({Op1=op1Reg; Op2=op2Reg; OpAddr=addrType; OpOff=Some(Lit(lit))})
                                        | Error(x) -> Error(x)
                               | REG -> match (Map.tryFind opOff regNames) with
                                        | Some(op3) -> Ok({Op1=op1Reg; Op2=op2Reg; OpAddr=addrType; OpOff=Some(Reg(op3))})
                                        | None -> makeErrorString ("Op3RegParseError") ("is not a valid register") (Some(op1Str))
                       | (_, _) -> (makeErrorString "NoLitError" " addressing type requires a literal" (Some addrType))
            | (Some(_), None, _) -> makeErrorString ("SrcRegParseError") ("is not a valid register") (Some(op1Str))
            | (None, Some(_), _) -> makeErrorString ("DestRegParseError") ("is not a valid register") (Some(op1Str))
            | (_, _, _)-> (makeErrorString "UnknownError" "Unknown error while making operand" None)

        match tokListR with
        | Ok(tokList) 
            -> match tokList with
               | OP(op1) :: COMMA :: LBRA :: OP(op2) :: RBRA :: tail
                  -> match tail with
                     | [END] ->                                 makeOperands' op1 op2 NORM None //good no offset
                     | COMMA :: HASH :: OFFSET(off) :: [END] -> makeOperands' op1 op2 POST (Some(off, LIT)) //post-indexed
                     | COMMA :: OP(op3) :: [END] -> makeOperands' op1 op2 POST (Some(op3, REG))
                     | HASH :: _ ->                             InvalidSynComma
                     | _ ->                                     Error("InvalidSyntaxError: Incorrect order or too many args")
               | OP(op1) :: COMMA :: LBRA :: OP(op2) :: COMMA :: HASH :: OFFSET(off) :: RBRA :: tail
                  -> match tail with
                     | [END] ->                                 makeOperands' op1 op2 NORM (Some(off, LIT)) //good normal-index
                     | EXCL :: [END] ->                         makeOperands' op1 op2 PRE (Some(off, LIT)) //good
                     | _ ->                                     Error("InvalidSyntaxError: Incorrect order or too many args")
               | OP(op1) :: COMMA :: LBRA :: OP(op2) :: COMMA :: OP(op3) :: RBRA :: tail
                  -> match tail with
                     | [END] ->                                 makeOperands' op1 op2 NORM (Some(op3, REG)) //good normal-index
                     | EXCL :: [END] ->                         makeOperands' op1 op2 PRE (Some(op3, REG)) //good
                     | _ ->                                     Error("InvalidSyntaxError: Incorrect order or too many args")
               | OP(_) :: LBRA :: OP(_) :: _ ->                 InvalidSynComma
               | _ ->                                           Error("InvalidSyntaxError: Too few or too many arguments")
        | Error(x) ->                                           Error(x)



    ////// ***** main parse function of memory module ****
    let parse (ld: LineData) : Result<Parse<Instr>,string> option = 
        
        //this is the module's instruction
        let genModInstr root suff oper iClass: Instr =
            {InsOpCodeRoot=root; InsOpCodeSuffix=suff; InsOperand=oper; InsClass=iClass}

        //this is the system-wide instruction ready for execution
        let genParseInstr cond label instr : Parse<Instr> =
            {PCond=cond; PInstr=instr; PLabel=label; PSize=4u} //size is always 4u for this module

        let parse' (instrC, (root,suffix,pCond)) =
            let parse'' operands = 
                match ((Map.tryFind root OpCodeMap), (Map.tryFind suffix SuffixMap)) with
                | (Some(opC), Some(suff)) -> (genModInstr opC suff operands instrC) |> genParseInstr (pCond) (None) |> Ok
                | _ -> (makeErrorString "InvalidDataError" "The Opcode/Suffix supplied is invalid." None) //shouldn't happen usually
            // let oprTokens =
            //     tokenize (ld.Operands)

            // let operands = makeOperands(oprTokens)

            ld.Operands             |> (fun x -> (match debug with | true -> printfn "[DEBUG] [PARSE] OperandStr: %A\n" x | false -> ()) ; x)
            |> tokenize             |> (fun x -> (match debug with | true -> printfn "[DEBUG] [PARSE] Tokens: %A\n" x | false -> ()) ; x)
            |> makeOperands         |> (fun x -> (match debug with | true -> printfn "[DEBUG] [PARSE] Operands: %A\n" x | false -> ()) ; x)
            |> Result.bind(parse'') |> (fun x -> (match debug with | true -> printfn "[DEBUG] [PARSE] Parse Result: %A\n" x | false -> ()) ; x)

        Map.tryFind ld.OpCode opCodes
        |> Option.map parse'
        //if found then send the above for parsing
        //it has to be an option
        //because if it is none then the top level parser will try to look elsewhere
        //so at the very least the opcode should be the one detected by this module
        //then it can have ok results or error results 
        



    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse

    let isCondTrue (fl : Flags) (c: Condition) : bool = 
        match c with
        | Ceq -> fl.Z
        | Cne -> not fl.Z
        | Cmi -> fl.N
        | Cpl -> not fl.N
        | Chi -> fl.C && (not fl.Z)
        | Chs -> fl.C
        | Clo -> not fl.C
        | Cls -> (not fl.C) || fl.Z
        | Cge -> (fl.N = fl.V)
        | Cgt -> (not fl.Z) && (fl.N = fl.V)
        | Cle -> (fl.N <> fl.V) || fl.Z
        | Clt -> (fl.N <> fl.V)
        | Cvs -> fl.V
        | Cvc -> not fl.V
        | Cnv -> false
        | Cal -> true    

    ///main execute stage
    //takes in parsed instr, memMap, dataPath and return a tuple of updated memMap and dataPath as result
    let execute (instr: Parse<Instr>) (memMap : MachineMemory<Instr>) (dataPath : DataPath) : Result<(MachineMemory<Instr> * DataPath), string>=
        match debug with | true -> printfn "[DEBUG] [EXEC] MemMap: %A\n" memMap | false -> ()
        match debug with | true -> printfn "[DEBUG] [EXEC] dataPath: %A\n" dataPath | false -> ()

        let getRegVal (reg : RName) (dP : DataPath) : uint32 =
            match (dP.Regs.TryFind reg) with
            | Some(x) -> x
            | None -> 0u //default value if not in map
        
        let op1Reg          = instr.PInstr.InsOperand.Op1
        let opRoot          = instr.PInstr.InsOpCodeRoot
        let opSuff          = instr.PInstr.InsOpCodeSuffix
        let opAddrScheme    = instr.PInstr.InsOperand.OpAddr
        let op2Reg          = instr.PInstr.InsOperand.Op2
        let opOff           = instr.PInstr.InsOperand.OpOff
        let op2RegVal       = (getRegVal op2Reg dataPath)
                  
        //calc offset value as uint32, must convert to int32 when doing arithmetic to get correct required index i.e. WAddr
        let calcOffVal (off : Offset) (dP : DataPath) : uint32 =
            match off with
            | Lit(x) -> litValue(x)
            | Reg(x) -> (getRegVal x dP)
        

        let addValToReg (reg) (dP : DataPath) (x : uint32) =
            let newRegVal = (getRegVal reg dP) + x
            (newRegVal , {dP with Regs=dP.Regs.Add(reg, newRegVal)})
        
        //the final value of the index
        let calcWAddrVal (baseVal: uint32) (offVal: uint32) : WAddr =
            WA(baseVal + offVal)

        //extracts underlying uint32 from literal or register
        let getOffsetVal (off : Offset) =
            match off with
            | Offset.Lit(x) -> x |> litValue
            | Offset.Reg(x) -> (getRegVal x dataPath)

        //Once final WAddr is calculated, this function is called
        let exec' (dP: DataPath) (mem: MachineMemory<Instr>) (wAddr : WAddr) : Result<(MachineMemory<Instr> * DataPath), string> =
            let loadByteToReg (destReg: RName) (srcAddr: WAddr) : Result<(MachineMemory<Instr> * DataPath), string> =
                //to load byte, reverse back to nearest little endian word-addr, read that and then extract the offset byte
                let actualAddr = match srcAddr with | WA(x) -> x
                let relPos = match srcAddr with | WA(x) -> (x % 4u)
                let alignedAddr = actualAddr - relPos //shuld work relPos = 0,1,2,3

                let extractByte (offSet : int) (word : uint32) : uint32 =
                    //(printfn "Word: %A off: %A Res: %A" word offSet ( (word &&& (0xFFu <<< (8 * offSet))) >>> (8 * offSet)))
                    (word &&& (0xFFu <<< (8 * offSet))) >>> (8 * offSet)

                let makeRec (x) = Ok(mem, {dP with Regs=dP.Regs.Add(destReg, x)})

                match (mem.TryFind (WA alignedAddr)) with
                | Some(DataLoc(x)) -> (extractByte (int relPos) x) |> makeRec
                | Some(Code(_)) -> Error("ProtMemAccessError: Reading data from instruction memory space is not allowed.")
                | None -> Ok(mem, {dP with Regs=dP.Regs.Add(destReg, 0u)}) //default value if not in map
            
            let loadWordToReg (destReg: RName) (srcAddr: WAddr) : Result<(MachineMemory<Instr> * DataPath), string> =
                match srcAddr with
                | WA(x) when (x % 4u = 0u) -> match (mem.TryFind srcAddr) with
                                              | Some(DataLoc(x)) -> Ok(mem, {dP with Regs=dP.Regs.Add(destReg, x)})
                                              | Some(Code(_)) -> Error("ProtMemAccessError: Reading data from instruction memory space is not allowed.")
                                              | None -> Ok(mem, {dP with Regs=dP.Regs.Add(destReg, 0u)}) //default value if not in map
                | _ -> Error("UnalignedWordAddr: LDR/STR instructions require aligned (divisible by 4) word addresses.")

            let storeWordToMem (srcReg: RName) (destAddr: WAddr) : Result<(MachineMemory<Instr> * DataPath), string> =
                match destAddr with
                | WA(x) when (x % 4u = 0u) -> Ok(mem.Add(destAddr, DataLoc(getRegVal srcReg dP)), dP)
                | _ -> Error("UnalignedWordAddr: LDR/STR instructions require aligned (divisible by 4) word addresses.")

            let storeByteToMem (srcReg: RName) (destAddr: WAddr) : Result<(MachineMemory<Instr> * DataPath), string> =
                //to load byte, reverse back to nearest little endian word-addr, read that and then extract the offset byte
                //destAddr can be unaligned in this case

                let actualAddr  = match destAddr with | WA(x) -> x
                let addrDiff    = match destAddr with | WA(x) -> (x % 4u)   //in bytes
                let alignedAddr = actualAddr - addrDiff                     //shuld work addrDiff = 0,1,2,3

                //printfn "[DEBUG] [EXEC] ByteToMemVars: 0x%X 0x%X 0x%X\n" actualAddr addrDiff alignedAddr
                //match debug with | true -> printfn "[DEBUG] [EXEC] ByteToMemVars: %A\n" x | false -> ()
                
                let makeWordFromMask (origWord) (byteOffSet : int) (newByte) : uint32 = 
                    //origWord in mem
                    //newWord from register -- only the least significant byte is used and all others are don't care
                    //the above^ is same as VisUAL
                    //example: makeWordFromMask 0xAABBCCDD 2 0x66778899 => 0xAA77CCDD
                    let getBitMask (bytePos : int) : uint32 =
                        //bytepos should be 0,1,2,3 but "% 4" will ensure this 
                        (0xFFu <<< (8 * (bytePos % 4)))

                    let extractLSByteAndAlign (newPos : int) (word : uint32) : uint32 =
                        //extracts the least significant byte from word and aligns it to newPos, little-endian
                        //e.g. extractByte 2 0xAABBCCDD => 0xDD0000
                        ((0xFFu &&& word) <<< (8 * (newPos % 4)))

                    let extractInverseOfMask (bitMask: uint32) (word: uint32) : uint32 =
                        //this will return the original word but "0x00" for the given mask position
                        //e.g. extractInverseOfMask 0xFF00 0xAABBCCDD => 0xAABB00DD
                        (word &&& ~~~(bitMask))

                    
                    //if newByte > 0xFFu only extract the LSByte
                    (extractInverseOfMask (getBitMask byteOffSet) origWord) ||| (extractLSByteAndAlign byteOffSet newByte)


                match alignedAddr with
                | addr when (addr % 4u = 0u) -> 
                    match (mem.TryFind (WA addr)) with
                        | Some(DataLoc(memVal)) -> Ok(mem.Add(WA(addr),  DataLoc(makeWordFromMask memVal (int addrDiff) (getRegVal srcReg dP))), dP)
                        | Some(Code(_)) -> Error("ProtMemAccessError: Reading data from instruction memory space is not allowed.")
                        | None -> Ok(mem.Add(WA(addr),  DataLoc(makeWordFromMask 0u (int addrDiff) (getRegVal srcReg dP))), dP)
                | _ -> Error("UnexpectedError: Word address was expected to be aligned but it was not.") //this error should never happen
                     
            match opRoot with
            | OpLDR -> match opSuff with
                       | NoneSuff -> loadWordToReg op1Reg wAddr
                       | BSuff ->    loadByteToReg op1Reg wAddr 
            | OpSTR -> match opSuff with
                       | NoneSuff -> storeWordToMem op1Reg wAddr
                       | BSuff ->    storeByteToMem op1Reg wAddr
            |> (fun x -> (match debug with | true -> printfn "[DEBUG] [EXEC] POST EXEC VALs: %A\n" x | false -> ()) ; x)

        //main entrypoint to exec function
        match isCondTrue dataPath.Fl instr.PCond with
        | false -> Ok(memMap, dataPath) //return data with no changes as condition for exec was not met!
        | true 
            -> match opOff with
                | _ when (op1Reg = op2Reg && (opAddrScheme = PRE || opAddrScheme = POST))
                          -> Error("Source and Dest operands must be different for pre/post indexed instructions")
                | None -> (exec' dataPath memMap (calcWAddrVal op2RegVal 0u))
                | Some(x) -> match opAddrScheme with
                             | NORM -> x 
                                       |> getOffsetVal 
                                       |> calcWAddrVal op2RegVal
                                       |> exec' dataPath memMap
                             | PRE ->  x
                                       |> getOffsetVal
                                       |> (addValToReg op2Reg dataPath)
                                       |> (fun (newReg, newDP) -> exec' newDP memMap (calcWAddrVal newReg 0u))
                             | POST -> x
                                       |> getOffsetVal
                                       |> (addValToReg op2Reg dataPath)
                                       |> (fun (_, newDP) -> exec' newDP memMap (calcWAddrVal op2RegVal 0u))


/// **************Test MODULE For Memory*******


module Tests =
    open System
    open Expecto.ExpectoFsCheck
    open Expecto
    open Memory
    
    open Common.CommonData
    open Common.CommonLex
    //open Common.CommonTop

    ////helper functions

    let makeLineDataNoLabel words =
        match words with
            | opc :: operands ->
                Ok{ OpCode=opc; Operands=String.concat "" operands;Label=None; LoadAddr=WA(0u); SymTab=None}
            | _ -> Error("Error during testing, please specify opc :: operands type of string, without labels")


    /// remove comments from string
    let removeComment (txt:string) =
        txt.Split(';')
        |> function
            | [|x|] -> x //if only one element in array, no comment found so return that
            | [||] -> "" //if linehas only comma and nothing else then line is empty
            | lineWithComment -> lineWithComment.[0] //if line has comment, return LHS which will be actual code

    /// split line on whitespace into an array
    let splitIntoWords ( line:string ) =
        line.Split( ([||] : char array),
            System.StringSplitOptions.RemoveEmptyEntries)
        // line.Split( ([|' '; ','|] : char array),
        //     System.StringSplitOptions.RemoveEmptyEntries) //removes whitespace

    let parseFn (strList : string list) =
        strList
        |> makeLineDataNoLabel
        |> function
           | Ok(x) -> match parse(x) with
                      | Some(res) -> res
                      | None -> Error("The test string did not contain a supported opcode.")
           | Error(x) -> Error(x) // generally shouldn't happen

    let memQuickInstr (opRoot, opSuff,cond, op1,op2,opAddr,(opOff : Offset option))  = {
            PInstr = {
                        InsOpCodeRoot = opRoot;
                        InsOperand = {
                                        Op1 = op1;
                                        Op2 = op2;
                                        OpAddr = opAddr;
                                        OpOff = opOff;
                        };
                        InsOpCodeSuffix = opSuff;
                        InsClass = MEMSINGLE;
            };
            PLabel = None; //keep same always
            PSize = 4u;    //keep same always
            PCond = cond;
    }

    //this can be directly pipelines to parseFn
    //the parse error will flow monadically
    let execMemFn (instrResult : Result<Parse<Memory.Instr>,string>) (memMap: MachineMemory<Memory.Instr>) (dataPath: DataPath) =
        match instrResult with
        | Ok(instr) -> (execute instr memMap dataPath)
        | Error(x) -> Error(x)
    //Note: id == identity function
    //requires:
    //inputTransformFn: any-func to transform input data, use id if no func needed
    //outputTransformFn: any-func to transform output data, use id if no func needed
    //testFn: The actual function to be tested
    //testListName: Name of the testList
    //listOfIOPairs: A tuple list as [(inp0, out0) ; (inp1, out1) ; ... ] which contain raw input/output

    let parseAndExec (x , memMap, dataPath) =
        let parsedInstr = x |> splitIntoWords |> Array.toList |> parseFn
        (execMemFn parsedInstr memMap dataPath)


    let checkCond (fl, c) : bool =
        isCondTrue fl c

    //Returns compound Tests as Test
    let makeExpectoTestList inputTransformFn outputTransformFn testFn testListName listOfIOPairs =

        //single test case
        let makeOneTest (index) (input) (output) =
            testCase (sprintf "%s:%d" "SubTest" index) <| fun () ->
                Expect.equal (input |> inputTransformFn |> testFn) (output |> outputTransformFn) ""

        //index all pairs
        listOfIOPairs
        |> List.indexed
        |> List.map (fun (i, (inp, out)) -> (makeOneTest (i+1) inp out))
        |> Expecto.Tests.testList (testListName)



    [<Tests>]
    let t1 = makeExpectoTestList id id id "Sample String Identity Test" [
                ("Haaris", "Haaris")
    ]
    [<Tests>]
    let t2 = makeExpectoTestList id id (tokenize) "Operands Tokenize Tests" [
                //("R0[R1]",Ok([Token.OP1("R0"); LBRA; Token.OP2("R1"); RBRA; END]))
                ("R0", Ok([OP("R0") ; END]))  //shuld be fine at this stage not later
                ("R0,[R1]", Ok([OP("R0"); COMMA; LBRA; OP("R1"); RBRA; END]))
                ("R0,[R1,#4]", Ok([OP("R0"); COMMA; LBRA; OP("R1"); COMMA; HASH; OFFSET("4"); RBRA; END]))
                ("R0,[R1,R3]", Ok([OP("R0"); COMMA; LBRA; OP("R1"); COMMA; OP("R3"); RBRA; END]))
                ("R0[R1#4]!", Ok([OP("R0"); LBRA; OP("R1"); HASH; OFFSET("4"); RBRA; EXCL; END])) //shuld be fine at this stage not later
                ("R0[R1]#4", Ok([OP("R0"); LBRA; OP("R1"); RBRA; HASH; OFFSET("4"); END]))
                ("R0[R1]#4error", Error("LexerMatchFailed at: 'error'"))
    ]

    [<Tests>]
    let t3 = makeExpectoTestList id id (makeLiteral) "Valid Literal Tests" [
                ("-244", Ok({K=243u ; R=0; I=true}))
                ("124", Ok({K=124u ; R=0; I=false}))
                ("0xFFFFFFF0", Ok({K=240u ; R=2; I=true}))
                ("0x0FFFFFF0", Ok({K=255u ; R=2; I=true}))
                ("121", Ok({K=121u ; R=0; I=false}) ) //this should pass at this stage, exec will check if its valid
                ("0xFFF", LitParErrRor8 )
                ("4294967252", LitParErrInvalid)
    ]

    [<Tests>]
    let t4 = makeExpectoTestList makeLiteral id (Result.map(litValue)) "Valid LiteralVal (Inversion) Tests" [
                ("-44", Ok(4294967252u))
                ("44", Ok(44u))
                ("0xFFFFFFF0", Ok(0xFFFFFFF0u))
                ("0x0FFFFFF0", Ok(0x0FFFFFF0u))
                ("0xFFF", LitParErrRor8 )
                ("4294967252", LitParErrInvalid)
    ]

    [<Tests>]
    let t5 = makeExpectoTestList tokenize id (makeOperands) "Tokens Validate Tests" [
                   ("R0[R1]", InvalidSynComma)
                   ("R0", Error("InvalidSyntaxError: Too few or too many arguments"))
                   ("R0[R1#4]", InvalidSynComma)
    ]

    [<Tests>]
    let t6 = makeExpectoTestList (splitIntoWords >> Array.toList) id parseFn "Parse Tests" [
                   ("LDR R0, [R1]", Ok(memQuickInstr(OpLDR, NoneSuff, Cal, R0, R1, NORM, None)))
                   ("LDR R22, [R1]", Error("DestRegParseError: 'R22' is not a valid register"))
                   ("LDR R0, [R1, #164]!", Ok(memQuickInstr(OpLDR, NoneSuff, Cal, R0, R1, PRE, Some(Lit({K=164u ; R=0; I=false})) ) ))
                   ("LDR R0, [R1, LR]!", Ok(memQuickInstr(OpLDR, NoneSuff, Cal, R0, R1, PRE, Some(Reg(R14)) ) ))
                   ("STRB R9, [R3], R7", Ok(memQuickInstr(OpSTR, BSuff, Cal, R9, R3, POST, Some(Reg(R7)) ) ))
                   ("LDR R0, [R1, #0x0FFFFFF0]", Ok(memQuickInstr(OpLDR, NoneSuff, Cal, R0, R1, NORM, Some(Lit({K=255u ; R=2; I=true})) ) ))
    ]

    [<Tests>]
    let t7 = makeExpectoTestList id id checkCond "Cond Flag Tests" [
                   (({N=false;C=false;V=false;Z=false}, Ceq) , false)
                   (({N=false;C=false;V=false;Z=true}, Ceq) , true)
                   (({N=true;C=false;V=false;Z=false}, Cle) , true)
    ]

    [<Tests>]
    let t8 = makeExpectoTestList id id parseAndExec "Mem Tests" [
                   (("LDR R0, [R1]", emptyMachMemMap.Add(WA(0x100u), DataLoc(0x123u)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x100u)}) ,
                    Ok(emptyMachMemMap.Add(WA(0x100u), DataLoc(0x123u)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x100u).Add(R0, 0x123u)}))

                   (("LDR R0, [R1, #4]", emptyMachMemMap.Add(WA(0x104u), DataLoc(0xF000000Fu)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x100u)}) ,
                    Ok(emptyMachMemMap.Add(WA(0x104u), DataLoc(0xF000000Fu)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x100u).Add(R0, 0xF000000Fu)}))

                   (("LDR R0, [R1, #3]", emptyMachMemMap.Add(WA(0x104u), DataLoc(0xF000000Fu)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x100u)}) ,
                    Error("UnalignedWordAddr: LDR/STR instructions require aligned (divisible by 4) word addresses."))

                   (("LDR R0, [R1, #0x10]!", emptyMachMemMap.Add(WA(0x110u), DataLoc(0xFFu)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x100u)}) ,
                    Ok(emptyMachMemMap.Add(WA(0x110u), DataLoc(0xFFu)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x110u).Add(R0, 0xFFu)}))

                   (("LDR R0, [R1], #0xC", emptyMachMemMap.Add(WA(0x100u), DataLoc(0xFFu)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x100u)}) ,
                    Ok(emptyMachMemMap.Add(WA(0x100u), DataLoc(0xFFu)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x10Cu).Add(R0, 0xFFu)}))

                   (("STR R0, [R1]", emptyMachMemMap, {defDataPath with Regs=defDataPath.Regs.Add(R0, 0xF4FFu).Add(R1, 0x100u)}) ,
                    Ok(emptyMachMemMap.Add(WA(0x100u), DataLoc(0xF4FFu)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x100u).Add(R0, 0xF4FFu)}))

                   (("LDRB R0, [R1]", emptyMachMemMap.Add(WA(0x100u), DataLoc(0xF4FFu)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x101u)}) ,
                    Ok(emptyMachMemMap.Add(WA(0x100u), DataLoc(0xF4FFu)), {defDataPath with Regs=defDataPath.Regs.Add(R1, 0x101u).Add(R0, 0xF4u)}))

                   (("STRB R0, [R1, #0x2]!", emptyMachMemMap.Add(WA(0x100u), DataLoc(0xAABBCCDDu)), {defDataPath with Regs=defDataPath.Regs.Add(R0, 0x66778899u).Add(R1, 0x100u)}) ,
                    Ok(emptyMachMemMap.Add(WA(0x100u), DataLoc(0xAA99CCDDu)), {defDataPath with Regs=defDataPath.Regs.Add(R0, 0x66778899u).Add(R1, 0x102u)}))
    ]


