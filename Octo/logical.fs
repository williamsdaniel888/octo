namespace Logical

module Logical =
    open Common.CommonData
    open Common.CommonLex
    open System
    open System.Text.RegularExpressions

    //TODO: Read the instruction coming in and use a lex / active pattern to match it with an item
    //in the logSpec or logTstSpec - this will give us the constituent parts of the instruction including
    //the root, suffix and condition
    //make a function that will... (what was I going to say?)
    //TODO: Increment the PC (R15 whenever an instruction is executed)

  
    //This type is used for values which can either be a register or a literal
    type Operand =
        | Register of RName
        | Literal of int32
        | OpError of string

    //Represents a generic instruction minus the condition for execution
    type Instr = {
        LoadAddr: WAddr
        //Label: string option
        SymTab: SymbolTable option
        //Class: InstrClass
        Root: string
        Suffix: string
        Dest: Operand option //every operand apart from op1 is optional
        Op1: Operand option //but option for op1 in case of error
        Op2: Operand option
        ShiftOp: string option
        ShiftExpr: Operand option
    }

    type ErrInstr = string

    type ErrRun = string

    let logSpec = {
        InstrC = LOG
        Roots = ["MOV";"MVN";"EOR";"AND";"ORR";"BIC";"LSL";"LSR";"ASR";"ROR";"RRX"]
        Suffixes = [""; "S"]
    }

    //https://intranet.ee.ic.ac.uk/t.clarke/hlp/projectdetails.html#H101
    //https://intranet.ee.ic.ac.uk/t.clarke/hlp/projectdetails.html#H10

    let logTstSpec = {
        InstrC = LOGtst
        Roots = ["TST";"TEQ"]
        Suffixes = [""] //These have conditions only
    }

    //Full Maps of possible opcodes
    let opCodesLog = opCodeExpand logSpec
    let opCodesLogTst = opCodeExpand logTstSpec
    
    ///This DU is used to discriminate instructions based on the
    ///format that their operands take. This is used to pick up on
    ///errors during parsing.
    type operandType =
        | RRX //op1 op2 (RRX)
        | TEST //op1 op2 shift (TST, TEQ)
        | MOV //dest op1 shift (MOV, MVN)
        | SHFT //dest op1 op2 (LSL, LSR, ASR, ROR)
        | BIT //dest op1 op2 shift (AND, EOR, BIC, ORR)

    ///A list of allowed shift operand codes
    let shiftList = ["LSL";"LSR";"ASR";"ROR";"RRX"]

    let condCodes = [ "EQ"; "NE" ; "MI" ; "PL" ; "HI" ; "HS" ; "LO" ; 
                    "LS" ; "GE" ; "GT" ; "LE" ; "LT" ; "VS" ; 
                    "VC" ; "NV" ; "AL" ; ""]

    let genCompleteSuffixes (opList: string list) : string list =
        List.collect (fun a -> List.collect (fun b -> List.map (fun c -> a + b + c) condCodes) [""; "S"]) opList

    //String arrays used to determine which instructions correspond
    //to particular types
    let RRXset = ["RRX"] |> genCompleteSuffixes |> List.map (fun a -> (a, RRX))
    let TESTset = ["TST";"TEQ"] |> List.collect (fun a -> List.map (fun b -> a + b) condCodes) |> List.map (fun a -> (a, TEST))
    let MOVset = ["MOV";"MVN"] |> genCompleteSuffixes |> List.map (fun a -> (a, MOV))
    let SHFTset = ["LSL";"LSR";"ASR";"ROR"] |> genCompleteSuffixes |> List.map (fun a -> (a, SHFT))
    let BITset = ["AND";"EOR";"BIC";"ORR"] |> genCompleteSuffixes |> List.map (fun a -> (a, BIT))
    ///Access the operandType of a suffixless opcode by using operandMap.["opcode"]
    ///e.g. operandMap.["AND"] = BIT
    let operandMap = RRXset @ TESTset @ MOVset @ SHFTset @ BITset
                    |> Map.ofList
    ///Function shortcut for finding the operand type of a particular suffixless opcode
    ///This is a safe method for accessing using Option type
    let findOperandType (opCode:string) = 
        if (Map.containsKey opCode operandMap) then Some operandMap.[opCode] else None     

    let parse (ls: LineData) : Result<Parse<Instr>,string> option =

        ///Checks an int32 is valid as an immediate literal
        ///by checking it is in the set of allowed literals
        let checkIntIsValid (number: int32) : bool =
            //check it belongs to the set of all no.s producible by rotating 32 bit no. by multiplying by 2,4,6..8,16
            let acceptableRotates = [int32(0)..int32(15)] |> List.map (fun a -> 2*a)
            let acceptableBaseBits = [int32(0)..int32(511)]
            let acceptableInts = List.collect (fun a -> (List.map (fun b -> a <<< b) acceptableRotates)) acceptableBaseBits
                                |> List.collect (fun a -> [a; ~~~a])
            List.contains number acceptableInts

        ///This will attempt to parse as an Operand Literal type a string supplied
        let literalParse (literalString: string) : Operand =
            //Custom function similar to Option.Bind for Operands
            let returnResult (operand: Operand option) =
                match operand with
                | Some(a) -> a
                | None -> OpError("Error parsing literal")
            let removeHash (str: string) =
                try
                    if (str.Split([|"#"|], StringSplitOptions.None).[0] = "") then //Ensure that we do not parse e.g. LSL #3 as the literal 3
                        Some (str.Split([|"#"|], StringSplitOptions.None).[1])
                    else
                        None
                with
                | e -> None
            let checkTypeAndConvertToUint (str: string) =
                try
                    //TODO: Lookup uint.Parse as a possible alternative to manual parsing
                    // uint32.Parse (hexString, System.Globalization.NumberStyles.AllowHexSpecifier)
                    // let prefixSuffix = str.Split([|"0x"; "&"; "0b"|], StringSplitOptions.None) |> List.ofArray
                    str.Replace("&", "0x") |> int32 |> Some
                with
                | e -> None

            let checkValidInt (value: int32) =
                //If rotatable by 8 (32 bit word) then return Literal(uint) no error
                //else return OpError
                if (checkIntIsValid (value)) then Some (Literal(value)) else None

            Some literalString
            |> Option.bind removeHash
            |> Option.bind checkTypeAndConvertToUint
            |> Option.bind checkValidInt
            |> returnResult

        //This check has the dual responsibility of checking for the
        //existence of the opcode and asserting the length
        //of the operands list
        let operands = ls.Operands.Split([|","|], StringSplitOptions.None) |> Array.toList
        let checkOperands =
            let operandType = findOperandType ls.OpCode
            match operandType with
                | Some RRX when (operands.Length = 2) -> Some RRX
                | Some TEST when (operands.Length = 3 || operands.Length = 2) -> Some TEST //GEQ means that the operand is optional
                | Some MOV when (operands.Length = 3 || operands.Length = 2) -> Some MOV
                | Some SHFT when (operands.Length = 3) -> Some SHFT
                | Some BIT when (operands.Length = 4 || operands.Length = 3) -> Some BIT
                | None -> None
                | _ -> None //Redundant
        
        ///Will attempt to convert a generic operand to a specific register or literal,
        ///or else fail with an error
        let genOperand (opString: string) : Operand =
            if Map.containsKey opString regNames then Register(regNames.[opString])
            else
                opString |> literalParse

        //Returns an OpError as the second parameter in the case of error
        ///This function will attempt to parse a shift using a supplied string
        ///list of operands as well as a position which indicates the index of
        ///the operand opcode
        let tryShift (position: int) (operands: string list)=
            ///Parses the shift string
            let genShift (shift: string) =
                try
                    let shiftOp = shift.[0..2] //Take first three letters
                    if List.contains shiftOp shiftList then 
                        if shiftOp = "RRX" then
                            if shift.Length = 3 then (Some("RRX"), None)
                            else
                            (None, Some (OpError("RRX Does not take arguments")))
                        else
                            (Some shiftOp, Some (genOperand shift.[3..]))                    
                    else
                        (None, Some (OpError("Invalid shift operand")))
                with //Catches string indexing out of bound exceptions
                | e -> (None, Some (OpError("Could not parse shift")))
            try //See if there is a shift operator or not
                //Then check that we are dealing with a register as the value to be shifted, otherwise reject
                match (genShift operands.[position]) with
                | ((Some(a)), (Some(b))) -> if (Map.containsKey (operands.[position - 1]) regNames) then (genShift operands.[position]) else (None, (Some (OpError("Only registers can be shifted"))))
                | ((Some(c)), None) -> if Map.containsKey (operands.[position - 1]) regNames then (genShift operands.[position]) else (None, Some (OpError("Only registers can be shifted")))
                | _ -> genShift operands.[position]
            with
            | e -> (None, None) //No (optional) shift operator supplied (acceptable)

        //If it passes this point then opcodes exist and correct no. of options
        if checkOperands = None then Some(Error("Operands were invalid"))
        else

        ///Allows you to throw an error if there are operands where there shouldn't be (i.e. extra operands supplied)
        ///The index is a location which should be empty for the string list
        let assertNoValues (operands:string list) (index:int) : string option * Operand option =
            if (operands.Length <= (index))
            then (None, None) else (None, Some(OpError("Too many operands supplied"))) //Throws an error if the string has too many values

        //Checks that the literal isn't bigger than 0xFF for shift and rotate instuctions
        let maxFF operand =
            match operand with
            | Register r -> Some operand
            | Literal l -> if (l>0xFF) then Some(OpError("Literal is too big")) else Some(operand)
            | OpError o -> Some operand

        //PARSING STRUCTURE
        //Set the operands according to the format determined
        //from the type of instruction checkOperands
        //in the case of a parse error for an operand, the operand will have
        //type OpError
        let (dest, op1, op2, (shiftOp, shiftExpr)) =
            match checkOperands with
            | Some MOV ->
                (Some (genOperand operands.[0]), Some (genOperand operands.[1]), None, tryShift 2 operands)
            | Some SHFT ->
                (Some (genOperand operands.[0]), Some (genOperand operands.[1]), maxFF ((genOperand operands.[2])), (assertNoValues operands 3))
            | Some BIT ->
                (Some (genOperand operands.[0]), Some (genOperand operands.[1]), Some (genOperand operands.[2]), tryShift 3 operands)
            | Some RRX ->
                (Some (genOperand operands.[0]), Some (genOperand operands.[1]), None, (assertNoValues operands 2))
            | Some TEST ->
                (None, Some (genOperand operands.[0]), Some (genOperand operands.[1]), tryShift 2 operands)
            | None -> (Some(OpError("Did not find opcode")), None, None, (None, None)) //Should never occur anyway due to prior type checking       

        //ASSERT VALIDITY
        //Check validity of operands if they are literals (Return an error)
        match (dest, op1, op2, shiftExpr) with
            | (Some(OpError(a)), _, _,_)
            | ( _,Some(OpError(a)), _,_)
            | ( _, _,Some(OpError(a)),_)
            | ( _, _,_,Some(OpError(a))) -> Some(Error(a))
            | _ ->

                //No instructions in the addressed set have special sizes at the moment
                let size = 4u

                ///Forms the final instruction from the supplied data
                let parse' (instrC, (root,suffix,pCond)) =
                    match (op2, shiftOp, shiftExpr) with
                    | ((Some (Literal a)), Some b, _) -> Error("Literals cannot be shifted")
                    //| (_, _, Some(Literal(a))) when ((a > 0xFF) || (a < 0)) -> Error("Literal immediate value shift must be <= 0xFF")
                    | _ ->
                        Ok {
                            PInstr={
                                    LoadAddr = ls.LoadAddr //Needs checks (or write we will simply assume checked)
                                    SymTab = ls.SymTab //Needs checks
                                    Root = root
                                    Suffix = suffix
                                    Dest = dest
                                    Op1 = op1
                                    Op2 = op2
                                    ShiftOp = shiftOp
                                    ShiftExpr = shiftExpr
                                    }
                            PLabel = if (ls.SymTab=None || ls.Label=None) then None else Some (ls.Label.Value,ls.SymTab.Value.[ls.Label.Value])
                            PSize = size
                            PCond = pCond
                            }
                
                ///A meta-function to return the result
                let ErrorIfNone a =
                    match a with
                    | Some(Ok a) -> Some(Ok a)
                    | Some(Error(a)) -> Some(Error(a))
                    | None -> Some(Error("Error while parsing opcode"))

                //FINAL PARSE
                //Return the result if all was successful
                Map.tryFind ls.OpCode opCodesLog
                |> (fun a -> if a=None then (Map.tryFind ls.OpCode opCodesLogTst) else a) //Try to find the code in opCodesLogTst too
                |> Option.map parse' //Handles errors in finding opcode
                |> ErrorIfNone

    let (|IMatch|_|) = parse
    
    let execute (ins: Result<Parse<Instr>, string>) (dp: DataPath)  =
        match ins with 
        | Error _ -> Error(dp) //Return the original datapath if the instruction is invalid
                                //wrapping in an error to indicate the failure to perform an instruction
        | Ok instruction ->
            //READ DATAPATH
            let flags = dp.Fl
            let regs = dp.Regs
            let (n,c,z,v) = (flags.N,flags.Z,flags.C,flags.V)

            //BITMASKS
            let MSB = int32(0x1 <<< 31) //1 is 31st position
            ///

            //Returns whether or not the instruction should be executed
            let conditionalExecute =
                let (n,z,c,v) = (flags.N,flags.Z,flags.C,flags.V)
                match instruction.PCond with
                | Ceq -> z
                | Cne -> not z
                | Cmi -> n
                | Cpl -> not n
                | Chi -> c && (not z)
                | Chs -> c
                | Clo -> (not c)
                | Cls -> (not c) || z
                | Cge -> n=v
                | Cgt -> (not z) && (n=v)
                | Cle -> z || (not n=v)
                | Clt -> not (n=v)
                | Cvs -> v
                | Cvc -> not v
                | Cnv -> false
                | Cal -> true
            
            //We convert to int32 for ease of interpretation and then rewrite as uint32
            let getRegValue reg =
                int32(regs.[(reg)])

            let getLitValue (myVal: Operand option) =
                match myVal with
                | Some(Register a) -> getRegValue a
                | Some(Literal b) -> b
                | _ -> 0 //Should never happen

            let getRotate (value: int32) (amountUnHandled: int32) (isReg: bool) : bool * int32 =
                //A rotate left is the same as a rotate right by (32 - abs(negative value))
                let amount =
                    if isReg then int32(0xFFu &&& uint32(amountUnHandled)) else amountUnHandled
                let abs x = if (x<0) then -x else x
                let direction = (amount>=0) //true if right rotate
                let amountModuloPos = (abs(amount) % 32)
                if amountModuloPos=0 then (false,value) else //Do not attempt if the value is 0
                if direction then //Right rotate
                    if (amount = 0) then
                        (false, value)
                    else
                        let result = int32((uint32(value) <<< (32-amountModuloPos)) + (uint32(value) >>> (amountModuloPos)))
                        ((result<0), result) //If negative then last rotated bit was a 1 (rotating right)
                else            //Left rotate
                    let result = int32((uint32(value) <<< (amountModuloPos)) + (uint32(value) >>> (32-amountModuloPos)))
                    ((result<0), result) //Never sets the carry bit if rotating using negative number

            ///Deals with the case where the shift is 32 bits long (checks MSB or LSB depending on shift direction)
            let dealWith32 (value:int32) (direction: bool) : bool * int32 =
                if (direction && (value<0)) then
                    (true, int32(0))
                else if ((not direction) && ((1 &&& int32(value))=1)) then
                    (true, int32(0))
                else
                    (false, 0)

            ///True means right and false means left
            let getShift (value: int32) (amountUnHandled: int32) (direction: bool) (isReg: bool) : bool * int32 =
                let amount = //If it is a register take only the first 8 bits
                    if isReg then int32(0xFFu &&& uint32(amountUnHandled)) else amountUnHandled
                if amount=0 then (false, value)
                else if amount>32 then (false, 0) //If greater than 32 then the result is 0
                else if amount=32 then
                    dealWith32 value direction
                else
                    if (amount > 0) then
                        if direction then //Right shift
                            ((1 &&& (int32(uint32(value) >>> (amount-1))))=1 , int32(uint32(value) >>> amount))
                        else            //Left shift
                            ((MSB &&& int32((uint32(value) <<< (amount-1))))=MSB, int32(uint32(value) <<< amount))                                                          
                    else
                        (false, 0)

            //////////SHIFTS (WHICH ARE THE ONLY THINGS TO SET CARRY BITS IN THIS SET)///////////
            //Note that these functions can be used for the direct implementation of shifts
            //The below should produce a carry flag (tentative) and a value as output
            let shiftrrx (reg:Operand option) (myVal:Operand option) : bool * int32 = 
                //We make 1 the MSB (if carry flag is set)
                ( (1 &&& (getLitValue reg))=1, ((getLitValue reg) >>> 1) +  (if c then MSB else 0))

            ///Returns true if the operand is a register (as opposed to a literal)
            let isReg reg =
                    match reg with
                    | Some(Register _) -> true
                    | _ -> false
                                            
            let shiftror (reg:Operand option) (myVal:Operand option) : bool * int32 =
            //Carry flag is set if the negative flag is set
                getRotate (getLitValue reg) (getLitValue myVal) (isReg myVal)

            //Logical shift left
            let shiftlsl (reg:Operand option) (myVal:Operand option) : bool * int32 =
                getShift (getLitValue reg) (getLitValue myVal) false (isReg myVal)
            
            //Logical shift right
            let shiftlsr (reg:Operand option) (myVal:Operand option) : bool * int32 =
                getShift (getLitValue reg) (getLitValue myVal) true (isReg myVal)
            
            //Arithmetic shift right
            let shiftasr (reg:Operand option) (myVal:Operand option) : bool * int32 =
                let (carry: bool, output: int32) = getShift (getLitValue reg) (getLitValue myVal) true (isReg myVal)
                (carry, (if ((MSB &&& (getLitValue reg))=1) then (output + MSB) else output))
            
            //Represents lack of a shift (used to cover non-shifting instructions)
            let shiftnop (reg:Operand option) (myVal:Operand option) : bool * int32 = 
                (false, getLitValue (if instruction.PInstr.Op2=None then instruction.PInstr.Op1 else instruction.PInstr.Op2))
            ///////////////////////////////////////////////////////////////////////
            let resultOfShift =
                match instruction.PInstr.ShiftOp with
                | Some "RRX" -> shiftrrx (if instruction.PInstr.Op2=None then instruction.PInstr.Op1 else instruction.PInstr.Op2) instruction.PInstr.ShiftExpr
                | Some "ROR" -> shiftror (if instruction.PInstr.Op2=None then instruction.PInstr.Op1 else instruction.PInstr.Op2) instruction.PInstr.ShiftExpr
                | Some "LSL" -> shiftlsl (if instruction.PInstr.Op2=None then instruction.PInstr.Op1 else instruction.PInstr.Op2) instruction.PInstr.ShiftExpr
                | Some "LSR" -> shiftlsr (if instruction.PInstr.Op2=None then instruction.PInstr.Op1 else instruction.PInstr.Op2) instruction.PInstr.ShiftExpr
                | Some "ASR" -> shiftasr (if instruction.PInstr.Op2=None then instruction.PInstr.Op1 else instruction.PInstr.Op2) instruction.PInstr.ShiftExpr
                | None -> shiftnop None None //Simply return the last parameter without doing anything if no shift/rotate

            ///INSTRUCTIONS///

            ///RRX
            let rrx (carry:bool, resultOfShift:int32) : (bool * int32) option = 
                //We make 1 the MSB (if carry flag is set)
                Some ( (1 &&& resultOfShift)=1, (int32(((uint32(resultOfShift))>>> 1)) ) + (if c then MSB else 0))

            ///ROR  
            let ror (carry:bool, resultOfShift:int32) : (bool * int32) option =
                //Carry flag is set if the negative flag is set
                Some ( getRotate (getLitValue (instruction.PInstr.Op1)) (resultOfShift) (isReg instruction.PInstr.Op2))

            ///Logical shift left
            let lslins (carry:bool, resultOfShift:int32) : (bool * int32) option =
                Some ( getShift (getLitValue (instruction.PInstr.Op1)) (resultOfShift) false (isReg instruction.PInstr.Op2))
            
            ///Logical shift right
            let lsrins (carry:bool, resultOfShift:int32) : (bool * int32) option =
                Some ( getShift (getLitValue (instruction.PInstr.Op1)) (resultOfShift) true (isReg instruction.PInstr.Op2))
            
            ///Arithmetic shift right
            let asrins (carry:bool, resultOfShift:int32) : (bool * int32) option =
                let (carry: bool, output: int32) = getShift (getLitValue(instruction.PInstr.Op1)) (resultOfShift) true (isReg instruction.PInstr.Op2)
                Some (carry, (if ((MSB &&& resultOfShift)=1) then (output + MSB) else output))

            ///MOV
            let mov (carry: bool, resultOfShift:int32) : (bool * int32) option =
                Some ((carry, resultOfShift))

            ///MOVN
            let mvn (carry: bool, resultOfShift:int32) : (bool * int32) option =
                Some ((carry, ~~~resultOfShift))

            ///TST
            let tst (carry: bool, resultOfShift:int32) : (bool * int32) option =
                Some(carry, int32(uint32(getLitValue(instruction.PInstr.Op1)) &&& uint32(resultOfShift)))

            ///TEQ
            let teq (carry: bool, resultOfShift:int32) : (bool * int32) option =
                Some(carry, int32(uint32(getLitValue(instruction.PInstr.Op1)) ^^^ uint32(resultOfShift)))

            ///ORR
            let orr (carry: bool, resultOfShift:int32) : (bool * int32) option =
                Some(carry, int32(uint32(getLitValue(instruction.PInstr.Op1)) ||| uint32(resultOfShift)))

            ///BIC
            let bic (carry: bool, resultOfShift:int32) : (bool * int32) option =
                Some(carry, int32(uint32(getLitValue(instruction.PInstr.Op1)) &&& (~~~uint32(resultOfShift))))

            ///Selects a function based on the opcode supplied in the instruction
            let selectFunc =
                match instruction.PInstr.Root with
                | "TST" -> tst
                | "TEQ" -> teq
                | "MOV" -> mov
                | "MVN" -> mvn
                | "RRX" -> rrx
                | "LSL" -> lslins //ins suffix because these are reserved infix operators
                | "LSR" -> lsrins
                | "ASR" -> asrins
                | "ROR" -> ror
                | "AND" -> tst //implemented the same for operands (only difference is result is stored)
                | "EOR" -> teq //ditto
                | "BIC" -> bic
                | "ORR" -> orr
                | _ -> (fun a -> None) //defaultFunction which returns None

            ///Sets the flags, conditionally
            let setFlags (carry:bool) (result:int32) : Flags =
                if (instruction.PInstr.Suffix = "S") then ({N=(result<0); Z=(result=0); C=carry; V=v}) else flags
                //None of the instructions changes v 

            ///Outputs the final DataPath result
            let writeRegsFlags (results: (bool * int32) option) : Result<DataPath, DataPath> option =
                match results with
                | None -> None
                | Some (carry, result) ->
                    let outFlags = setFlags carry result //Set the flags
                    match instruction.PInstr.Dest with
                    | None -> Some(Ok ({Fl=outFlags; Regs=regs})) //Output same registers, amended flags
                    | Some(Register(outputRegister)) ->
                        let uintResult = uint32(result)
                        let regs' = Map.add outputRegister uintResult regs
                        Some(Ok({Fl=outFlags; Regs=regs'}))

            ///Meta-function for output
            let OptionError input =
                match input with
                | Some(a) -> a
                | None -> Error(dp) //Return datapath with an error               

            //Execute based on the conditions
            if conditionalExecute then //TODO: Also, only write to the flags if S is set.
                Some resultOfShift
                |> Option.bind selectFunc //Input is a (carry * resultOfShift)
                |> writeRegsFlags //Input is a (carry, finalResult) which we are able to use setFlags and write to appr register
                //Output is a datapath of the combined flagArray and amended register
                |> OptionError
                //Should return an error if the value is None
            else
            Ok(dp) //Return the datapath unchanged if no execution is to be performed