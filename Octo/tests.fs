namespace Logical
module LogicalTests =
//    open Common.CommonLex
//    open Common.CommonData
//    open Logical.Logical
//    open System

//        //This function will make LineData from a given assembly instruction
//    let makeTest (assInstr:string) =
//        let makeLineData opcode operands = {
//            OpCode=opcode
//            Operands=String.concat "" operands // ["R0,"; "R0,"; "LSL"; "R5"] |> "R0,R0,LSLR5"
//            Label=None //Currently labels are assumed to be none
//            LoadAddr = WA(0u)
//            SymTab = None
//        }
//        /// remove comments from string (everything after ;)
//        let removeComment (txt:string) =
//            txt.Split(';')
//            |> function 
//                | [|x|] -> x 
//                | [||] -> "" 
//                | lineWithComment -> lineWithComment.[0]
//        /// split line on whitespace into an array
//        let splitIntoWords ( line:string ) =
//            line.Split( ([||] : char array), 
//                System.StringSplitOptions.RemoveEmptyEntries)
//        let parseString = assInstr |> removeComment |> splitIntoWords |> Array.toList
//        makeLineData parseString.[0] parseString.[1..]

//    let testLog asmInstr =
//        asmInstr |> makeTest |> parse
//    //let myLineData = {OpCode="MOV";Operands="R2,R2";Label=None;LoadAddr=WA(32u);SymTab=None};;
//    //Logical.parse myLineData;;

//    //let testInstr = "MOV R0, R0, LSL #4"

//open System
//open Expecto
//open ExpectoFsCheck
//open LogicalTests
//open Logical.Logical
//open Common.CommonData
//open Common.CommonLex

////Literals of the set of uints and registers
////Instructions of the set of instructions

//let expectoCheck result subject =
//    match subject with
//        | Some a -> Expect.equal a result  "The results should be equal" 
//        | None -> Expect.isFalse true "Unexpected return value of None"


//let makeResult root suffix condition dest o1 o2 so se =
//    match condition with
//    | None -> Error("Generic error (expecto may fail due to mismatch")
//    |Some a -> 
//        let (instc, (ro,suf,pCond)) = a
//        Ok {
//            PInstr={
//                    LoadAddr = WA(0u) //Needs checks (or write we will simply assume checked)
//                    SymTab = None 
//                    Root = root
//                    Suffix = suffix
//                    Dest = dest
//                    Op1 = o1
//                    Op2 = o2
//                    ShiftOp = so
//                    ShiftExpr = se
//                    }
//            PLabel = None
//            PSize = 4u
//            PCond = pCond
//        }    


////Unit tests that should pass with expected output go here
//[<Tests>]
//let unitParseTests =
//    //Primitive tests to determine if all opcodes are parsed correctly
//    testList "Basic tests" [
//      testCase "MOV" <| fun () ->
//        "MOV R0, R0" |> testLog |> expectoCheck (makeResult "MOV" "" (Map.tryFind "MOV" opCodesLog) (Some (Register(R0))) (Some (Register(R0))) None None None)
//      testCase "LSL" <| fun () ->
//        "LSL R2, R2, R3" |> testLog |> expectoCheck (makeResult "LSL" "" (Map.tryFind "LSL" opCodesLog) (Some (Register(R2))) (Some (Register(R2))) (Some(Register(R3))) None None)
//      testCase "LSLAL" <| fun () ->
//        "LSLAL R3, R3, #55" |> testLog |> expectoCheck (makeResult "LSL" "" (Map.tryFind "LSLAL" opCodesLog) (Some (Register(R3))) (Some (Register(R3))) (Some(Literal(55))) None None)
//      testCase "RRX" <| fun () ->
//        "RRX R2, PC" |> testLog |> expectoCheck (makeResult "RRX" "" (Map.tryFind "RRX" opCodesLog) (Some (Register(R2))) (Some (Register(R15))) None None None)
//      testCase "RRXS" <| fun () ->
//        "RRXS R2, PC" |> testLog |> expectoCheck (makeResult "RRX" "S" (Map.tryFind "RRXS" opCodesLog) (Some (Register(R2))) (Some (Register(R15))) None None None)
//      testCase "AND" <| fun () ->
//        "AND R7, R4, R1, LSL #0x22" |> testLog |> expectoCheck (makeResult "AND" "" (Map.tryFind "AND" opCodesLog) (Some (Register(R7))) (Some (Register(R4))) (Some (Register(R1))) (Some( "LSL")) (Some(Literal(int32(0x22)))))
//      testCase "LSRSAL" <| fun () ->
//        "LSRSAL R4, R1, #0b101" |> testLog |> expectoCheck (makeResult "LSR" "S" (Map.tryFind "LSR" opCodesLog) (Some (Register(R4))) (Some (Register(R1))) (Some(Literal(0b101))) None None)
//      testCase "ROR" <| fun () ->
//        "ROR LR, R8, R8" |> testLog |> expectoCheck (makeResult "ROR" "" (Map.tryFind "ROR" opCodesLog) (Some (Register(R14))) (Some (Register(R8))) (Some (Register(R8))) None None)
//      testCase "TST" <| fun () ->
//        "TST R2, R10, LSR R8" |> testLog |> expectoCheck (makeResult "TST" "" (Map.tryFind "TST" opCodesLogTst) None (Some (Register(R2))) (Some (Register(R10))) (Some("LSR")) (Some (Register(R8))))
//      testCase "TSTEQ" <| fun () ->
//        "TSTEQ R1, R0" |> testLog |> expectoCheck (makeResult "TST" "" (Map.tryFind "TSTEQ" opCodesLogTst) None (Some (Register(R1))) (Some (Register(R0))) None None)
//      testCase "MVN" <| fun () ->
//        "MVN R2, R2, ROR #0xFF" |> testLog |> expectoCheck (makeResult "MVN" "" (Map.tryFind "MVN" opCodesLog) (Some (Register(R2))) (Some (Register(R2))) None (Some("ROR")) (Some (Literal(0xFF))))
//      testCase "EOR" <| fun () ->
//        "EOR R2, R2, R9" |> testLog |> expectoCheck (makeResult "EOR" "" (Map.tryFind "EOR" opCodesLog) (Some (Register(R2))) (Some (Register(R2))) (Some (Register(R9))) None None)
//      testCase "BIC" <| fun () ->
//        "BIC R0, R2, R3" |> testLog |> expectoCheck (makeResult "BIC" "" (Map.tryFind "BIC" opCodesLog) (Some (Register(R0))) (Some (Register(R2))) (Some (Register(R3))) None None)
//      testCase "ORR" <| fun () ->
//        "ORR R2, R2, #0xFF0" |> testLog |> expectoCheck (makeResult "ORR" "" (Map.tryFind "ORR" opCodesLog) (Some (Register(R2))) (Some (Register(R2))) (Some(Literal(0xFF0))) None None)
//      testCase "ASREQ" <| fun () ->
//        "ASREQ R2, R2, #0xFF000000" |> testLog |> expectoCheck (makeResult "ASR" "" (Map.tryFind "ASREQ" opCodesLog) (Some (Register(R2))) (Some (Register(R2))) (Some (Literal(0xFF000000))) None None)    
//    ]

//let expectoFailCheck subject =
//    match subject with
//        | Some a -> Expect.isError a "This should not have parsed correctly" 
//        | None -> Expect.isFalse true "Unexpected return value of None"
        
//let expectoSuccessCheck subject = 
//    match subject with
//    | Some a -> Expect.isOk a "This should be a successful execute"
//    | None -> Expect.isFalse true "Unexpected return value of None"


////Unit Parse tests that should fail go here
//[<Tests>]
//let unitParseFailTests =
//    testList "Tests that should fail" [
//          testCase "Not enough operands" <| fun () ->
//            "MOV R0" |> testLog |> expectoFailCheck
//          testCase "Literal is too big" <| fun () ->
//            "MOV R0, R0, LSL #FF0" |> testLog |> expectoFailCheck
//          testCase "Logical shift cannot have shift" <| fun () ->
//            "LSL R0, R1, LSL #0xFF" |> testLog |> expectoFailCheck
//        ]



///////////////////////////////////EXECUTION/////////////////////////

////Create the initial datapath here
//let flags = {N=false; C=false; Z=false; V=false}
////Note that the MOV instruction can be used to Fill registers instead on an individual test basis
//let regs : Map<RName,uint32> = Map.ofList [(R0, 0u); (R1, 0u);(R2,0u);(R3,0u);(R4,0u);(R5,0u);(R6,0u);(R7,0u);(R8,0u);(R9,0u);(R10,0u);(R11,0u);(R12,0u);(R13,0u);(R14,0u);(R15,0u)]
//let testDataPath = {Fl=flags; Regs=regs}

////Note that in the below tests, parsing occurs first, although all parsed strings have been previously tested
//[<Tests>]
//let unitExecuteTests =
//    testList "Tests that should execute with required results" [
//          testCase "Basic RRX" <| fun () ->
//            Expect.isOk (execute (testLog "RRX R0, R0").Value testDataPath) "Should execute correctly"
//          testCase "S flag RRX" <| fun () ->
//            let output = (execute (testLog "RRXS R0, R0").Value testDataPath)
//            match output with
//            | Error a -> Expect.isTrue false "There was an error"
//            | Ok a ->
//                Expect.sequenceEqual [a.Fl.C; a.Fl.N; a.Fl.V; a.Fl.Z] [false;false;false;true] "Should execute correctly"
//        ]


///////////////CHAIN EXECUTION/////////////////
////An idea for testing as an alternative to property based testing is to execute several instructions head to tail
////and compare with the expected output from the Visual Emulator

/////Recursively executes instructions until we exhaust the list or Error
//let rec chainExecute (instrsRemaining : string list) (initialDataPath: Result<DataPath,DataPath>) =
//    match instrsRemaining with
//    | [] -> initialDataPath
//    | a :: b ->
//        let result = Result.bind (execute (testLog a).Value) initialDataPath
//        match result with
//        | Ok dp -> chainExecute b (Ok dp)
//        | Error dp -> Error(dp)

////Lists of instructions can be formed as string lists
//let chain1 = [  "RRX R0, R0" 
//                "RRX R0, R0" 
//                "RRX R1, R0" //All good
//             ]

//let chain2 = [
//                "MOV R0, #33"
//                "RORS R0, R0, #1"
//                "LSR R0, R0, #2"
//                "MOVS R2, R0, LSL #9"
//                "RRX R0, R3"
//                "MVNS R0, R2" //All good
//             ]

//let chain3 = [
//                "MOV R1, #0b1010101"
//                "MOV R0, #33"
//                "RORS R0, R0, #1"
//                "LSR R0, R0, #2"
//                "MOVS R2, R0, LSL #9"
//                "RRX R0, R3"
//                "MVNS R0, R2"
//                "MOV R1, #0x2200"
//                "ORRS R1,R2,R3" 
//                "MOV R3, #-&1"
//                "BIC R0,R3,R2, LSL R1"
//                "ANDS R1,R0,R2, ROR R1"
//                "MVN R1,R1, LSL #1"
//                "MVNGT R1, R1, LSR #2"
//                "ANDSEQ	R1,R0,R2, ROR #0x22"
//                "LSRSLO R2,R2,#0b11"
//                "ASRS R2, R1, #8"
//                "TST R0,R2"
//                "RRX R0,R0"
//            ]

////..and executed as shown below, with register and flag final values being checked
////.. you specify the values you wish to check for in registers and flags like (ex.):
////registers:
////Expect.sequenceEqual [a.Regs.[R0]; a.Regs.[R2]] [uint32(0xFFFFF7FF); uint32(0x800)]
////checks for R0=0xFFFFF7FF and R2=0x800
////flags:
////Expect.sequenceEqual [a.Fl.N; a.Fl.Z; a.Fl.C; a.Fl.V] [false;true;false;false]
////checks for NZCV bits set to 0100
//[<Tests>]
//let chainExecuteTest =
//    testList "Chain Execution Lists" [
//        testCase "Chain 1 Flags" <| fun () ->
//            let output = chainExecute chain1 (Ok(testDataPath))
//            match output with
//            | Error a -> Expect.isTrue false (sprintf "%A" a)
//            | Ok a ->
//                Expect.sequenceEqual [a.Fl.N; a.Fl.Z; a.Fl.C; a.Fl.V] [false;false;false;false] "Should execute correctly"
//        testCase "Chain 1 Regs" <| fun () ->
//            let output = chainExecute chain1 (Ok(testDataPath))
//            match output with
//            | Error a -> Expect.isTrue false (sprintf "%A" a)
//            | Ok a ->
//                Expect.sequenceEqual [a.Regs.[R0]] [0u] "Should execute correctly"
//        testCase "Chain 2 Flags" <| fun () ->
//            let output = chainExecute chain2 (Ok(testDataPath))
//            match output with
//            | Error a -> Expect.isTrue false (sprintf "%A" a)
//            | Ok a ->
//                Expect.sequenceEqual [a.Fl.N; a.Fl.Z; a.Fl.C; a.Fl.V] [true;false;false;false] "Should execute correctly"
//        testCase "Chain 2 Regs" <| fun () ->
//            let output = chainExecute chain2 (Ok(testDataPath))
//            match output with
//            | Error a -> Expect.isTrue false (sprintf "%A" a)
//            | Ok a ->
//                Expect.sequenceEqual [a.Regs.[R0]; a.Regs.[R2]] [uint32(0xFFFFF7FF); uint32(0x800)] "Should execute correctly"
//        testCase "Chain 3 Flags" <| fun () ->
//            let output = chainExecute chain3 (Ok(testDataPath))
//            match output with
//            | Error a -> Expect.isTrue false (sprintf "%A" a)
//            | Ok a ->
//                Expect.sequenceEqual [a.Fl.N; a.Fl.Z; a.Fl.C; a.Fl.V] [false;false;false;false] "Should execute correctly"
//        testCase "Chain 3 Regs" <| fun () ->
//            let output = chainExecute chain3 (Ok(testDataPath))
//            match output with
//            | Error a -> Expect.isTrue false (sprintf "%A" a)
//            | Ok a ->
//                Expect.sequenceEqual [a.Regs.[R0]; a.Regs.[R1]; a.Regs.[R2]; a.Regs.[R3]] [uint32(0x7FFFFBFF); uint32(0x200); uint32(0x2); uint32(0xFFFFFFFF)] "Should execute correctly"                       
//    ]

//[<EntryPoint>]
//let main args =
//    printfn "Running Program" |> ignore
//    runTestsInAssembly defaultConfig args
//    //An example output is shown below (uncomment to view)
//    //printfn "%A" (execute (testLog "RRXS R0, R0").Value testDataPath)