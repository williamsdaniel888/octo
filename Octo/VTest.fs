namespace VisualTest

/// top-level code demonstrating how to run tests

open CommonTop
open Arith
module VTest =

    open Expecto
    open VCommon
    open VLog
    open Visual
    open System.Threading
    open System.IO
    open Common.CommonData //Most of code access commondata
    open Common.CommonLex
    open CommonTop
    open Arith

    /// parameters setting up the testing framework
    /// WARNING: PostludeLength must be changed if Postlude is changed
    /// WARNING: global cache (CacheFileName) must be deleted if Postlude is changed
    /// Postlude can contain instructions to move CPU state (flags, memory locations) into rgeisters
    /// standard Postlude moves flags into R1
    /// Simulation reads VisUAL log and returns registers after test assembly code, and also after postlude
    let defaultParas = {
        Parallel = true              // parallel testing is now supported!
        MaxConcurrentVisualDirs = 10 // should only need the same number as of cores
        Cached = true                // true if results are stored in a cache on disk and reused to speed 
                                     // up future repeat simulations
        VisualPath =  
            @"..\visualapp\visual\"  // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir = 
            @"..\VisualWork\"        // the directory in which both temporary files and the persistent cache file are put
        CacheFileName = 
            @"..\VisualWork\Cache"   // the file name of the global cache
        CacheLimit = 10               // the number of results before adding to global cache
        InitFlags = {FN=false;FZ=false;FC=true;FV=false}
        InitRegs = [0u..10u..140u]          // initial values of registers R0..R14
        MemReadBase = 0x1000u          // locations read from memory (currently 13 consecutive words are read)
        Postlude = ""                 // this is overwritten by code
        Prelude = ""                  // this is overwritten by code
    } 
    
    
    /// run an expecto test of VisUAL
    ///
    let VisualUnitTest paras name src (flagsExpected:string) (outExpected: (Out * int) list) =
        testCase name <| fun () ->
            let flagsActual, outActual = RunVisualWithFlagsOut paras src
            Expecto.Expect.equal flagsActual (flagsExpected |> strToFlags) "Status flags don't match"
            let outRegsNoted = 
                outExpected 
                |> List.map fst
            let outActualNoted = 
                outActual.Regs 
                |> List.filter (fun (r,_) -> List.contains r outRegsNoted)
                |> List.sort
            Expecto.Expect.equal outActualNoted (outExpected |> List.sort) <|
                sprintf "Register outputs>\n%A\n<don't match expected outputs, src=%s" outActual.Regs src
            

    let VisualFrameworkTest paras =
        testCase "Framework test failed" <| fun () ->
            let parasExpected = 
                paras.InitRegs
                |> List.indexed
                |> List.map (fun (n,v) -> R n, int v)

            let flagsActual, outActual = RunVisualWithFlagsOut paras ""
            let outSorted = 
                outActual.Regs
                |> List.sort
                |> List.take 15
            Expecto.Expect.equal flagsActual  paras.InitFlags "Status flags don't match"
            Expecto.Expect.equal outSorted parasExpected <|
                sprintf "Register outputs>\n%A\n<don't match expected outputs" outActual.Regs


    type rType = {
        R0:int;R1:int;R2:int;R3:int;R4:int;R5:int;R6:int;R7:int;
        R8:int;R9:int;R10:int;R11:int;R12:int;R13:int;R14:int
    }

    let rType2List (r:rType)=
        [r.R0;r.R1;r.R2;r.R3;r.R4; r.R5;r.R6;r.R7;
         r.R8;r.R9;r.R10;r.R11;r.R12;r.R13;r.R14]
      

    let VisualFrameworkRun (regs: rType,flags:VCommon.Flags) =
        let performTest() =
            let initRegs = 
                rType2List regs
                |> List.map uint32
        
            let expectedRegs =
                initRegs
                |> List.indexed
                |> List.map (fun (n,v) -> R n, int v)

            let flagsActual, outActual = 
                    RunVisualWithFlagsOut { 
                        defaultParas with 
                            InitFlags=flags;
                            InitRegs=initRegs
                        } ""
            let actualRegs = 
                outActual.Regs
                |> List.sort
                |> List.take 15
            let flagsOK = flagsActual = flags
            let regsOK = actualRegs = expectedRegs 
            if not flagsOK then 
                printfn "Framework error: Bad flags: %A" flagsActual
                System.Console.ReadKey() |> ignore
            if not regsOK then 
                printfn "Framework error: Bad registers %A" actualRegs
                System.Console.ReadKey() |> ignore
            flagsOK && regsOK
        match flags with
        | {FN=true;FZ=true} -> true // prevent test with impossible input
        | _ -> performTest()
            
    let testParas = defaultParas
 
    let vTest = VisualUnitTest testParas

    /// to test the testbench, create many tests with assembler
    /// this is enough for each test to need being run separately
    //For each function check that output flags and registers are correctly updated, 
    type test_op = |ADD |ADC |SUB |SBC |RSB |RSC |CMP |CMN
    
    let createArgs (opcode:test_op) (set_flags:bool) (dest:string option) (op1:string) (op2:string) =
        let op =
            match opcode with
            |ADD -> "ADD"
            |ADC -> "ADC"
            |SUB -> "SUB"
            |SBC -> "SBC"
            |RSB -> "RSB"
            |RSC -> "RSC"
            |CMP -> "CMP"
            |CMN -> "CMN"
            |> fun a ->
                match set_flags with 
                |true  -> a+"S"
                |false -> a
        let rd =  
            match dest with 
                |Some x -> sprintf " %s," x 
                |None -> ""
        op + sprintf "%s %s, %s" rd op1 op2
    
    //need full namespace path due to "DP" keyword from commonLex
    let stateIn: DataPathAndMem<Arith.Arith.Instr> = 
        let flagsIn = 
            defaultParas.InitFlags 
            |> fun a -> 
                {
                Flags.N = a.FN; 
                Flags.Z = a.FZ; 
                Flags.C = a.FC;
                Flags.V = a.FV
                }
        let regsIn = 
            [0..14]
            |> List.map (fun a -> register a)
            |> List.map2 (fun a b -> b,a) defaultParas.InitRegs
            |> Map.ofList
        let mmIn: MachineMemory<Arith.Arith.Instr> = 
            defaultParas.MemReadBase
            |> fun a -> [a..4u..a+(13u*4u)] 
            |> List.map (fun a -> WA a) 
            |> fun b -> List.allPairs b [DataLoc 0u] 
            |> Map.ofList
        let DPIn:DataPath = {Fl = flagsIn; Regs = regsIn}
        {DP = DPIn; MM = mmIn}

    let testEngine (opcode:test_op) (set_flags:bool) (dest:string option) (op1:string) (op2:string)= 
        let argIn = createArgs opcode set_flags dest op1 op2
        let test_id = argIn
        let evaluatedOutput = CommonTop.parseAndExecuteLine None (WA defaultParas.MemReadBase) argIn stateIn
        let evaluatedFlagsReg = 
            evaluatedOutput
            |> Result.map (fun a ->
                let flOut =  //"1000" //get flag output from engine
                    match a.DP.Fl with
                    |{N=false;Z=false;C=false;V=false} -> "0000"
                    |{N=false;Z=false;C=false;V=true} -> "0001"
                    |{N=false;Z=false;C=true;V=false} -> "0010"
                    |{N=false;Z=false;C=true;V=true} -> "0011"
                    |{N=false;Z=true;C=false;V=false} -> "0100"
                    |{N=false;Z=true;C=false;V=true} -> "0101"
                    |{N=false;Z=true;C=true;V=false} -> "0110"
                    |{N=false;Z=true;C=true;V=true} -> "0111"
                    |{N=true;Z=false;C=false;V=false} -> "1000"
                    |{N=true;Z=false;C=false;V=true} -> "1001"
                    |{N=true;Z=false;C=true;V=false} -> "1010"
                    |{N=true;Z=false;C=true;V=true} -> "1011"
                    |{N=true;Z=true;C=false;V=false} -> "1100"
                    |{N=true;Z=true;C=false;V=true} -> "1101"
                    |{N=true;Z=true;C=true;V=false} -> "1110"
                    |{N=true;Z=true;C=true;V=true} -> "1111"
                let regOut = 
                    match opcode with
                    |CMP |CMN -> Map.find R0 a.DP.Regs |> fun a -> [R 0, int(a)] //TODO: does this need to be adapted to relevant dest or op1 register
                    |_ ->
                        match dest with
                        |Some x -> 
                            let rd = Map.find x regNames
                            let rd_number = Map.find rd regNums
                            Map.find rd a.DP.Regs |> fun a -> [R rd_number, int(a)]
                        |None -> failwithf "Destination register is missing"
                flOut,regOut
                )
        match evaluatedFlagsReg with
        |Ok a -> vTest test_id argIn (fst a) (snd a)
        |Error e -> failwithf "Instruction: %s; Error Detected: %s" argIn e

    //TODO: allow all permutations of dest,r1,r2; include Shifts
    //TODO: allow for full range of literals and negative immediates, not just [0,256]
    let DP_test_lit (m:int) (opcode:test_op) (set_flags:bool)= 
        if (opcode = RSB) || (opcode = RSC) 
            then [0..m] 
            else [-m..m]
        |> List.filter (fun a -> (Map.containsKey (uint32 (a)) allowedLiterals) || (Map.containsKey (uint32 (-a)) allowedLiterals))
        |> List.map (fun n -> 
            let n' = n //(n % 256)
            let op2 = sprintf "#%d" n'
            testEngine opcode set_flags (Some "R0") "R0" op2 
            )

    let DP_test_regs (opcode:test_op) (set_flags:bool)= 
        let a = System.Random().Next(14)
        let b = System.Random().Next(14)
        let op1 = [0..12] @ [14] |> List.map (fun n -> sprintf "R%d" n) |> fun n -> n.[a]
        let op2 = [0..12] @ [14] |> List.map (fun n -> sprintf "R%d" n) |> fun n -> n.[b]
        match opcode with
            |CMN |CMP -> 
                [testEngine opcode set_flags None op1 op2]
            |_ ->
                let c = System.Random().Next(14)
                let dest = 
                    [0..12] @ [14] 
                    |> fun n -> n.[c] 
                    |> fun n -> Some (sprintf "R%d" n)
                [testEngine opcode set_flags dest op1 op2]
    
    let runVisUALIncrTests = true

    
    // //Basic Literal Tests (must do outside [0,255])
    [<Tests>]
    let allIncrTests =
        match runVisUALIncrTests with
        | true ->
            testList "ALL DP Instructions" [
                testList "All DP Instructions Literal Tests"  [
                    testList "All ADD SubClass Instructions" [
                        testList "Literal test with ADD" (DP_test_lit 2 ADD true)
                        testList "Literal test with ADDS" (DP_test_lit 2 ADD false)
                        testList "Literal test with ADC" (DP_test_lit 2 ADC true)
                        testList "Literal test with ADCS" (DP_test_lit 2 ADC false)
                    ]
                    testList "All SUB SubClass Instructions" [
                        testList "Literal test with SUB" (DP_test_lit 2 SUB true)
                        testList "Literal test with SUBS" (DP_test_lit 2 SUB false)
                        testList "Literal test with SBC" (DP_test_lit 2 SBC true)
                        testList "Literal test with SBCS" (DP_test_lit 2 SBC false)
                    ]
                    testList "All RSB SubClass Instructions" [
                        testList "Literal test with RSB" (DP_test_lit 2 RSB true)
                        testList "Literal test with RSBS" (DP_test_lit 2 RSB false)
                        testList "Literal test with RSC" (DP_test_lit 2 RSC true)
                        testList "Literal test with RSCS" (DP_test_lit 2 RSC false)
                    ]
                ]
                //testList "All DP Instructions Register Tests" [
                //    testList "All ADD SubClass Instructions" [
                //        testList "Register test with ADD" (DP_test_regs ADD true)
                //        testList "Register test with ADDS" (DP_test_regs ADD false)
                //        testList "Register test with ADC" (DP_test_regs ADC true)
                //        testList "Register test with ADCS" (DP_test_regs ADC false)
                //    ]
                //    testList "All SUB SubClass Instructions" [
                //        testList "Register test with SUB" (DP_test_regs SUB true)
                //        testList "Register test with SUBS" (DP_test_regs SUB false)
                //        testList "Register test with SBC" (DP_test_regs SBC true)
                //        testList "Register test with SBCS" (DP_test_regs SBC false)
                //    ]
                //    testList "All RSB SubClass Instructions" [
                //        testList "Register test with RSB" (DP_test_regs RSB true)
                //        testList "Register test with RSBS" (DP_test_regs RSB false)
                //        testList "Register test with RSC" (DP_test_regs RSC true)
                //        testList "Register test with RSCS" (DP_test_regs RSC false)
                //    ]
                //    testList "All COMPARISON SubClass Instructions" [
                //        //testList "Register test with CMP" (DP_test_regs CMP false) //TODO: Fix flagsetting error
                //        //testList "Register test with CMPS" (DP_test_regs CMP true) //REDUNDANT
                //        //testList "Register test with CMN" (DP_test_regs CMN false) //TODO: Fix flagsetting error
                //        //testList "Register test with CMNS" (DP_test_regs CMN true) //REDUNDANT
                //    ]
                //]
            ]
        | false -> testList "EmptyTestList" []
    


    [<Tests>]
    /// implements random property-based tests of the framework
    /// tests that read/write of registers and flags is consistent for random
    /// input values
    let frametests =        
       let fsConfig = {
               FsCheckConfig.defaultConfig with
                   replay = Some (0,0) // seed for RNG. Means that the same tests are done each run
                                       // replace by None for a random time-based seed and therefore
                                       // new tests each time that will not cache
                   maxTest = 100       // number of random tests
               }
       testPropertyWithConfig fsConfig "Flags and registers are preserved" VisualFrameworkRun


    [<Tests>]
    let tests = 
        testList "Minimal Visual Unit Tests"
            [
            VisualFrameworkTest defaultParas
            //vTest "SUB test" "SUB R0, R0, #1" "0010" [R 0, -1]
            //vTest "SUBS test" "SUBS R0, R0, #0" "0110" [R 0, 0]
            //vTest "This ADDS test should fail" "ADDS R0, R0, #4" "0000" [R 0, 4; R 1, 0] 
            // R1 should be 10 but is specified here as 0
            ]

