﻿namespace VisualTest

/// top-level code demonstrating how to run tests

open CommonTop
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
    open Octo.DP

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
    
    let op2Test_lit (m:int) (p:test_op) (set_flags:bool)= 
        [0..m]
        |> List.filter (fun a -> Map.containsKey (uint32 (a)) allowedLiterals)
        |> List.map (fun n -> 
            let n' = (n % 256) //1+(n%254)
            let op =
                match p with
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
            let test_id = op + sprintf "%d test" n'
            let argIn = op + sprintf " R0, R0, #%d" n'

            //need full namespace path due to "DP" keyword from commonLex
            let stateIn: DataPathAndMem<Octo.DP.Instr> = 
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
                let mmIn: MachineMemory<Octo.DP.Instr> = 
                    defaultParas.MemReadBase
                    |> fun a -> [a..4u..a+(13u*4u)] 
                    |> List.map (fun a -> WA a) 
                    |> fun b -> List.allPairs b [DataLoc 0u] 
                    |> Map.ofList
                {Fl = flagsIn; Regs = regsIn; MM =  mmIn}
            let simOut = CommonTop.parseAndExecuteLine None (WA defaultParas.MemReadBase) argIn stateIn
            let f_r_Out = 
                simOut
                |> Result.map (fun a ->
                    let flOut =  //"1000" //get flag output from engine
                        match a.Fl with
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
                    let regOut = Map.find R0 a.Regs |> fun a -> [R 0, int(a)] //[R 0, -n'] //get register output from engine
                    flOut,regOut
                    )
            match f_r_Out with
            |Ok a -> vTest test_id argIn (fst a) (snd a)
            |Error e -> failwithf "Error detected with literal input %d: %s" n' e
            )

    let op2Test_reg (p:test_op) (set_flags:bool)= 
        [0..12] @ [14]
        |> List.map (fun n -> 
            let op =
                match p with
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
            let test_id = op + sprintf " R%d test" n
            let argIn = op + sprintf " R0, R0, R%d" n
            let stateIn: DataPathAndMem<Octo.DP.Instr> = 
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
                let mmIn: MachineMemory<Octo.DP.Instr> = 
                    defaultParas.MemReadBase
                    |> fun a -> [a..4u..a+(13u*4u)] 
                    |> List.map (fun a -> WA a) 
                    |> fun b -> List.allPairs b [DataLoc 0u] 
                    |> Map.ofList
                {Fl = flagsIn; Regs = regsIn; MM =  mmIn}
            let simOut = CommonTop.parseAndExecuteLine None (WA defaultParas.MemReadBase) argIn stateIn
            let f_r_Out = 
                simOut
                |> Result.map (fun a ->
                    let flOut =  //"1000" //get flag output from engine
                        match a.Fl with
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
                    let regOut = Map.find R0 a.Regs |> fun a -> [R 0, int(a)] //[R 0, -n'] //get register output from engine
                    flOut,regOut
                    )
            match f_r_Out with
            |Ok a -> vTest test_id argIn (fst a) (snd a)
            |Error e -> failwithf "Error detected: %s" e
            )
    
    let runVisUALIncrTests = false

    
    // //Basic Literal Tests (must do outside [0,255])
    [<Tests>]
    let allIncrTests =
        match runVisUALIncrTests with
        | true ->
            testList "ALL DP Instructions" [
                testList "All DP Instructions Literal Tests"  [
                    testList "All ADD SubClass Instructions" [
                        testList "Literal test with ADD" (op2Test_lit 255 ADD true)
                        testList "Literal test with ADDS" (op2Test_lit 255 ADD false)
                        testList "Literal test with ADC" (op2Test_lit 255 ADC true)
                        testList "Literal test with ADCS" (op2Test_lit 255 ADC false)
                    ]
                    testList "All SUB SubClass Instructions" [
                        testList "Literal test with SUB" (op2Test_lit 255 SUB true)
                        testList "Literal test with SUBS" (op2Test_lit 255 SUB false)
                        testList "Literal test with SBC" (op2Test_lit 255 SBC true)
                        testList "Literal test with SBCS" (op2Test_lit 255 SBC false)
                    ]
                    testList "All RSB SubClass Instructions" [
                        testList "Literal test with RSB" (op2Test_lit 255 RSB true)
                        testList "Literal test with RSBS" (op2Test_lit 255 RSB false)
                        testList "Literal test with RSC" (op2Test_lit 255 RSC true)
                        testList "Literal test with RSCS" (op2Test_lit 255 RSC false)
                    ]
                ]
                testList "All DP Instructions Register Tests" [
                    testList "All ADD SubClass Instructions" [
                        testList "Register test with ADD" (op2Test_reg ADD true)
                        testList "Register test with ADDS" (op2Test_reg ADD false)
                        testList "Register test with ADC" (op2Test_reg ADC true)
                        testList "Register test with ADCS" (op2Test_lit 255 ADC false)
                    ]
                    testList "All SUB SubClass Instructions" [
                        testList "Register test with SUB" (op2Test_reg SUB true)
                        testList "Register test with SUBS" (op2Test_reg SUB false)
                        testList "Register test with SBC" (op2Test_reg SBC true)
                        testList "Register test with SBCS" (op2Test_lit 255 SBC false)
                    ]
                    testList "All RSB SubClass Instructions" [
                        testList "Register test with RSB" (op2Test_reg RSB true)
                        testList "Register test with RSBS" (op2Test_reg RSB false)
                        testList "Register test with RSC" (op2Test_reg RSC true)
                        testList "Register test with RSCS" (op2Test_lit 255 RSC false)
                    ]
                ]
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
            vTest "SUB test" "SUB R0, R0, #1" "0010" [R 0, -1]
            vTest "SUBS test" "SUBS R0, R0, #0" "0110" [R 0, 0]
            //vTest "This ADDS test should fail" "ADDS R0, R0, #4" "0000" [R 0, 4; R 1, 0] 
            // R1 should be 10 but is specified here as 0
            ]

