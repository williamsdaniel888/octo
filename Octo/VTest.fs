﻿namespace VisualTest

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


    type RType = {
        R0:int;R1:int;R2:int;R3:int;R4:int;R5:int;R6:int;R7:int;
        R8:int;R9:int;R10:int;R11:int;R12:int;R13:int;R14:int
    }

    let rType2List (r:RType)=
        [r.R0;r.R1;r.R2;r.R3;r.R4; r.R5;r.R6;r.R7;
         r.R8;r.R9;r.R10;r.R11;r.R12;r.R13;r.R14]
      

    let VisualFrameworkRun (regs: RType,flags:VCommon.Flags) =
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
    type TestOpCode = |ADD |ADC |SUB |SBC |RSB |RSC |CMP |CMN
    
    let createArgs (opcode:TestOpCode) (set_flags:bool) (dest:string option) (op1:string) (op2:string) =
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
    
    //the unit input allows us to maintain generic types
    let stateIn () = 
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
            |> List.map (register) //no need for lambda it is implied
            |> List.map2 (fun a b -> b,a) defaultParas.InitRegs
            |> Map.ofList
        let mmIn: MachineMemory<'INS> = 
            defaultParas.MemReadBase
            |> fun a -> [a..4u..a+(13u*4u)] 
            |> List.map (WA) 
            |> fun b -> List.allPairs b [DataLoc 0u] 
            |> Map.ofList
        {DP = {Fl = flagsIn; Regs = regsIn;}; MM =  mmIn}

    let flag2BinStr (fl : Flags) : string =
        //Order => N ; Z ; C ; V
        [fl.N ; fl.Z ; fl.C ; fl.V]
        |> List.map (function
            | true -> "1"
            | false -> "0")
        |> List.reduce (+)
        //sprintf "%d %d" (System.Convert.ToUInt16 fl.N) (System.Convert.ToUInt16 fl.N)

    let testEngine (opcode:TestOpCode) (set_flags:bool) (dest:string option) (op1:string) (op2:string)= 
        let argIn = createArgs opcode set_flags dest op1 op2
        let testId = argIn
        printfn "%s" testId
        let evaluatedOutput = CommonTop.parseAndExecuteLine None (WA defaultParas.MemReadBase) argIn (stateIn ())
        let evaluatedFlagsReg = 
            evaluatedOutput
            |> Result.map (fun a ->
                let flOut =  //get flag output from engine
                    (flag2BinStr a.DP.Fl)
                let regOut = 
                    let rd =
                        match opcode with
                        | CMP |CMN -> Map.find op1 regNames
                        | _ ->
                            match dest with
                            | Some x -> Map.find x regNames
                            | None -> failwithf "Destination register is missing"
                    let rdNumber = Map.find rd regNums
                    Map.find rd a.DP.Regs |> fun a -> [R rdNumber, int(a)]
                flOut,regOut
                )
        match evaluatedFlagsReg with
        | Ok a -> vTest testId argIn (fst a) (snd a)
        | Error e -> failwithf "Instruction: %s; Error Detected: %s" argIn e

    //TODO: allow all permutations of dest,r1,r2 - DONE
    //TODO: include Shifts
    //TODO: allow for full range of literals and negative immediates, not just [0,256] - DONE
    //let getRandElemFromList (lst : 'a list) : 'a = (List.item (System.Random().Next lst.Length) lst) //now defined in commonlex!

    let dpTestLit (m:int) (opcode:TestOpCode) (set_flags:bool)= 
        let allowedRIntLst = [0..12] @ [14]
        let op1 = regStrings.[inverseRegNums.[getRandElemFromList allowedRIntLst]]
        if (opcode = RSB) || (opcode = RSC) 
            then [(1<<<8)..(1<<<8)..(m<<<8)] 
            else [((-m)<<<8)..(1<<<8)..(m<<<8)]
        |> List.filter (fun a -> (Map.containsKey (uint32 (a)) allowedLiterals) || (Map.containsKey (uint32 (-a)) allowedLiterals))    
        //Map.toList allowedLiterals |> List.map (fst) |> List.map (int)
        |> List.map (fun n -> 
            let n' = n //(n % 256)
            let op2 = sprintf "#%d" n'
            match opcode with
            |CMN |CMP -> testEngine opcode set_flags None op1 op2
            |_ -> 
                let dest = regStrings.[inverseRegNums.[getRandElemFromList allowedRIntLst]]
                testEngine opcode set_flags (Some dest) op1 op2 
            )


    let dpTestRegs (opcode:TestOpCode) (set_flags:bool)= 
        let allowedRIntLst = [0..12] @ [14]
        let op1 = ["R8"]//[regStrings.[inverseRegNums.[getRandElemFromList allowedRIntLst]]]
        //let op2 = [regStrings.[inverseRegNums.[getRandElemFromList allowedRIntLst]]]
        //let op3 = List.map (fun a -> sprintf "R%d" a) [0..3]
        let flop2 = [sprintf "R12, RRX"] //List.allPairs op2 op3 |> List.map (fun a -> sprintf "%s, LSL %s" (fst a) (snd a)) //regStrings.[inverseRegNums.[getRandElemFromList allowedRIntLst]] 
        //RRX
        //ADC R2, R8, R14, RRX
        //ADDS R2, R8, R14, RRX
        //SUBS R2, R14, R14, RRX
        //RSCS R2, R5, R5, RRX
        //RSBS R2, R10, R7, RRX
        //SBCS R2, R7, R7, RRX
        //SUBS R2, R8, R8, RRX
        //LSL
        //RSBS R2, R3, R3, LSL R3
        //SBCS R2, R9, R7, LSL R3

        List.allPairs op1 flop2
        |> List.map (fun pp ->
            match opcode with
                | CMN | CMP -> 
                    testEngine opcode set_flags None (fst pp) (snd pp)
                    //[testEngine opcode set_flags None op1 flop2]
                | _ ->
                    let dest = Some "R2"
                        //getRandElemFromList allowedRIntLst
                        //|> fun n -> regStrings.[inverseRegNums.[n]]
                        //|> Some
                    //[testEngine opcode set_flags dest op1 flop2]
                    testEngine opcode set_flags dest (fst pp) (snd pp)
        )
    let runVisUALIncrTests = true
    
    [<Tests>]
    let allIncrTests =
        match runVisUALIncrTests with
        | true ->
            testList "ALL DP Instructions" [
                //testList "All DP Instructions Literal Tests"  [
                //    testList "All ADD SubClass Instructions" [
                //        testList "Literal test with ADDS" (dpTestLit 10 ADD true)
                //        testList "Literal test with ADD" (dpTestLit 10 ADD false)
                //        testList "Literal test with ADCS" (dpTestLit 10 ADC true)
                //        testList "Literal test with ADC" (dpTestLit 10 ADC false)
                //    ]
                //    testList "All SUB SubClass Instructions" [
                //        testList "Literal test with SUBS" (dpTestLit 10 SUB true)
                //        testList "Literal test with SUB" (dpTestLit 10 SUB false)
                //        testList "Literal test with SBCS" (dpTestLit 10 SBC true)
                //        testList "Literal test with SBC" (dpTestLit 10 SBC false)
                //    ]
                //    testList "All RSB SubClass Instructions" [
                //        testList "Literal test with RSBS" (dpTestLit 10 RSB true)
                //        testList "Literal test with RSB" (dpTestLit 10 RSB false)
                //        testList "Literal test with RSCS" (dpTestLit 10 RSC true)
                //        testList "Literal test with RSC" (dpTestLit 10 RSC false)
                //    ]
                //    testList "All COMPARISON SubClass Instructions" [
                //        testList "Literal test with CMP" (dpTestLit 10 CMP false)
                //        //testList "Literal test with CMPS" (dpTestLit CMP true) //REDUNDANT
                //        testList "Literal test with CMN" (dpTestLit 10 CMN false)
                //        //testList "Literal test with CMNS" (dpTestLit CMN true) //REDUNDANT
                //    ]
                //]
                testList "All DP Instructions Register Tests" [
                    testList "All ADD SubClass Instructions" [
                        testList "Register test with ADDS" (dpTestRegs ADD true)
                        testList "Register test with ADD" (dpTestRegs ADD false)
                        testList "Register test with ADCS" (dpTestRegs ADC true)
                        testList "Register test with ADC" (dpTestRegs ADC false)
                    ]
                    testList "All SUB SubClass Instructions" [
                        testList "Register test with SUBS" (dpTestRegs SUB true)
                        testList "Register test with SUB" (dpTestRegs SUB false)
                        testList "Register test with SBCS" (dpTestRegs SBC true)
                        testList "Register test with SBC" (dpTestRegs SBC false)
                    ]
                    testList "All RSB SubClass Instructions" [
                        testList "Register test with RSBS" (dpTestRegs RSB true)
                        testList "Register test with RSB" (dpTestRegs RSB false)
                        testList "Register test with RSCS" (dpTestRegs RSC true)
                        testList "Register test with RSC" (dpTestRegs RSC false)
                    ]
                    //testList "All COMPARISON SubClass Instructions" [
                    //    testList "Register test with CMP" (dpTestRegs CMP false)
                    //    //testList "Register test with CMPS" (dpTestRegs CMP true) //REDUNDANT
                    //    testList "Register test with CMN" (dpTestRegs CMN false)
                    //    //testList "Register test with CMNS" (dpTestRegs CMN true) //REDUNDANT
                    //]
                ]
            ]
        | false -> testList "EmptyTestList" []
    


    //[<Tests>]
    ///// implements random property-based tests of the framework
    ///// tests that read/write of registers and flags is consistent for random
    ///// input values
    //let frametests =        
    //   let fsConfig = {
    //           FsCheckConfig.defaultConfig with
    //               replay = Some (0,0) // seed for RNG. Means that the same tests are done each run
    //                                   // replace by None for a random time-based seed and therefore
    //                                   // new tests each time that will not cache
    //               maxTest = 100       // number of random tests
    //           }
    //   testPropertyWithConfig fsConfig "Flags and registers are preserved" VisualFrameworkRun


    //[<Tests>]
    //let tests = 
    //    testList "Minimal Visual Unit Tests"
    //        [
    //            VisualFrameworkTest defaultParas
    //            vTest "SUB test" "SUB R0, R0, #1" "0010" [R 0, -1]
    //            vTest "SUBS test" "SUBS R0, R0, #0" "0110" [R 0, 0]
    //            //vTest "This ADDS test should fail" "ADDS R0, R0, #4" "0000" [R 0, 4; R 1, 0] 
    //            // R1 should be 10 but is specified here as 0
    //        ]

