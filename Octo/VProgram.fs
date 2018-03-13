namespace VisualTest
module VProgram =

    open Expecto
    open VCommon
    open VLog
    open Visual
    open VTest
    open System.Threading
    open System.IO



    /// configuration for this testing framework      
    /// configuration for expecto. Note that by default tests will be run in parallel
    /// this is set by the fields oif testParas above
    let expectoConfig = { Expecto.Tests.defaultConfig with 
                            parallel = testParas.Parallel
                            parallelWorkers = 2 // try increasing this if CPU use is less than 100%
                    }

    [<EntryPoint>]
    let main _ = 
        initCaches testParas
        let rc = runTestsInAssembly expectoConfig [||]
        finaliseCaches testParas

        //TODO: MUST DELETE THIS CODE AND INCORPORATE ALL TESTING IN VTESTS
        //THIS IS JUST PROOF THAT MULTIPLE INSTRUCTION CLASSES ARE WORKING
        // let dP = {Memory.Memory.defDataPath with Regs=Memory.Memory.defDataPath.Regs.Add(Common.CommonData.R1, 0x100u)}
        // let mem = Memory.Memory.emptyMachMemMap.Add(Common.CommonData.WA(0x100u), Common.CommonData.DataLoc(0x123u))
        // let str = "LDR R0, [R1]"

        // CommonTop.CommonTop.parseAndExecuteLine None (Common.CommonData.WA defaultParas.MemReadBase) str {DP=dP; MM=mem}
        // |> printfn "result of common execution:\n\n %A"
        
        System.Console.ReadKey() |> ignore                
        rc // return an integer exit code - 0 if all tests pass
