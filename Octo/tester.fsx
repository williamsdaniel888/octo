    //#load "CommonData.fs"
    //#load "CommonLex.fs"
    //#load "DP.fs"
    ////#load "Memory.fs"
    ////#load "CommonTop.fs"

    //open Octo
    //open CommonData
    //open CommonLex
    //open DP
    ////open Memory
    ////open CommonTop

    //// Resources
    //let defaultFlags: Flags = {N=false;Z=false;C=false;V=false}
    //let defaultRegs: Map<RName,uint32> = Map.map (fun _ (s:string) -> uint32(1)) regStrings
    //let defaultMM: MachineMemory<Instr> = [0u..4u..48u] |> List.map (fun a -> WA a) |> fun b -> [DataLoc 0u] |> List.allPairs b |> Map.ofList
    //let (tcpuData: DataPath<Instr>) = {Fl = defaultFlags; Regs = defaultRegs; MM =  defaultMM}
    //let tInstr = {LoadAddr = WA 0u;
    //Label = None;
    //SymTab = None;
    //OpCode = "RSBS";
    //Operands = "R1,R2,R3"}
    ////parse_eval tInstr tcpuData