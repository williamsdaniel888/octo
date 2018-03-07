namespace VisualTest
/// common types and low-level functions
/// provided for VisualTest Testbench
module VCommon =

    open System
    open System.Threading

    /// ARM Flags output from Visual run using postlude code via a register
    type Flags = {
        FN: bool
        FZ: bool
        FC: bool
        FV: bool
    }
   
    type TestSetup = { Asm: string }

    /// register name used for visual output
    type Out = 
       | R of int


    /// configuration data for running VisUAL
    /// 'D is DataPath type - used to make initial Datapth value equal to
    /// the initial machine state set up for VisUAL
    /// 'R is type of register names
    type Params = {
        Parallel: bool // if true run tests in parallel.
        MaxConcurrentVisualDirs: int // should only need same number as of cores
        Cached: bool  // If true use a cache to deliver instant results on repeat simulations
        VisualPath: string // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir: string // the directory in which both temporary files and the persistent cache file are put
        CacheFileName: string // The global cache name
        CacheLimit: int      // the max number of worker cache entries before the global cache is updated
        /// instructions to control inputs and outputs
        InitRegs: uint32 list
        InitFlags: Flags
        MemReadBase: uint32 //Read locations MemReadBase...MemReadBase+14 into registers R0
        /// temporary data used to construct assembler
        Prelude: string
        Postlude: string

    }

   

    /// additional info extracted from Visual outputs by postlude.
    type VisState = {
        VFlags: Flags
        VMemData: uint32 list
        }

    /// info output by Visual
    type VisOutput = {
        Regs: (Out * int) list // registers after provided assembler
        RegsAfterPostlude: (Out * int) list // for processing postlude info
        State: VisState
        }

    type CacheT = { Dat: Map<string, ((Out*int) list * (Out*int) list)>; Limit: int}

    let mutable gLock: Semaphore array = [||]
    let mutable cLock: Semaphore = null
    let mutable dirUsedHint: bool array = [||]
    let mutable workerCache:  CacheT array = [||]
    let mutable GlobalCache: CacheT = { Dat = Map.empty ; Limit = 0}





