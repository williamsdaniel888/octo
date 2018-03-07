namespace VisualTest

module VData =

    open VCommon
    /// Generates assembler to set Reg n to value u.
    let SETREG (n:int) (u:uint32) =
        let setRegByte n b (u:uint32) =
            sprintf "MOV R%d, #0x%x\r\n" n (u <<< b*8)

        let addRegByte n b u =
            sprintf "ADD R%d, R%d, #0x%x\r\n" n n (u <<< b*8)

        if n < 0 || n >= 16 then 
            failwithf "Can't set register %d to %x because %d is out of range 0..15" n u n
        [0..3]
        |> List.collect (function | b when u &&& (0xFFu <<< b*8) <> 0u -> [(n , b,  (u >>> b*8) &&& 0xFFu)] | _ -> []
                                    | _ -> [])
        |> function
            | [] -> setRegByte n 0 0u
            | (n,b,u) :: rest -> 
                setRegByte n b u + (
                    rest 
                    |> List.map (fun (n,b,u) -> addRegByte n b u)
                    |> String.concat ""
                )

    /// Generates assembler which sets flags to value given by fl.        
    /// Uses R0 as temporary register
    let SETFLAGS (fl: Flags) = 

        let initNZ (fl:Flags) =     
            match fl.FN, fl.FZ with
            | false, false -> "MOVS R0, #1\r\n"
            | false, true -> "MOVS R0, #0\r\n"
            | true, false -> "MOVS R0, #-1\r\n"
            | true, true -> failwithf "What? This is an impossible flag combination: N=1, Z=1"

        let initCV (fl:Flags) =
            (match fl.FC, fl.FV with
            | true, true ->  "MOV R0, #0x80000000\r\n"
            | true, false -> "MOV R0, #0xFFFFFFFF\r\n"
            | false, true -> "MOV R0, #0x7FFFFFFF\r\n"
            | false, false -> "MOV R0, #0\r\n") +
            "ADDS R0, R0, R0\r\n"
            

        initCV fl + initNZ fl

    /// Generates assembler which
    /// loads memory location mAddr into register reg
    let LOADLOC (reg:int) (mAddr:uint32) =
        if mAddr < 0x100u then 
            failwithf "LOADMEM called with code section address: %x" mAddr
        SETREG reg mAddr + (sprintf "LDR R%d, [R%d]\r\n" reg mAddr)

    /// Generates assembler to store mDat in memory location mAddr
    /// Uses R0 and R2 as temporary registers
    let STORELOC (mDat:uint32) (mAddr: uint32) =
        // uses R2 and R3
        SETREG 0 mDat +
        SETREG 2 mAddr +
        "STR R0, [R2]\r\n"
    
    /// Generates assembler which Sets registers R0-R14
    /// from the supplied list of values
    /// RegVals must be of length 15
    let SETALLREGS (regVals: uint32 list) =
        if regVals.Length <> 15 then
            failwithf "Error in SETALLREGS. regVals = %A is not length 15." regVals
        regVals
        |> List.indexed
        |> List.map (fun (n,v) -> SETREG n v)
        |> String.concat ""
    
    /// Read NZCV into bits 3210 of R0
    let READFLAGSINTOR0 =
    // use R0 as for output
           """MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
"""
    /// Read memBase..memBase+13 into R1..R14
    /// uses R13 as temporary register
    let READMEMORY (memBase:uint32) =
        SETREG 13 memBase +
        "LDMIA R13, {R0-R12}\n"

    /// Construct postlude assembly code
    /// memBase: base addr of memory locs read by postlude
    let POSTLUDE memBase =
        READMEMORY memBase + READFLAGSINTOR0


    /// Construct wrapper code for simulation
    /// regs, flags: init values of regs, flags
    ///    regs must be length 15 sets R0..R15
    /// memBase: location of block of memory words to read
    /// asm: assembly code to test
    /// returns (n, maincode, postlude)
    /// n is length of postlude
    let GETWRAPPER regs flags memBase =
        let main =
            SETFLAGS flags +
            SETALLREGS regs +
            "\r\n"
        let post = POSTLUDE memBase
        main, post
            
    /// processes after-postlude registers to extract additional state info
    /// as determined by postlude. E.g. flags from the standard postlude
    let decodeStateFromRegs outsAfter =
        /// Rn (n > 0) represents mem locations
        let memLocs = 
            outsAfter
            |> List.filter (fun (R n,_) -> n >= 0 && n < 13)
            |> List.map (fun (R _, v) -> uint32 v)
        let flagsInt = List.find (function | (R 0, _) -> true | _ -> false) outsAfter |> snd
        let flagBool n = (flagsInt &&& (1 <<< n)) > 0
        let flags =
            { 
                FN = flagBool 3
                FZ = flagBool 2
                FC = flagBool 1
                FV = flagBool 0
            }
        { VFlags = flags ; VMemData = memLocs}
            

   


