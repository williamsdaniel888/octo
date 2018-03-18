// Learn more about F# at http://fsharp.org
namespace CommonTop

////////////////////////////////////////////////////////////////////////////////////
//      Code defined at top level after the instruction processing modules
////////////////////////////////////////////////////////////////////////////////////


open Arith.Arith
module CommonTop =

    open Common.CommonLex
    open Common.CommonData
    open Memory
    open Arith

    /// allows different modules to return different instruction types
    type Instr =
        | IMEM of Memory.Instr
        | IAR of Arith.Instr
    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIMEM of Memory.ErrInstr
        | ERRIAR of Arith.ErrInstr
        | ERRTOPLEVEL of string
    
    

    type CondInstr = Condition * Instr

    let dpMemTuple2DPAndMem (x : Result<(MachineMemory<'INS> * DataPath),'ERR>) : Result<DataPathAndMem<'INS>, 'ERR> =
        match x with
        | Ok(mem, dp) -> Ok ({DP = {Fl=dp.Fl ; Regs=dp.Regs}; MM = mem})
        | Error(x) -> Error(x)
    
    //THIS IS FOR PARSING ONLY
    let IMatch (ld: LineData) : Result<Parse<Instr>, ErrInstr> option =
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | Arith.IMatch pa -> pa |> pConv IAR ERRIAR
        | Memory.IMatch parsedResult -> parsedResult |> pConv IMEM ERRIMEM
        | _ -> None

    ///THIS IS FOR EVALUATING
    let EMatch (state: DataPathAndMem<'INS>) (ld:LineData) : Result<DataPathAndMem<Instr>, ErrInstr> option =
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | Arith.IMatch parsedData -> 
            parsedData
            |> function
               | Ok m -> 
                    Arith.eval m state 
                    |> function
                        |Ok x -> Ok x |> Some
                        |Error e -> Error (ERRIMEM e) |> Some
               | Error x -> Error (ERRIAR x) |> Some
        | Memory.IMatch parsedData ->
            parsedData
            |> function
               | Ok parsedData -> 
                    Memory.execute parsedData state.MM state.DP
                    |> dpMemTuple2DPAndMem
                    |> function
                        |Ok x -> Ok x |> Some
                        |Error e -> Error (ERRIMEM e) |> Some
               | Error e -> Error (ERRIMEM e) |> Some
        | _ -> None


    let parseAndExecuteLine (symtab: SymbolTable option) (loadAddr: WAddr) (asmLine:string) (state:DataPathAndMem<'INS>) =
        /// put parameters into a LineData record
        let makeLineData opcode operands = {
            OpCode=opcode
            Operands=String.concat "" operands
            Label=None
            LoadAddr = loadAddr
            SymTab = symtab
        }
        /// remove comments from string
        let removeComment (txt:string) =
            txt.Split(';')
            |> function 
                | [|x|] -> x 
                | [||] -> "" 
                | lineWithComment -> lineWithComment.[0]
        /// split line on whitespace into an array
        let splitIntoWords ( line:string ) =
            line.Split( ([||] : char array), 
                System.StringSplitOptions.RemoveEmptyEntries)
        /// try to parse 1st word, or 2nd word, as opcode
        /// If 2nd word is opcode 1st word must be label
        let matchLine words =
            let pNoLabel =
                match words with
                | opc :: operands -> 
                    let p = makeLineData opc operands
                    let pp = p.Label |> function |Some x -> x |None -> ""
                    printfn "LABEL %s, OPCODE %s, OPS %s" pp p.OpCode p.Operands
                    p
                    |> IMatch
                | _ -> None
            match pNoLabel, words with
            | Some _, _ -> 
                let opc::operands = words
                {makeLineData opc operands with Label = None} |> EMatch state
            | None, label :: opc :: operands -> 
                printfn "Label %s OPC %s" label opc
                match { makeLineData opc operands with Label=Some label} |> EMatch state with
                | None -> 
                    ERRTOPLEVEL (sprintf "BAD OPC Unimplemented instruction %s" opc) |> Error |> Some
                | Some pa -> Some pa
            | _ -> ERRTOPLEVEL (sprintf "Unimplemented instruction %A" words) |> Error |> Some
        asmLine
        |> removeComment
        |> splitIntoWords
        |> Array.toList
        |> matchLine

////////////////////////////////////////////////////////////////////////////////////
//      Code defined for top-level testing
////////////////////////////////////////////////////////////////////////////////////

module CommonTest =
    open Expecto
    //TODO:


////////////////////////////////////////////////////////////////////////////////////
//      Code defined to execute top-level tests
////////////////////////////////////////////////////////////////////////////////////

module CommonTestExecute =
    open Expecto

    (*OUR MAIN ENTRY POINT, TODO: MAKE THIS THE ONLY ENTRY POINT*)
    //[<EntryPoint>]
    //Tests.runTestsInAssembly Tests.defaultConfig [||] |> ignore