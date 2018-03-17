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
        | IDP of  Arith.Instr
    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIMEM of Memory.ErrInstr
        | ERRIDP of Arith.ErrInstr
        | ERRTOPLEVEL of string
    
    

    type CondInstr = Condition * Instr

    let FlRegMem2DPAndMem (x : Result<FlRegMemRecord<'INS>,'ERR>) : Result<DataPathAndMem<'INS>, 'ERR> =
        match x with
        | Ok(x) -> Ok ({DP = {Fl=x.Fl ; Regs=x.Regs}; MM = x.MM})
        | Error(x) -> Error(x)

    let dpMemTuple2DPAndMem (x : Result<(MachineMemory<'INS> * DataPath),'ERR>) : Result<DataPathAndMem<'INS>, 'ERR> =
        match x with
        | Ok(mem, dp) -> Ok ({DP = {Fl=dp.Fl ; Regs=dp.Regs}; MM = mem})
        | Error(x) -> Error(x)
    
    //THIS IS FOR PARSING ONLY
    let IMatch (state: DataPathAndMem<'INS>) (ld: LineData) : Result<Parse<'INS>, string> option =
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | Arith.IMatch pa -> pa
        | Memory.IMatch parsedResult -> parsedResult
        | _ -> None

    ///THIS IS FOR PARSING AND EVALUATING
    let IMatch (state: DataPathAndMem<'INS>) (ld: LineData) : Result<DataPathAndMem<'INS>, string> option =
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | Arith.IMatch pa -> 
            pa
            |> fun a -> {Arith.pd = (Some a); Arith.st = {MM = state.MM; Fl = state.DP.Fl; Regs = state.DP.Regs}}
            |> Arith.eval
            |> FlRegMem2DPAndMem
            |> Some
        | Memory.IMatch parsedResult ->
            parsedResult
            |> function
               | Ok(pRes) -> 
                    (Memory.execute pRes state.MM {Fl=state.DP.Fl; Regs=state.DP.Regs})
                    |> dpMemTuple2DPAndMem
                    |> Some
               | Error(x) -> Error(x) |> Some
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
                    printfn "First OPC %s" opc
                    makeLineData opc operands 
                    |> IMatch state
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> pa
            | None, label :: opc :: operands -> 
                printfn "Label %s" label
                match { makeLineData opc operands with Label=Some label} |> IMatch state with
                | None -> 
                    Error (sprintf "BAD OPC Unimplemented instruction %s" opc)
                | Some pa -> pa
            | _ -> Error (sprintf "Unimplemented instruction %A" words)
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