// Learn more about F# at http://fsharp.org
namespace CommonTop

////////////////////////////////////////////////////////////////////////////////////
//      Code defined at top level after the instruction processing modules
////////////////////////////////////////////////////////////////////////////////////


module CommonTop =

    open Common.CommonLex
    open Common.CommonData
    open Memory
    open Octo

    /// allows different modules to return different instruction types
    type Instr =
        | IMEM of Memory.Instr
    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIMEM of Memory.ErrInstr
        | ERRTOPLEVEL of string
    
    

    type CondInstr = Condition * Instr
    
    //main IMATCH fn and parseAndEval fn
    (* *)
    (* THIS ONLY WORKS FOR DP ATM*)
    let IMatch (state:DataPathAndMem<DP.Instr>) (ld: LineData) : Result<DataPathAndMem<DP.Instr>,DP.ErrInstr> option =
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | DP.IMatch pa -> 
            pa
            |> fun a -> {DP.pd = (Some a); DP.st = state}
            |> DP.eval
            |> Some
        | _ -> None

    let parseAndExecuteLine (symtab: SymbolTable option) (loadAddr: WAddr) (asmLine:string) (state:DataPathAndMem<DP.Instr>) =
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
                    makeLineData opc operands 
                    |> IMatch state
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> pa
            | None, label :: opc :: operands -> 
                match { makeLineData opc operands with Label=Some label} |> IMatch state with
                | None -> 
                    Error (sprintf "Unimplemented instruction %s" opc)
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