namespace VisualTest

/// Code to read/write structured data from Visual log file (XML) and Cache files
/// This does not use existing F# serialisers or type providers (that would do this nicely)
/// because that would introduce dependencies which at current state of F# ecosystem
/// reduce portability.
/// As a result, the code here is a one-off hack
/// no effort has been put into making this code general or maintainable

module VLog =

    open VCommon
    open VData
    open System
    open System.IO

    let sysNewline = System.Environment.NewLine

    /// used to parse log files using regex strings (primitive)
    let regexMatch (regex:string) (str:string) =
        let m = Text.RegularExpressions.Regex(regex).Match(str)
        if m.Success
        then
            let mLst = [ for x in m.Groups -> x.Value ]
            Some (List.head mLst, List.tail mLst)
        else None

    /// To do regex matching via pattern
    let (|MATCH|_|) = regexMatch

    
    /// detect register output from Visual log line
    let rgxReg = """<register name="R([([0-9]+)">(0x[0-9A-F]+)</register>"""

    /// Detect syntax error from Visual log line
    let rgxSyntaxError = """<syntax-error([^<]*)</syntax-error>"""

    /// detect runtime error from visual log line
    let rgxRuntimeError = """<runtime-error([^<]*)</runtime-error>"""

    /// Read an entire VisUAL log parsing it.
    /// Todo: reimplement using sequences and array reader for higher efficiency
    /// The cache size could be very large
    let readVisualLog n vLog =        
        let matches = 
            vLog
            |> Array.rev
            |> Array.collect (function
                | MATCH rgxSyntaxError (_, [e]) -> [|Error (e + sprintf "syntax error in\n:%A\n" vLog)|]
                | MATCH rgxRuntimeError (_, [e]) -> [|Error (e+ sprintf "runtime error in\n%A\n" vLog)|]
                | MATCH rgxReg (_,[rNum;rVal]) -> [|Ok (R (int rNum), int rVal)|]
                | _ -> [||])

        let errors = 
            matches
            |> Array.collect (function | Error e -> [|e|] |_ -> [||])
            |> Array.toList

        let regsAll =
            matches
            |> Array.collect (function | Ok x -> [|x|] | _ -> [||])
            |> Array.toList
        
        
        match errors with
        | [] when regsAll.Length >= 16*(1+n) -> 
            let regsReal = List.take 16 (List.skip (16*n) regsAll)
            let regsAfter = List.take 16 regsAll
            //printfn "Log:%A" regsReal
            Ok { 
            Regs = regsReal
            RegsAfterPostlude = regsAfter
            State =  decodeStateFromRegs regsAfter
            }
        | [] -> failwithf "No registers found in non-Error Visual log: %A" vLog
        | e -> Error e

    /// translate a text string for a custom log file escaping @ and '\n'
    /// those characters are used to delimit log entries
    let writeEscaped (txt:string) =
        txt.Replace("@","@@")
        |> (fun txt -> txt.Replace ("\n", "@n"))
        |> (fun txt -> txt.Replace ("\r","@r"))

    /// inverse of writeEscaped
    let readEscaped (line: string):string =
        let newl = sysNewline |> Seq.toList
        let rec rd s res =
            match s with
            | '@' :: '@' :: rest -> rd rest ('@' :: res)
            | '@' :: 'n' :: rest -> rd rest ('\n' :: res)
            | '@' :: 'r' :: rest -> rd rest ('\r' :: res)
            | '@' :: _ -> failwithf "What? unexpected '@' in coded string"
            | c :: rest -> rd rest (c :: res)
            | [] -> List.rev res
        rd (Seq.toList line) []
        |> List.toArray
        |> System.String

    /// write a specified set of output registers (not all) to a log file
    /// flags can be written as a pseudo-register (see Out type defn)
    /// in which case they are written as register no -1
    let writeRegs (outs:(Out*int) list) =
        outs
        |> List.map (function | R n,m -> sprintf "%d %d" n m )
        |> String.concat " "
    
    /// inverse of writeRegs
    let readRegs (str:string) =
        let s = str.Split([|' ';'\n';'\r';'\t'|],System.StringSplitOptions.RemoveEmptyEntries)
        [0..s.Length/2-1]
        |> List.map (fun n -> R (int s.[2*n]), int s.[2*n+1] )

    let writeVisOut (vso: VisOutput) =
        sprintf "%s,%s" (writeRegs vso.Regs) (writeRegs vso.RegsAfterPostlude)
  
    let readFileCacheItem ( srcS:string, regsS:string) =
        let txt,regs = readEscaped srcS, regsS.Split([|','|])
        let outs = regs |> Array.map readRegs
        let state = decodeStateFromRegs outs.[1]
        txt, {Regs = outs.[0] ; RegsAfterPostlude = outs.[1]; State = state}
        
    let writeFileCacheItem ( src:string, vso: VisOutput) =
        [| writeEscaped src ; writeVisOut vso |]
       
        


    /// read the entire cache from disk into a Map
    /// coalesce multiple identical entries
    /// cacheF: path of cache file on disk
    let readCache cacheF =
        let dat = File.ReadAllLines cacheF
        let n = dat.Length
        if n % 3 <> 0 then 
            failwithf "------\n%A\number of lines (%d) in cache not divisible by 3\n" dat n
        [|0..(n/3)-1|]
        |> Array.map (fun n -> readEscaped dat.[3*n], (readRegs dat.[3*n+1], readRegs dat.[3*n+2]))
        |> Array.distinct
        |> Map.ofArray


        
    /// add a single new item to the result cache
    /// scr: assembly source run
    /// outs: visual result info
    let appendCacheItem (cacheF:string) src (vso: VisOutput) =
        let outs = vso.Regs
        let outsAfterPostlude = vso.RegsAfterPostlude
        try
            File.AppendAllLines (cacheF, [| writeEscaped src ; writeRegs outs; writeRegs outsAfterPostlude|])
        with
            | e -> printfn "Append failed: %A" e; failwithf "What?"

    let appendMapToCache (cacheF:string) cacheToAppend =
        cacheToAppend
        |> Map.toSeq 
        |> Seq.collect (fun (src, (outs, outsAfter)) -> [ writeEscaped src ; writeRegs outs ; writeRegs outsAfter ])
        |> (fun s ->
                try
                    match File.Exists cacheF with
                    | true -> File.AppendAllLines (cacheF, s)
                    | false -> File.WriteAllLines(cacheF, s)
                with
                    | e -> printfn "Append failed: %A" e; failwithf "What?"
           )
        GlobalCache <- {Dat = readCache cacheF ; Limit = 0}
