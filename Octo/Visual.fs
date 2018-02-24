namespace VisualTest

/// Code to run Visual from parallel expecto tests
/// Ccaheing is also provided so that second time round tests run much faster
module Visual =
    open VCommon
    open VLog
    open VData

    open System.Threading
    open System.IO 
    open System
    
   
    let  VisualOpts = "--cycles --regs --syntax --runtime --mode:all "
    
    let VisualOptsMem memsize locs =
        let locshex = String.concat "," (List.map (sprintf "0x%x") locs)
        VisualOpts + (sprintf " --meminstsize:0x%x --custom:%s "  memsize locshex)
    
        
    /// run VisUAL via a separate command line process on source src
    /// returns output info as a list of registers and flags
    /// also returns register values after postlude
    let RunVisualBase (paras: Params) (src: string) : Result<VisOutput, string list>  = 
        let postludeLength = 
            paras.Postlude
            |> Seq.filter ((=) '\n') 
            |> Seq.length
        let visualJarExec vp = vp + @"jre\bin\java -jar "
        //printfn "\n\nVisual Temp files:%s\n" tempFilePath
        Directory.CreateDirectory paras.WorkFileDir |> ignore
        let runOpts = VisualOpts + sprintf " --meminstsize:0x%x " 0x1000
        let visualHeadlessExec srcFile outputFile opts = 
            (visualJarExec paras.VisualPath) + paras.VisualPath + @"content\visual_headless.jar --headless " + srcFile + " " + outputFile 
            + " " + opts
        let addQuotes s = sprintf "\"%s\"" s
        //printfn "Paths are: %s" visDir
        let srcF = Path.Combine(paras.WorkFileDir, "source.s")
        File.WriteAllText(srcF, src )
        //printfn "srcF=%s" srcF
        let outputF = Path.Combine(paras.WorkFileDir, "visoutput.log")
        let cmdArgs = "/C " + (addQuotes <| visualHeadlessExec srcF outputF runOpts)
        //printfn "%s" cmdArgs
        File.WriteAllText(paras.WorkFileDir + "comstr.txt", cmdArgs)
        try 
            let proc = System.Diagnostics.Process.Start("cmd", cmdArgs)
            proc.WaitForExit()
        with e -> ()//printfn "%s" e.Message
        let visLog = File.ReadAllLines outputF
        let visOutput = readVisualLog postludeLength visLog
        let recordError e = 
            let mess = sprintf "\n>>---SOURCE:\n%s\n-------\n\n>>>>>>>>>>>>>>>>>>>>>>\n\n%A\n----------------------\n" src "none"
            printfn "%s" mess
            File.AppendAllText( paras.WorkFileDir+"\\VisualErrors", String.concat "\n" e)
        match visOutput with
        | Error e -> recordError e
        | _ -> ()
        visOutput

    /// update global cacahe on disk with items in worker-local cache in memory
    let UpdateCache n (paras:Params) (src:string) (visResults: Result<VisOutput, string list>) = 
        match visResults with
        | Error e -> ()
        | Ok vOkRes ->
            let cacheFName = paras.CacheFileName
            let cache = workerCache
            cache.[n] <- {cache.[n] with Dat = cache.[n].Dat.Add(src, (vOkRes.Regs, vOkRes.RegsAfterPostlude))}
            if cache.[n].Dat.Count > cache.[n].Limit then
                cLock.WaitOne() |> ignore
                appendMapToCache cacheFName cache.[n].Dat
                cache.[n] <- { Dat = Map.empty; Limit = paras.CacheLimit}
                cLock.Release() |> ignore

    /// all-purpose Visual test run, with parallel execution and cacheing supported
    let RunVisualBaseWithLocksCached (paras: Params) src =
        let srcWithWrapper = paras.Prelude + "\r\n" + src + "\r\n" + paras.Postlude
                
        let rec prepDir' n =
            let dir = sprintf "%sd%d/" paras.WorkFileDir n
            let workerParas = {paras with WorkFileDir = dir}
            gLock.[n].WaitOne() |> ignore
            dirUsedHint.[n] <- true
            Directory.CreateDirectory dir |> ignore
            let res =
                match paras.Cached && GlobalCache.Dat.ContainsKey srcWithWrapper with
                | true  -> 
                    let co = GlobalCache.Dat.[srcWithWrapper]
                    Ok { Regs = fst co; RegsAfterPostlude = snd co; State = decodeStateFromRegs (snd co)}
                | false ->                     
                    let visRes = RunVisualBase workerParas srcWithWrapper
                    if paras.Cached then UpdateCache n workerParas srcWithWrapper visRes
                    visRes
                  
            dirUsedHint.[n] <- false
            gLock.[n].Release() |> ignore
            res

        let possN = 
            [0..paras.MaxConcurrentVisualDirs-1]
            |> List.filter (fun n -> dirUsedHint.[n] = false)

        match possN with
        | n :: _ -> prepDir' n
        | [] -> printfn "WARNING: all Visual directories are in use: waiting for directory"
                prepDir' 0

    /// top-level run Visual function
    let RunVisual (paras: Params) src =
        Directory.CreateDirectory paras.WorkFileDir |> ignore
        RunVisualBaseWithLocksCached paras src
           
    /// Adds postlude to assembly code to detect flags values.
    /// Returns flags , registers (before flag detection code)
    let RunVisualWithFlagsOut paras src =
        let main, post = VData.GETWRAPPER paras.InitRegs paras.InitFlags paras.MemReadBase
        let res = RunVisual {paras with Prelude=main; Postlude=post} src
        match res with
        | Error e -> failwithf "Error reading Visual Log %A" e
        | Ok ({ Regs=_; State={VFlags=fl}} as vso) -> fl, vso
 
    /// convenience function, convert 4 char string to NZCV status flag record
    let strToFlags s =
        let toBool = function | '0' -> false | '1' -> true | s -> failwithf "Bad character in flag specification '%c'" s
        match s |> Seq.toList |> List.map toBool with
        | [ a ; b ; c ; d] -> { FN=a; FZ=b;FC=c;FV=d}
        | _ -> failwithf "Wrong number of characters (should be 4) in flag specification %s" s

    /// initialise caches and semaphores for correct cached parallel operation
    /// called before any VisuAL process is started
    let initCaches (paras:Params) = 
        cLock <- new Semaphore(1,1,"visualCache")
        gLock <- 
            [|0..paras.MaxConcurrentVisualDirs-1|]
            |> Array.map (fun n -> new Semaphore(1,1,sprintf "visualDir%d" n))
        dirUsedHint <- Array.replicate paras.MaxConcurrentVisualDirs false
        workerCache <- Array.replicate paras.MaxConcurrentVisualDirs { Dat = Map.empty; Limit = paras.CacheLimit }
        if File.Exists paras.CacheFileName then 
            GlobalCache <- {Dat = readCache  paras.CacheFileName; Limit = 0}
            File.Delete paras.CacheFileName
            appendMapToCache paras.CacheFileName GlobalCache.Dat
                
    /// flush caches writing all info to global cache
    /// called after all VisUAL processes have terminated
    let finaliseCaches (paras:Params) =
        let cMap mArr =
            Array.collect (fun {Dat=m} -> Map.toArray m) mArr
            |> Array.distinct
            |> Map.ofArray
        workerCache
        |> cMap
        |> appendMapToCache paras.CacheFileName

