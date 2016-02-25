module FunScript.TypeScript.Provider

open System
open System.Diagnostics
open System.Reflection
open System.IO
open System.Net
open System.Net.Security
open System.CodeDom.Compiler

open Ionic.Zip
open Argu

open FunScript.TypeScript.AST
open FunScript.TypeScript.Parser
open FunScript.TypeScript.TypeGenerator

let openUriStream uri =
    let acceptAllCerts = RemoteCertificateValidationCallback(fun _ _ _ _ -> true)
    ServicePointManager.ServerCertificateValidationCallback <- acceptAllCerts
    let req = System.Net.WebRequest.Create(Uri(uri))
    let resp = req.GetResponse()
    resp.GetResponseStream()

/// Open a file from file system or from the web in a type provider context
/// (File may be relative to the type provider resolution folder and web
/// resources must start with 'http://' prefix)
let openFileOrUri resolutionFolder (fileName:string) =
    if fileName.StartsWith("http://", StringComparison.InvariantCultureIgnoreCase) ||
        fileName.StartsWith("https://", StringComparison.InvariantCultureIgnoreCase) then
        new StreamReader(openUriStream fileName)
    else
        // If the second path is absolute, Path.Combine returns it without change
        let file =
            if fileName.StartsWith ".." then Path.Combine(resolutionFolder, fileName)
            else fileName
        new StreamReader(file)

let compile outputAssembly references (source : string) =
    let source =
        if source.Trim() = "" then "namespace FunScript.TypeScript"
        else source
    if File.Exists outputAssembly then
        true
    else
        /// This is to get the code dom to work!
        if System.Environment.GetEnvironmentVariable("FSHARP_BIN") = null then
            let defaultFSharpBin = @"C:\Program Files (x86)\Microsoft SDKs\F#\3.1\Framework\v4.0"
            if Directory.Exists defaultFSharpBin then
                Environment.SetEnvironmentVariable("FSHARP_BIN", defaultFSharpBin)
            else failwith "Expected FSHARP_BIN environment variable to be set."
    
        let fsharpBin = System.Environment.GetEnvironmentVariable("FSHARP_BIN")
    
        use provider = new FSharp.Compiler.CodeDom.FSharpCodeProvider()
        let parameters = CompilerParameters(OutputAssembly = outputAssembly)
        let msCorLib = typeof<int>.Assembly.Location
        parameters.ReferencedAssemblies.Add msCorLib |> ignore<int>
        let fsCorLib = Path.Combine(fsharpBin, "FSharp.Core.dll")
        parameters.ReferencedAssemblies.Add fsCorLib |> ignore<int>
        let funScriptInterop = typeof<FunScript.JSEmitInlineAttribute>.Assembly.Location
        parameters.ReferencedAssemblies.Add funScriptInterop |> ignore<int>
        parameters.ReferencedAssemblies.AddRange references
        let sourceFile = outputAssembly + ".fs"
        File.WriteAllText(sourceFile, source)
        
        let results = provider.CompileAssemblyFromFile(parameters, [|sourceFile|])
        match [| for err in results.Errors -> err |] with
        | [||] -> true
        | errors ->
            let hasErrors = results.Errors.HasErrors
            printfn (if hasErrors then  "\tFailed to compile:" else "\tCompiled with warnings:")
            errors |> Array.iter ( fun err ->
                printf "\t\t%s  %s " (if err.IsWarning then "WARNING" else "ERROR") err.ErrorNumber
                printfn "at line %d, column %d in %s" err.Line err.Column (err.FileName.Trim())
                printfn "\t\t\t %s" err.ErrorText)
            not hasErrors

let generateAssemblies tempDir postBuildStep inputs  =
    if not(Directory.Exists tempDir) then
        Directory.CreateDirectory tempDir |> ignore

    let filterAndLogParsingErrors s =
        let split (successes , errors) (path, name, parserResult) =
            match parserResult with
            | Success(def) -> (successes |> Seq.append [(path, name, def)], errors)
            | Failure(msg) -> (successes , errors |> Seq.append [(path, name, msg)])
        let outputError i (_,name,_) =
            if i = 0 then printfn "Parsing failed for the following definition files (see failed-parsing.txt for details):" else ()
            printfn "%s" name
        let successes, errors = s |> Seq.fold (split) (Seq.empty, Seq.empty)
        errors |> Seq.iteri outputError
        errors
        |> Seq.map (fun (_,_,msg) -> msg)
        |> Seq.toArray
        |> fun xs -> File.WriteAllLines("failed-parsing.txt", xs)
        |> ignore

        successes

    let outputFiles =
        inputs
        |> Seq.map (fun (path, name, contents) -> path, name, Parser.parseDeclarationsFile contents)
        |> filterAndLogParsingErrors
        |> Seq.toList
        |> TypeGenerator.Compiler.generateTypes
    let assemblyLocation moduleName =
        Path.Combine(tempDir, "FunScript.TypeScript.Binding." + moduleName + ".dll")
    let outputMappings =
        outputFiles |> Seq.map (fun (name, contents, dependencies) ->
            name, (contents, dependencies))
        |> Map.ofSeq
    let tryCreate name =
        try
            let contents, dependencies = outputMappings.[name]
            let moduleLocation = assemblyLocation name
            printfn "Generating assembly for %s..." name
            let dependencyLocations =
                dependencies |> List.toArray |> Array.map assemblyLocation
            let wasSuccessful =
                compile
                    moduleLocation
                    dependencyLocations
                    contents
                && postBuildStep name moduleLocation dependencies
            if wasSuccessful then Some moduleLocation
            else None
        with ex ->
            printfn "[ERROR] %s" ex.Message
            None

    let generateAssembly generateAssembly name =
        let hasDependencies() =
            async {
                let dependencies = snd outputMappings.[name]
                let! results =
                    dependencies |> Seq.map generateAssembly
                    |> Async.Parallel
                return results |> Array.forall (snd >> Option.isSome)
            }

        async {
            let! hasDependencies = hasDependencies()
            if hasDependencies then return name, tryCreate name
            else return name, None
        }

    let eventually f =
        let started, notStarted = 1, 0
        let state = ref notStarted
        let finishedEvent = new Threading.ManualResetEvent(false)
        let result = ref Unchecked.defaultof<_>
        async {
            let isFirst = System.Threading.Interlocked.Exchange(state, started) = notStarted
            if isFirst then
                try
                    let! x = f()
                    result := x
                finally
                    finishedEvent.Set() |> ignore
            let! _ = Async.AwaitWaitHandle finishedEvent
            return !result
        }

    let memoize f =
        let cache = System.Collections.Concurrent.ConcurrentDictionary()
        let rec find x =
            match cache.TryGetValue x with
            | false, _ ->
                cache.TryAdd(x, eventually (fun () -> f find x)) |> ignore
                cache.[x]
            | true, v -> v
        find

    let generateAssembly = memoize generateAssembly

    outputFiles |> List.map (fun (name, _, _) ->
        generateAssembly name)
    |> List.map Async.RunSynchronously
//    |> Async.Parallel
//    |> Async.RunSynchronously

let loadDefaultLib() =
    let ass = typeof<AST.AmbientClassBodyElement>.Assembly
    use stream = ass.GetManifestResourceStream("lib.d.ts")
    use reader = new StreamReader(stream)
    "lib.d.ts", "lib", reader.ReadToEnd()

let blacklist =
    set [
        "text-buffer"
        "atom"
        "lib"
        "joi"
        "status-bar"
    ]

let whitelist =
    set [
        "yui-test"
    ]

let readBindings (files : seq<string*(unit->Stream)>) = 
    files
    |> Seq.filter (fun (fileName, _) ->
            fileName.EndsWith ".d.ts") 
    |> Seq.map (fun (fName, openReader) ->
            use stream = openReader()
            use reader = new StreamReader(stream)
            let filename = Path.GetFileName fName
            let moduleName = filename.Substring(0, filename.Length - ".d.ts".Length)
            sprintf "I:\\%s" (fName.Replace('/','\\')), moduleName, reader.ReadToEnd())
    |> Seq.filter (fun (path,moduleName,_) ->
        whitelist.Contains moduleName
        || not (path.Contains "test" || blacklist.Contains moduleName))
//        |> Seq.filter (fun (path,moduleName,_) ->
//            (moduleName = "jquery" || moduleName.Contains "rx") && not(moduleName.Contains "knockout"))
    |> Seq.toList
    
let addDefaultLib modules = loadDefaultLib() :: modules

let openFiles paths = 
    paths 
    |> Seq.map (fun path -> path, (fun unit -> File.OpenRead(path) :> Stream))

let openZip (path:string) =
    let zip = ZipFile.Read path
    zip.Entries
    |> Seq.filter (fun entry ->
        entry.FileName.EndsWith ".d.ts")
    |> Seq.map (fun entry -> entry.FileName, (fun unit -> entry.OpenReader() :> Stream))
    |> Seq.toList
    
let downloadTypesZip tempDir zipUri =
    let tempFile = Path.Combine(tempDir, "Types.zip")
    if not (File.Exists tempFile) then
        printfn "Downloading DefinitelyTyped repository..."
        use stream = openUriStream zipUri
        use memStream = new MemoryStream()
        stream.CopyTo memStream
        File.WriteAllBytes(tempFile, memStream.ToArray())
    tempFile

let template =
    lazy
        let ass = typeof<AST.AmbientClassBodyElement>.Assembly
        use stream = ass.GetManifestResourceStream("template.nuspec")
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

let nuspec name version dependencies =
    let deps =
        dependencies
        |> Seq.map (fun n -> sprintf "        <dependency id=\"FunScript.TypeScript.Binding.%s\" version=\"%s\" />" n version)
        |> String.concat System.Environment.NewLine
    template.Value
        .Replace("{package}", name)
        .Replace("{version}", version)
        .Replace("{dependencies}", deps)

let uploadPackage tempDir key version moduleName assLoc deps =
    printfn "Generating nuspec for %s..." moduleName
    let nuspecFile = Path.Combine(tempDir, sprintf "FunScript.TypeScript.Binding.%s.nuspec" moduleName)
    File.WriteAllText(nuspecFile, nuspec moduleName version deps)
    let packProcess = Process.Start("nuget.exe", sprintf "pack %s" nuspecFile)
    packProcess.WaitForExit()
    packProcess.ExitCode = 0 && (
        let pushProcess =
            Process.Start(
                "nuget.exe",
                sprintf "push FunScript.TypeScript.Binding.%s.%s.nupkg %s" moduleName version key)
        pushProcess.WaitForExit()
        pushProcess.ExitCode = 0)

open System.Threading
open System.Collections.Concurrent

let defTypedUri = "https://github.com/borisyankov/DefinitelyTyped/archive/master.zip"

type CliArguments = 
    | Push of version:string * nugetKey:string
    | From_Url of string
    | From_Path of string
with 
    interface IArgParserTemplate with
        member s.Usage = 
            match s with 
            | Push _ -> "triggers nuget push, requires version and nuget key needed to push generated assemblies to NuGet as packages"
            | From_Url _ -> ("specify link to zip archive with type bindindgs. " 
                            + "Link to definitely typed repo is used by default if no value is specified: " 
                            + defTypedUri) 
            | From_Path _ -> "specify local path to zip archive with type bindigs or signle type binding file"

[<EntryPoint>]
let main args =
    try
        let tempDir = System.Environment.CurrentDirectory
        let outDir = Path.Combine(tempDir, "Output")
        
        let argParser = ArgumentParser.Create<CliArguments>()
        let argsParsed = argParser.Parse(args, raiseOnUsage=false)
        
        if argsParsed.IsUsageRequested then 
            let usage = argParser.Usage()
            printfn "%s" usage
        else
            let modules =
                match argsParsed.Contains <@ From_Url @>, argsParsed.Contains <@ From_Path @> with
                | true, false -> let url = argsParsed.GetResult(<@ From_Url @>, defaultValue = defTypedUri)
                                 downloadTypesZip tempDir url |> openZip |> List.toSeq
                                 
                | false, true -> let path = argsParsed.GetResult(<@ From_Path @>)
                                 if path.EndsWith ".zip" then
                                    openZip path |> List.toSeq
                                 else
                                    openFiles [path]
                | _, _ -> failwith "either url to bindigs zip or local path must be specified. See help for more details."
                |> readBindings 

            modules
            |> addDefaultLib
            |> generateAssemblies outDir (fun moduleName location dependencies ->
                match argsParsed.Contains <@ Push @> with
                | false -> true
                | true  -> let version, key = argsParsed.GetResult(<@ Push @>)
                           uploadPackage outDir key version moduleName location dependencies)
            |> Seq.filter (snd >> Option.isNone)
            |> Seq.map fst
            |> Seq.toArray
            |> fun xs -> File.WriteAllLines("failed-modules.txt", xs)
            |> ignore
        0
    with ex ->
        printfn "[ERROR] %s" (ex.ToString())
        Console.ReadLine() |> ignore
        1
