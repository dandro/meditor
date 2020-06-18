open System
open System.Diagnostics
open System.IO
open Argu

module Domain =
    type MeditorError =
        | ArgumentsNotSpecified
        | InputFileDoesNotExist
        | OutputFileWouldBeOverriden
        | FFMPEGError
        | CouldNotBuildFFMPEGArgs

    type MeditorConfig =
        { filePath: string
          output: string
          fps: int
          maxWidth: int
          start: Option<int>
          endAt: Option<int> }

    type MeditorConfigBuilder() =

        member this.Bind(v, f) =
            match v with
            | Ok v -> f v
            | Error e -> Error e

        member this.Return v = Ok v

    let meditorConfigBuilder = MeditorConfigBuilder()

    let withOption f e v =
        match f v with
        | true -> Ok v
        | false -> Error e

    let mkMeditorConfig path maxWidth output start endAt =
        meditorConfigBuilder {
            let! filePath = withOption File.Exists InputFileDoesNotExist path
            let! output = withOption (not << File.Exists) OutputFileWouldBeOverriden output
            return { filePath = filePath
                     output = output
                     maxWidth = maxWidth
                     fps = 24
                     start = start
                     endAt = endAt }
        }

module Handlers =
    open Domain

    [<Literal>]
    let ffmpegPath = "/usr/local/bin/ffmpeg"

    let mapOptions func first second =
        first
        |> Option.bind (fun x ->
            second
            |> Option.map (fun y -> func x y)
            |> Option.orElse (Some x))

    let addOption k acc v =
        acc @ [ k, v.ToString() ]

    let toFfmpegArgs (config: Domain.MeditorConfig) =
        let args =
            [ "-i", config.filePath
              "-r", config.fps.ToString()
              "-vf", sprintf "scale=%i:-2" config.maxWidth ]
            |> Some
        
        mapOptions (addOption "-ss") args config.start
        |> fun args' -> mapOptions (addOption "-to") args' config.endAt
        |> Option.map (Seq.fold (fun acc (k, v) -> sprintf "%s %s %s" acc k v) "")
        |> Option.map (fun args' -> sprintf "%s %s" args' config.output)

    let convertToGif (config: Domain.MeditorConfig) =
        match toFfmpegArgs config with
        | Some args ->
            printfn "Running command %s" args
            let ``process`` = new Process()
            ``process``.StartInfo <- ProcessStartInfo(ffmpegPath, args)
            ``process``.Start() |> ignore
            ``process``.WaitForExit()
            if ``process``.ExitCode = 0 then Ok() else Error FFMPEGError
        | None -> Error CouldNotBuildFFMPEGArgs

module CliPresenter =
    open Domain

    type CmdArgs =
        | [<AltCommandLine("-f")>] FilePath of input: string
        | [<AltCommandLine("-w")>] MaxWidth of width: int
        | [<AltCommandLine("-o")>] Output of output: string
        | [<AltCommandLine("-s")>] Start of start: int
        | [<AltCommandLine("-e")>] EndAt of endAt: int
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | FilePath _ -> "File path"
                | MaxWidth _ -> "Max width of Gif"
                | Output _ -> "Output file path"
                | Start _ -> "Start at second"
                | EndAt _ -> "End at second"

    type MeditorCommand =
        { filePath: string
          width: int
          output: string
          start: Option<int>
          endAt: Option<int> }

    let (|MeditorCommand|_|) (result: ParseResults<CmdArgs>) =
        match result with
        | args when args.Contains(FilePath) && args.Contains(MaxWidth) && args.Contains(Output) ->
            Some
                { filePath = (args.GetResult(FilePath))
                  width = (args.GetResult(MaxWidth))
                  output = (args.GetResult(Output))
                  start = (args.TryGetResult(Start))
                  endAt = (args.TryGetResult(EndAt)) }
        | _ -> None

    let getExitCode result =
        match result with
        | Ok () -> 0
        | Error _err -> 1

    let handleResult result =
        match result with
        | Ok () -> printfn "All Done! Success!"
        | Error err ->
            match err with
            | Domain.ArgumentsNotSpecified ->
                printfn "Some required arguments were not specified. Call with --help to see cli usage"
            | Domain.FFMPEGError -> printfn "FFMPEG Failed to convert the video."
            | Domain.InputFileDoesNotExist -> printfn "The input file does not exist."
            | Domain.OutputFileWouldBeOverriden -> printfn "Output file exists and would be overriden."
            | Domain.CouldNotBuildFFMPEGArgs -> printfn "Could not build FFMPEG args from your input."
        result

    let run argv =
        let errorHandler =
            ProcessExiter
                (colorizer =
                    function
                    | ErrorCode.HelpText -> None
                    | _ -> Some ConsoleColor.Red)

        let parser = ArgumentParser.Create<CmdArgs>(programName = "Meditor", errorHandler = errorHandler)

        match parser.ParseCommandLine argv with
        | MeditorCommand cmd ->
            mkMeditorConfig cmd.filePath cmd.width cmd.output cmd.start cmd.endAt
            |> Result.bind Handlers.convertToGif
        | _ ->
            printfn "%s" (parser.PrintUsage())
            Error Domain.ArgumentsNotSpecified
        |> handleResult
        |> getExitCode

[<EntryPoint>]
let main argv =
    CliPresenter.run argv
