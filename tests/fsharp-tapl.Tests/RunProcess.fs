namespace FsharpTapl.Tests.Regression

open System
open System.Diagnostics

module internal RunProcess =
    //from http://fssnip.net/raw/sw
    let runProc filename args startDir = 
        
        let procStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args
            )

        match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data

        use p = new Process(StartInfo = procStartInfo)

        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))

        let started = 
            try
                p.Start()
            with | ex ->
                ex.Data.Add("filename", filename)
                reraise()

        if not started then
            failwithf "Failed to start process %s" filename

        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()

        let cleanOut l = l |> Seq.filter (fun o -> String.IsNullOrEmpty o |> not)
        cleanOut outputs,cleanOut errors