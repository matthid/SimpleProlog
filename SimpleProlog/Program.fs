// Weitere Informationen zu F# unter "http://fsharp.net".
// Weitere Hilfe finden Sie im Projekt "F#-Lernprogramm".

    
open System

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    if argv.Length = 0 then
        // interpreter mode
        printfn "Prolog interpreter mode, please enter your clauses!"
        printfn "You get results when you enter your target clause."
        printfn "Enter \"exit\", \"quit\" or \"q\" to quit."
        let builder = new System.Text.StringBuilder()
        while true do
            try
                printf "> "
                let line = Console.ReadLine()
                if (line = "exit" || line = "q" || line = "quit")  then
                    System.Environment.Exit(0)
                builder.AppendLine(line) |> ignore
                if line.StartsWith("?") then
                    let parsed = PrologParser.parseProgramStringSync (builder.ToString())
                    let result = SimpleProlog.syncInterpret parsed
                    printfn (if result then "YES" else "NO")
                    builder.Clear() |> ignore
            with exn ->
                printfn "Seems like you entered something invalid, try again."
                printfn "Error was: %O" exn
                builder.Clear() |> ignore
                try
                    let readme = System.IO.File.ReadAllText("README.md")
                    printfn "Readme: %s" readme
                with exn2 ->
                    printfn "Error finding README.md: %O" exn2
                    printfn "Readme not found, try to find it yourself and read it! (https://github.com/matthid/SimpleProlog)"
        


    0 // Exitcode aus ganzen Zahlen zurückgeben
