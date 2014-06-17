module SimpleProlog

open System.Threading.Tasks;
open PrologParser


let doStep stepper prog  =
    { prog with Targets = stepper prog }

let isFinished (targets:Target list) =
    targets |> List.exists (fun target -> target.IsEmpty)

let rec handleTargetAsync (procedures: Procedure list) (targets:Target list) =
    async {
        if targets.IsEmpty then
            return false // this means we have no target formula, or our last step trying to apply a procedure found no applicable procedure.
        else if isFinished targets then
            return true // true means a path was found -> ie the formula is unsolvable
        else
        let tasks =
            targets
            |> Seq.collect (fun target ->
                // We have to process every target
                target
                |> Seq.map (fun item ->
                    // We can possibly process every item within a target
                    procedures
                    // Is the procedure applicable?
                    |> List.filter (fun proc -> proc.Head = item)
                    // apply procedure
                    |> List.map (fun proc ->
                        target 
                        // remove 'item' from list
                        |> Seq.filter (fun t -> t <> item)
                        // append procedure results
                        |> Seq.append (proc.Others)
                        |> Seq.distinct
                        |> Seq.toList
                        : Target)
                    // For every new Target we have work to do
                    |> handleTargetAsync procedures
                    |> Async.StartAsTask))
        
        // if any of the tasks return with 'true' we can return true ourselfs
        let source = TaskCompletionSource()
        for t in tasks do
            t.ContinueWith (new System.Action<Task<bool>>(fun t -> 
                if t.Result then source.TrySetResult(true) |> ignore))
            |> ignore
        // if all childs return false we can return false as well.
        Task.WhenAll(tasks).ContinueWith (new System.Action<Task<_>>(fun results -> 
                if results.Result |> Seq.forall (not) then source.TrySetResult(false) |> ignore))
            |> ignore
        return! source.Task |> Async.AwaitTask
    }

let interpretProgram (prog:PrologProgram) =
    async {
        let! result = handleTargetAsync prog.Procedures prog.Targets
        return result
    }
    
let syncInterpret (prog:PrologProgram) = interpretProgram prog |> Async.RunSynchronously