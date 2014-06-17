module SimplePrologTests

// https://github.com/fsharp/FsCheck/blob/master/Docs/Documentation.md
// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/
open SimpleProlog

open FsUnit
open NUnit.Framework
open Swensen.Unquote

open ParsePrologTest
open PrologParser


[<Test>]
let ``Interpret zeroFactProgram``() =
    test <@ syncInterpret zeroFactProgram = false @>


[<Test>]
let ``Interpret oneFactProgram``() =
    test <@ syncInterpret oneFactProgram = true @>
  


[<Test>]
let ``Interpret oneFactProgram2``() =
    test <@ syncInterpret oneFactProgram2 = false @>



[<Test>]
let ``Interpret oneFactProgramMultipleTargets``() =
    test <@ syncInterpret oneFactProgramMultipleTargets = true @>



[<Test>]
let ``Interpret endlessProgram``() =
    test <@ syncInterpret endlessProgram = true @>

    


[<Test>]
let ``Interpret biggerProgram``() =
    test <@ syncInterpret biggerProgram = true @>