module ParsePrologTest

// https://github.com/fsharp/FsCheck/blob/master/Docs/Documentation.md
// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/
open SimpleProlog
open PrologParser
open FsUnit
open NUnit.Framework
open Swensen.Unquote


[<Test>]
let ``Check valid lines``() =
    test <@ lineWithInvalidChar "" = false @>
    test <@ lineWithInvalidChar "A." = false @>
    test <@ lineWithInvalidChar "?-A." = false @>
    test <@ lineWithInvalidChar "?-A,B,C." = false @>
    test <@ lineWithInvalidChar "A:-B." = false @>
    test <@ lineWithInvalidChar "A:-B,C,D." = false @>
    test <@ lineWithInvalidChar "A.^" = true @>
    test <@ lineWithInvalidChar "^" = true @>
    test <@ lineWithInvalidChar "#" = true @>
    test <@ lineWithInvalidChar "t}" = true @>
    test <@ lineWithInvalidChar "*" = true @>



let zeroFactProgramString =
    """?-B."""
let zeroFactProgram =
    {
        Procedures = [ ]
        Targets = [ ['B'] ]
    }

[<Test>]
let ``Check parse zeroFactProgram``() =
    test <@ PrologParser.parseProgramStringSync zeroFactProgramString = zeroFactProgram @>
    


[<Test>]
let ``Check cannot parse zeroFactProgram with x at end``() =
    raises<exn> <@ PrologParser.parseProgramStringSync (zeroFactProgramString + "x") = zeroFactProgram @>

let spacedProgramString =
    """  ?-B.  
    """
let spacedProgram =
    {
        Procedures = [ ]
        Targets = [ ['B'] ]
    }

[<Test>]
let ``Check parse spacedProgram``() =
    test <@ PrologParser.parseProgramStringSync spacedProgramString = spacedProgram @>
    
let spacedEndProgramString =
    """  ?-B.     """
let spacedEndProgram =
    {
        Procedures = [ ]
        Targets = [ ['B'] ]
    }

[<Test>]
let ``Check parse spacedEndProgram``() =
    test <@ PrologParser.parseProgramStringSync spacedEndProgramString = spacedEndProgram @>
let oneFactProgramString =
    """A.
?-A."""
let oneFactProgram =
    {
        Procedures = [ createFact 'A' ]
        Targets = [ ['A'] ]
    }
    
[<Test>]
let ``Check parse oneFactProgram``() =
    test <@ PrologParser.parseProgramStringSync oneFactProgramString = oneFactProgram @>

let oneFactProgram2String =
    """A.
?-B."""
let oneFactProgram2 =
    {
        Procedures = [ createFact 'A' ]
        Targets = [ ['B'] ]
    }
    
[<Test>]
let ``Check parse oneFactProgram2``() =
    test <@ PrologParser.parseProgramStringSync oneFactProgram2String = oneFactProgram2 @>

let oneFactProgramMultipleTargetsString =
    """A.
?-B.
?-A."""
let oneFactProgramMultipleTargets =
    {
        Procedures = [ createFact 'A' ]
        Targets = [ ['B']; ['A'] ]
    }
[<Test>]
let ``Check parse oneFactProgramMultipleTargets``() =
    // Not supported by the parser, but used in unit tests anyway...
    raises<exn> <@ PrologParser.parseProgramStringSync oneFactProgramMultipleTargetsString = oneFactProgramMultipleTargets @>
    

let oneFactProgramComplexTargetsString =
    """A.
?-B,A."""
let oneFactProgramComplexTargets =
    {
        Procedures = [ createFact 'A' ]
        Targets = [ ['B'; 'A'] ]
    }
[<Test>]
let ``Check parse oneFactProgramComplexTargets``() =
    test <@ PrologParser.parseProgramStringSync oneFactProgramComplexTargetsString = oneFactProgramComplexTargets @>

let endlessProgramString =
    """A:-B.
B:-A.
A.
?-A."""
let endlessProgram =
    {
        Procedures = 
            [ createProcedure 'A' ['B']
              createProcedure 'B' ['A']
              createFact 'A' ]
        Targets = [ ['A'] ]
    }
[<Test>]
let ``Check parse endlessProgram``() =
    test <@ PrologParser.parseProgramStringSync endlessProgramString = endlessProgram @>
let biggerProgramString =
    """A:-B,C.
A:-D.
B.
B:-D.
C.
D:-E.
D.
?-A."""
let biggerProgram =
    {
        Procedures = 
            [ createProcedure 'A' ['B'; 'C']
              createProcedure 'A' ['D']
              createFact 'B'
              createProcedure 'B' ['D']
              createFact 'C'
              createProcedure 'D' ['E']
              createFact 'D']
        Targets = [ ['A'] ]
    }
[<Test>]
let ``Check parse biggerProgram``() =
    test <@ PrologParser.parseProgramStringSync biggerProgramString = biggerProgram @>