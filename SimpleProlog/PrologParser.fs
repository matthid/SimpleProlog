module PrologParser

open ParserHelper

type Variable = char
type Procedure = 
    {
        Head : Variable 
        Others : Variable list
    }
type Target = Variable list
type Fact = Procedure // Only a procedure with empty variable list
let createProcedure variable others =  { Head = variable; Others = others }
let createFact variable = createProcedure variable []
type PrologClause =
    | Procedure of Procedure
    | Target of Target
type PrologProgram = 
    {
        Procedures : Procedure list
        Targets : Target list 
    }

    
let validChars = [ ':'; '-'; '.'; ','; '\r'; '\n'; '?' ]
let isInvalidChar c =
    if System.Char.IsLetter(c) then false
    else if validChars |> Seq.exists ((=) c) then false
    else true
let lineWithInvalidChar (s:string) =
    if s |> Seq.exists isInvalidChar then true
    else false

module PrologParserHelper =
    open ParserHelper

    let rec program = prologProgram << endoffile
    and prologProgram =
        maybeWhitespace >>
        procedureList >>= fun procedures ->
        target >>= fun target ->
        maybeWhitespace >>
        (  { Procedures = procedures
             Targets = [ target ] } |> mreturn)
    and procedureList = 
        many0 (
            procedureFact >>= fun fact ->
            endofline >>
            mreturn fact)
    and procedureFact = procedure <|> fact
    // A:-B,C,D.
    and procedure =
        variable >>= fun var ->
        symbol &":-" >>
        variableList >>= fun varList ->
        symbol &"." >>
        (  { Head = var
             Others = varList } |> mreturn)
    // A.
    and fact =
        variable >>= fun var ->
        symbol &"." >>
        (  { Head = var
             Others = [] } |> mreturn)
    //?-A.
    //?-A,B,C.
    and target =
        symbol &"?-" >>
        variableList >>= fun varList ->
        symbol &"." >>
        ((varList : Target) |> mreturn)
    and variableList = sepBy variable (symbol &",")
    and variable = 
        alpha >>= fun varName ->
        (varName : Variable) |> mreturn
        
    let parseProgram s = handleParse program s

let parseProgram (reader:System.IO.StringReader) =
    async {
        let! line = reader.ReadToEndAsync() |> Async.AwaitTask
        return PrologParserHelper.parseProgram line
    }
let parseProgramString (s:string) =
    async {
        return PrologParserHelper.parseProgram s
    }

let parseProgramSync (reader:System.IO.StringReader) =
    parseProgram reader |> Async.RunSynchronously
let parseProgramStringSync (s:string) =
    parseProgramString s |> Async.RunSynchronously