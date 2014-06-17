module ParserHelper
open System

// convert a string to a list of characters
let (~&) (str:string) = str.ToCharArray() |> List.ofArray
// convert a list of characters to a string
let (~%) (chars:char list) = new String(Array.ofList chars)

type ParseData = char list
type ParseResult<'r> =
    {
        ParseResults : ('r * ParseData) list
        ErrorList : string list
        Unexpected : string
        NumberOfNotParsedChars : int
    } 
module ParseResult =
    let empty =
        {
            ParseResults = []
            ErrorList = []
            Unexpected = ""
            NumberOfNotParsedChars = 0
        }
    let fromLength cs =
        { empty with NumberOfNotParsedChars = List.length cs }

type Parser<'r> = Parser of (ParseData -> ParseResult<'r>)
let parse (Parser p) = p
let handleParse p d =
    let results = parse p &d
    match results.ParseResults with
    | [] -> 
        match results.ErrorList with
        | [] -> failwithf "Unexpected: %s" results.Unexpected
        | (h::_) -> failwithf "Expected: %s" h
    | ((data,_)::_) -> data


let (>>=) p f = Parser(fun cs ->
    
    let pResult = parse p cs
    match pResult.ParseResults with
    | [] -> { ParseResult.empty with 
                NumberOfNotParsedChars = pResult.NumberOfNotParsedChars;
                ErrorList = pResult.ErrorList; Unexpected = pResult.Unexpected }
    | rs ->
        rs
        |> List.map (fun (data, next) -> parse (f data) next)
        |> List.fold (fun newRes curRes ->
            { curRes with
                ParseResults = List.append curRes.ParseResults newRes.ParseResults
                ErrorList = List.append curRes.ErrorList newRes.ErrorList }) 
            { ParseResult.fromLength cs with ParseResults = []; ErrorList = pResult.ErrorList }
            //{ ParseResult<_>.Empty with NumberOfNotParsedChars = List.length cs }
    )

let (>>) p q = p >>= fun _ -> q


let (++) p q = Parser(fun cs ->
    let pResult = parse p cs
    let qResult = parse q cs
    {
        ParseResults = List.append pResult.ParseResults qResult.ParseResults
        ErrorList = List.append pResult.ErrorList qResult.ErrorList
        Unexpected = if pResult.NumberOfNotParsedChars < qResult.NumberOfNotParsedChars then pResult.Unexpected else qResult.Unexpected
        NumberOfNotParsedChars = min pResult.NumberOfNotParsedChars qResult.NumberOfNotParsedChars
    })

let (<|>) p q = Parser(fun cs ->
    let pResult = parse p cs
    match pResult.ParseResults with
    | [] ->
      let qResult = parse q cs
      { qResult with ErrorList = List.append pResult.ErrorList qResult.ErrorList }
    | other -> pResult)
    
let mreturn r = Parser(fun cs -> { ParseResult.fromLength cs with ParseResults = [(r,cs)] })
let lambda = Parser(fun cs -> ParseResult.fromLength cs)
let (>>@) p exp = Parser(fun cs ->
    let pResult = parse p cs
    match pResult.ParseResults with
    | [] -> 
        { pResult with ErrorList = [exp] }
    | other -> pResult)

let item = Parser(fun cs ->
    match cs with
    | [] -> { ParseResult.empty with Unexpected = "end-of-file" }
    | c::cs' -> { ParseResult.empty with ParseResults = [(c,cs')] })

let err unex = Parser(fun cs ->  { ParseResult.fromLength cs with Unexpected = unex })

let sat cond exp =
    (item >>= fun c -> if cond c then mreturn c else err %[c])
    >>@ exp

let char c = sat ((=)c) %[c]

let digit = sat (fun c ->
  (List.tryFind ((=)c) ['0'..'9']).IsSome) "a digit"

let alpha = sat (fun c ->
    (List.tryFind ((=)c)(List.append
      ['a'..'z'] ['A'..'Z'])).IsSome) "a letter"


let rec many0 (p:Parser<'a>) = many1 p <|> mreturn []
and many1 (p:Parser<'a>) = p >>= fun (r:'a) -> many0 p >>= fun rs -> mreturn (r::rs)

let rec symbol cs =
    (match cs with
    | [] -> mreturn []
    | c::cs' -> char c >> symbol cs' >> mreturn cs)
    >>@ %cs

let sepBy p sep =
    p >>= fun r ->
    many0 (sep >> p) >>= fun rs ->
    mreturn (r::rs)

let (<<) p e =
    p >>= fun r ->
    e >>
    mreturn r

let endBy p e =
    p >>= fun r ->
    (p ++ e) >>
    mreturn r
    
let space = many1 (char ' ' <|> char '\t')
let maybespace = many0 (char ' ' <|> char '\t')
let maybeWhitespace = many0 (char ' ' <|> char '\t' <|> char '\r' <|> char '\n')

let endoffile = Parser(fun cs ->
    match cs with
    | [] -> { ParseResult.empty with ParseResults = [([],[])] }
    | _ -> { ParseResult.fromLength cs with ErrorList = ["end-of-file"]; Unexpected = %[List.head cs] })

let newline = symbol &"\r\n"
let endofline = maybespace >> newline >> maybespace
