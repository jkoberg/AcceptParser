module AcceptsParser.Parse

open FParsec

let grammar = """
  Content-Type   = "Content-Type" ":" media-type
  media-type     = type "/" subtype *( ";" parameter )
  parameter      = attribute "=" value
  attribute      = token
  value          = token | quoted-string
  quoted-string  = ( <"> *(qdtext | quoted-pair ) <"> )
  qdtext         = <any TEXT except <">>
  quoted-pair    = "\" CHAR
  type           = token
  subtype        = token
  token          = 1*<any CHAR except CTLs or separators>
  separators     = "(" | ")" | "<" | ">" | "@"
                 | "," | ";" | ":" | "\" | <">
                 | "/" | "[" | "]" | "?" | "="
                 | "{" | "}" | SP | HT
  CTL            = <any US-ASCII ctl chr (0-31) and DEL (127)>
  """
type Param =
  | Q of float
  | Mxb of int64
  | Other of string * string

type MediaRange = 
  | Qualified of string * string
  | SubtypesOf of string
  | Any

type AcceptItem = {
  range: MediaRange
  q: float option
  mxb: int64 option
  parameters: Param list
  }
       
type Header = 
  | Accept of AcceptItem list
  | Other of string * string

  

module Elements = 
  let rstrip (s:string) = s.TrimEnd()

  let separators = "()<>@,;:\\\"/[]?={} \t"
  let controls =  [| for n = 0 to 31 do yield char n |] |> System.String.Concat

  let token = many1 (noneOf (separators + controls)) |>> System.String.Concat

  let headerName = parse {
    let! t = token
    do! skipString ":"
    return t
    }

  let indentedLine = many1 (anyOf " \t") >>. restOfLine true


  let skipWs : Parser<unit,unit> = skipMany1 (anyOf " \t")

  let nonNewlineSpaces =
    skipWs <|>
    skipAnyOf "\n" >>. skipWs 

  let rejoinLines = List.map rstrip >> String.concat " "
  let headerValueLines = many1 indentedLine
  let headerValue = headerValueLines |>> rejoinLines
 
  let body = (many anyChar |>> System.String.Concat) .>> eof

  let unquote= function
    | 'r' -> '\r'  | 'n' -> '\n'  | 't' -> '\t'  | 'b' -> '\b'
    | 'a' -> '\a'  | 'f' -> '\f'  | 'v' -> '\v'  | _ as qc -> qc

  let quotedPair = skipString "\\" >>. anyChar |>> unquote
  let qdText = noneOf "\""
  let quotedString = skipString "\""  >>. many (quotedPair <|> qdText) .>> skipString "\"" |>> System.String.Concat

  let attribute = token
  let value = token <|> quotedString

  let parameter = attribute .>> skipString "=" .>>. value |>> Param.Other
  let qparam = skipString "q=" >>. pfloat |>> Q
  let mxbparam = skipString "mxb=" >>. pint64 |>> Mxb
  let parameters = many (skipString ";" >>. (qparam <|> mxbparam <|> parameter))

  let mtype = token
  let msubtype = token

  let rec scanPs state = function | [] -> state
                                  | p::ps -> match p with | Mxb count -> scanPs {state with mxb=Some count} ps
                                                          | Q q -> scanPs {state with q=Some q} ps
                                                          | _ -> scanPs state ps

  let rec scanPs' state = function
    | [] -> state
    | p::ps -> match p with
               | Mxb count -> scanPs {state with mxb=Some count} ps
               | Q q -> scanPs {state with q=Some q} ps
               | _ -> scanPs state ps


  let matchRange r (mt:string,mst:string) =
    match r with
    | Qualified (t,st) -> (t=mt, st=mst)
    | SubtypesOf t -> (t=mt, true)
    | Any -> true, true

  let unpackAcceptItem = function
    | (("*","*"), ps) -> scanPs {range=Any; q=None; mxb=None; parameters=ps} ps
    | ((t,"*"), ps) -> scanPs {range=SubtypesOf t; q=None; mxb=None; parameters=ps} ps
    | ((t,st) as fulltype, ps) -> scanPs {range=Qualified fulltype; q=None; mxb=None; parameters=ps} ps

  let mediaType = mtype .>> skipString "/" .>>. msubtype .>>. parameters 
  
  let accepts = parse {
    do! spaces
    let! mt = mediaType |>> unpackAcceptItem
    let! mts = many (parse {
      do! skipString ","
      do! spaces
      return! mediaType |>> unpackAcceptItem
      })
    return mt :: mts
    }

  let enumerate = List.mapi (fun n i -> n,i)

  let sortAccepts fulltype = enumerate >> List.sortBy (fun (n,item:AcceptItem) -> (matchRange item.range fulltype), item.q, n)

  let parseAcceptHeader = many1 mediaType |>> List.map unpackAcceptItem

  let otherHeader = tuple2 headerName headerValue |>> Header.Other
  let parseS s = runParserOnString parseAcceptHeader () "" s
  let acceptHeader = skipString "Accept:" >>. accepts |>> Accept

  let header = acceptHeader <|> otherHeader 
  
  let headers = (many1 header) .>> newline
  let httpMessage = tuple2 headers body



let parseHttpMessage = run Elements.httpMessage

let parseAccepts = run Elements.accepts



