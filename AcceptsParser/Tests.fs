module AcceptsParserTests

open NUnit.Framework
open FParsec

open AcceptsParser

let (=?=) actual expected = 
  if actual <> expected then 
    Assert.Fail(sprintf "expected %A\n but was %A" expected actual)
  else
    Assert.Pass(sprintf "ok, got: %A" expected)


let testParser parser s ex =
  match run parser s with
  | Success (result, _, _) -> result =?= ex 
  | Failure (msg, _, _) -> Assert.Fail(msg)


let [<Test>] ``Multiple headers`` () = 
  let headers = 
    "Content-Type: text/xml\n" +
    "Host: www.hello.com\n" +
    "\n"

  let expected = [
    Other ("Content-Type", "text/xml")
    Other ("Host", "www.hello.com")
    ]

  testParser Parse.HEADERS headers expected


let [<Test>] ``Whole document`` () = 
  let doc = 
    "Content-Type: text/xml\n" +
    "Host: www.hello.com\n" +
    "\n" +
    "This is the body text.\n" +
    "yay!"

  let expected =
    ( [ Other ("Content-Type", "text/xml")
        Other ("Host", "www.hello.com")
      ],
      "This is the body text.\nyay!"
    )

  testParser Parse.HTTPMESSAGE doc expected


let [<Test>] ``Accept`` () = 
  let input = "text/json;format=simple;q=1.0, text/xml;q=0.9, text/*;q=0.7, text/yaml, */*;q=0.5"
  testParser Parse.ACCEPTS input [ { qualification = Qualified
                                     mtype = "text"
                                     msubtype = "json"
                                     q = 1.0
                                     mxb = None
                                     parameters = [ Param.Other ("format","simple")] }
                                   { qualification = Qualified
                                     mtype = "text"
                                     msubtype = "xml"
                                     q = 0.9
                                     mxb = None
                                     parameters = [] } 
                                   { qualification = SubtypesOf
                                     mtype = "text"
                                     msubtype = "*"
                                     q = 0.7
                                     mxb = None
                                     parameters = [] }
                                   { qualification = Qualified
                                     mtype = "text"
                                     msubtype = "yaml"
                                     q = 1.0
                                     mxb = None
                                     parameters = [] }
                                   { qualification = Any
                                     mtype = "*"
                                     msubtype = "*"
                                     q = 0.5
                                     mxb = None
                                     parameters = [] }
                                 ]


let [<Test>] ``AcceptSorted`` () = 
  let input = "text/json;format=simple;q=1.0, text/json;q=0.9, text/xml;q=0.8, text/yaml;q=0.8, */*;q=0.5"
  let sorted = FindMatches input "text/json"
  sorted =?= [ "text", "json", 1.0, []
               "text", "json", 0.9, []
               "*", "*", 0.5, [] ]