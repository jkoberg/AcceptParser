module AcceptsParser.Tests

open NUnit.Framework
open FParsec

open AcceptsParser.Parse

let (=?=) expected actual = 
  if expected <> actual then 
    Assert.Fail(sprintf "expected %A\n but was %A" expected actual)
  else
    Assert.Pass(sprintf "ok, got: %A" expected)


let testParser p s ex =
  match run p s with
  | Success (result, _, _) -> ex =?= result
  | Failure (msg, _, _) -> Assert.Fail(msg)


let [<Test>] ``Multiple headers`` () = 
  let headers = 
    "Content-Type: text/xml\n" +
    "Host: www.hello.com\n" +
    "\n"

  let expected = [
    Parse.Header.Other ("Content-Type", "text/xml")
    Parse.Header.Other ("Host", "www.hello.com")
    ]

  testParser Parse.Elements.headers headers expected


let [<Test>] ``Whole document`` () = 
  let doc = 
    "Content-Type: text/xml\n" +
    "Host: www.hello.com\n" +
    "\n" +
    "This is the body text.\n" +
    "yay!"

  let expected =
    ( [ Parse.Header.Other ("Content-Type", "text/xml")
        Parse.Header.Other ("Host", "www.hello.com")
      ],
      "This is the body text.\nyay!"
    )

  testParser Parse.Elements.httpMessage doc expected


let [<Test>] ``Accept`` () = 
  let input = "text/json;format=simple;q=1.0, text/xml;q=0.9, text/*;q=0.7, */*;q=0.5"
  testParser Parse.Elements.accepts input
    [ { range = Qualified ("text","json");
        q = Some 1.0;
        mxb = None;
        parameters = [Param.Other ("format","simple"); Q 1.0];};
      { range = Qualified ("text","xml");
        q = Some 0.9;
        mxb = None;
        parameters = [Q 0.9];};
      { range = SubtypesOf "text";
        q = Some 0.7;
        mxb = None;
        parameters = [Q 0.7];};
      { range = Any;
        q = Some 0.5;
        mxb = None;
        parameters = [Q 0.5];}]