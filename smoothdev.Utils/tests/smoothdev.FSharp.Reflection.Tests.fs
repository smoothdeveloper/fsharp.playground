module ``smoothdev FSharp Reflection Tests``

open smoothdev.FSharp.Reflection
open NUnit.Framework
open FsUnit

type Tags = A | B | C | D

[<Test>]
let ``can reflect Tag type discriminated union`` () =  
  let cases = smoothdev.FSharp.Reflection.TagUnionCaseReflection<Tags>.AllCases
  
  let expected = [A;B;C;D]
  let actual = cases |> Seq.map (fun c -> c.Value)
  expected |> should equal actual 
  printfn "%O %O" actual expected