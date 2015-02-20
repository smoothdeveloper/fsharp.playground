module ``smoothdev FSharp Reflection Tests``

open FsUnit
open NUnit.Framework
open smoothdev.FSharp.Reflection

type Tags = A | B | C | D

type reflect<'T> when 'T : equality = TagUnionCaseReflection<'T>

[<Test>]
let ``can reflect Tag type discriminated union`` () =  
  let cases = reflect<Tags>.AllCases
  let expected = [ A; B; C; D ]
  let actual = cases |> Seq.map (fun c -> c.Value)
  expected |> should equal actual
  assert ((Seq.length actual) = 4)

[<Test>]
let ``can get name``() = 
  let actual = reflect<Tags>.AllCases |> Seq.map (fun c -> reflect<Tags>.GetName(c.Value))
  let expected = [ "A"; "B"; "C"; "D" ]
  expected |> should equal actual

[<Test>]
let ``try get name`` () = 
  let caseA = reflect<Tags>.TryGet "a" true
  assert (caseA = Some A)
  let caseA = reflect<Tags>.TryGet "a" false
  assert (caseA = None)
