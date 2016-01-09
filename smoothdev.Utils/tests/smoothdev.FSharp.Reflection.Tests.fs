namespace smoothdev.FSharp.Reflections.Tests

open FsUnit
open NUnit.Framework
open smoothdev.FSharp.Reflection

module ``TagUnionCaseReflection tests`` =

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

module ``Option tests`` =

  [<Test>]
  let ``toOptionSome of boxed value returns expected type`` () =
    let value = box 3
    let expected = Some 3
    let actual = Option.toOptionSome value
    expected |> should equal actual

  [<Test>]
  let ``toOptionNone of runtime type gives expected None`` () =
    let expected : int option = None
    let actual = Option.toOptionNone typeof<int>
    expected |> should equal actual