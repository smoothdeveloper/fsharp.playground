namespace smoothdev.FSharp.Reflection
open global.Microsoft.FSharp.Reflection
module Option =
  let private getOptionType t =
      let optionGenericType = typedefof<option<_>>
      optionGenericType.MakeGenericType([|t|])

  let toOptionNone t =
    // remark could be replaced with
    // toOptionNone t :obj = null 
    let optionType = getOptionType t
    let cases = FSharpType.GetUnionCases(optionType)
    let noneFsharpType = cases |> Seq.find(fun i -> i.Name = "None")
    FSharpValue.MakeUnion(noneFsharpType, [||])
  
  let toOptionSomeFromType t v =
    let optionType = getOptionType (t)
    let cases = FSharpType.GetUnionCases(optionType)
    let someFsharpType = cases |> Seq.find(fun i -> i.Name = "Some")
    FSharpValue.MakeUnion(someFsharpType, [|v|])

  let toOptionSome v =
    toOptionSomeFromType (v.GetType()) v