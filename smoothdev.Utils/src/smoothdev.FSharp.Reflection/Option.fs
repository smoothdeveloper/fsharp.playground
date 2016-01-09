module Option

let private getOptionType t =
    let optionGenericType = typedefof<FSharp.Core.option<_>>
    optionGenericType.MakeGenericType([|t|])

let toOptionNone t =
  // remark could be replaced with
  // toOptionNone t :obj = null 
  let optionType = getOptionType t
  let cases = FSharp.Reflection.FSharpType.GetUnionCases(optionType)
  let noneFsharpType = cases |> Seq.find(fun i -> i.Name = "None")
  FSharp.Reflection.FSharpValue.MakeUnion(noneFsharpType, [||])
  
let toOptionSomeFromType t v =
  let optionType = getOptionType (t)
  let cases = FSharp.Reflection.FSharpType.GetUnionCases(optionType)
  let someFsharpType = cases |> Seq.find(fun i -> i.Name = "Some")
  FSharp.Reflection.FSharpValue.MakeUnion(someFsharpType, [|v|])

let toOptionSome v =
  toOptionSomeFromType (v.GetType()) v