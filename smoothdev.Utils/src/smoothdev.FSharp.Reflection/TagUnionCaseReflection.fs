namespace smoothdev.FSharp.Reflection
open System
open System.Collections.Generic
open FSharp.Reflection

type TagUnionCaseReflection<'T> when 'T : equality =
  { CaseInfo: UnionCaseInfo
  ; Value: 'T
  }
  with
    static member unionCasesLookup =
      FSharpType.GetUnionCases(typeof<'T>)
      |> Seq.map TagUnionCaseReflection<'T>.makeCase
      |> Seq.map (fun c -> c.Value, c)
      |> dict

    // will crash if 'T contains members which aren't only tags
    static member internal construct (caseInfo: UnionCaseInfo) = FSharpValue.MakeUnion(caseInfo, [||]) :?> 'T
    static member internal makeCase (caseInfo: UnionCaseInfo)  = {CaseInfo = caseInfo; Value = TagUnionCaseReflection<'T>.construct caseInfo}
    static member GetName (tag: 'T)                            = TagUnionCaseReflection.unionCasesLookup.[tag].CaseInfo.Name
    static member AllCases                                     = TagUnionCaseReflection<'T>.unionCasesLookup.Values :> seq<_>
    static member AllValues                                    = TagUnionCaseReflection<'T>.unionCasesLookup.Values |> Seq.map (fun v -> v.Value)
    static member TryGet (name: string) (ignoreCase) =  
      let comparison =
        if ignoreCase then StringComparison.CurrentCultureIgnoreCase
        else StringComparison.CurrentCulture
      let caseInfo = 
        TagUnionCaseReflection<'T>.unionCasesLookup.Values
        |> Seq.tryFind (fun c -> System.String.Compare(c.CaseInfo.Name, name, comparison) = 0)
      match caseInfo with
      | Some case -> Some case.Value
      | _ -> None
