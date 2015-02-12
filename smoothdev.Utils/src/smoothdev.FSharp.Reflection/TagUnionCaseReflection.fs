namespace smoothdev.FSharp.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Reflection

type TagUnionCaseReflection<'T> when 'T : equality =
  { CaseInfo: UnionCaseInfo
  ; Value: 'T
  }
  with
    static member unionCasesLookup =
      let dic = new Dictionary<_,_>()
      FSharpType.GetUnionCases(typeof<'T>)
      |> Seq.map (fun t -> (TagUnionCaseReflection<'T>.construct t, TagUnionCaseReflection<'T>.makeCase t))
      |> Seq.iter dic.Add
      dic

    // will crash if 'T contains members which aren't only tags
    static member construct (caseInfo: UnionCaseInfo)    = FSharpValue.MakeUnion(caseInfo, [||]) :?> 'T
    static member makeCase (caseInfo: UnionCaseInfo)     = {CaseInfo = caseInfo; Value = TagUnionCaseReflection<'T>.construct caseInfo}
    static member GetName (tag: 'T)                      = TagUnionCaseReflection.unionCasesLookup.[tag].CaseInfo.Name
    static member AllCases                               = TagUnionCaseReflection<'T>.unionCasesLookup.Values :> seq<_>
      
