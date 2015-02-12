namespace smoothdev.FSharp.Reflection

open Microsoft.FSharp.Reflection

type TagUnionCaseReflection<'T> =
  { CaseInfo: UnionCaseInfo
  ; Value: 'T
  }
  with
    // will crash if 'T contains members which aren't only tags
    static member construct (caseInfo: UnionCaseInfo)         = FSharpValue.MakeUnion(caseInfo, [||]) :?> 'T
  
    static member AllCases = 
      FSharpType.GetUnionCases(typeof<'T>)
      |> Seq.map (fun info -> {CaseInfo= info; Value = TagUnionCaseReflection<'T>.construct info})
