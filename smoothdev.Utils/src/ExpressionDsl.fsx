#load "ExpressionDsl.fs"
open System
open System.Linq
open ExpressionDsl

//module DataViewExpressionDsl =
let (.<=.) a b = LessOrEqual { left = a; right = b }
let (.>.) a b = MoreThan { left = a; right = b }
let (.=.) a b = Equal { left = a; right = b }
let (!.) a = Not a
let (.&&.) a b = Compound (And { left = a; right = b })
let (.||.) a b = Compound (Or  { left = a; right = b })

let rec printValue value =
  match value with
  | StringValue   value -> value.Replace("'", "''") |> sprintf "'%s''"
  | DateTimeValue value -> string value             |> sprintf "'%s'"
  | IntValue      value -> value                    |> sprintf "%i"
  | LongValue     value -> value                    |> sprintf "%i"
  | SingleValue   value -> value                    |> sprintf "%f"
  | DoubleValue   value -> value                    |> sprintf "%f"
  | BooleanValue  value -> value                    |> sprintf "%b"
  | DecimalValue  value -> value                    |> sprintf "%M"

and printExpression expr =
  match expr with
  | ColumnName name -> name
  | Constant value -> printValue value
and printOperands printer (operands: Operands) =
  printer (printExpression operands.left) (printExpression operands.right)
and printWildCardParts parts =
  let mutable hasWildCard = false
  let parts = 
    [|
    for p in parts do
      yield!
        match p with
        | WildcardChar 
        | WildcardString ->
          hasWildCard <- true
          Wildcard |> Seq.singleton 
        | Literal l ->
          l |> Seq.map Char
    |]
  if hasWildCard then
    // need escaping
    // todo: throw if wild card is not at either or both at start/end but in the middle
    [|
    for p in parts do
      yield
        match p with
        | Wildcard  -> "*"
        | Char '['  -> "[[]"
        | Char ']'  -> "[]]"
        | Char '*'  -> "[*]"
        | Char '%'  -> "[%]"
        | Char '\'' -> "''"
        | Char c    -> string c
    |]
    |> String.concat ""
  else
    parts 
    |> Array.map (function | Char c -> c | _ -> failwithf "not happening")
    |> String
    
and printBooleanExpr =
  function
  | LessThan    ops -> printOperands (sprintf "%s < %s") ops
  | MoreThan    ops -> printOperands (sprintf "%s > %s") ops
  | LessOrEqual ops -> printOperands (sprintf "%s <= %s") ops
  | Equal       ops -> printOperands (sprintf "%s = %s") ops
  | Like (e, parts) -> sprintf "%s like '%s'" (printExpression e) (printWildCardParts parts)
  | Compound o -> sprintf "(%s)" (printLogicOp o)
  | EndsWith   (e, s) -> Like (e, [WildcardString;Literal s]) |> printBooleanExpr
  | BeginsWith (e, s) -> Like (e, [Literal s;WildcardString]) |> printBooleanExpr
  | Contains   (e, s) -> Like (e, [WildcardString;Literal s;WildcardString]) |> printBooleanExpr
  | Not e             -> sprintf "not (%s)" (printBooleanExpr e)

and printLogicOp =
  function
  | And ops -> sprintf "%s and %s" (printBooleanExpr ops.left) (printBooleanExpr ops.right)
  | Or  ops -> sprintf "%s or %s" (printBooleanExpr ops.left) (printBooleanExpr ops.right)
and print (expr: obj) =
  match expr with
  | :? BooleanExpression   as e    -> printBooleanExpr e
  | :? Expression          as e    -> printExpression e
  | :? LogicOperator       as e    -> printLogicOp e
  | :? LiteralValue        as e    -> printValue e
  | :? (WildCardParts seq) as like -> printWildCardParts like

let expr =
  C "timestamp" .<=. V DateTime.Now
  .&&. (C "timestamp" .>. V (DateTime.Now.AddDays(-1.)))
  .&&. (Like (C "foo", [WildcardString ; Literal "foo"]))
  .&&. (
    (BeginsWith (C "baz", "qux"))
    .||. (BeginsWith (C "baz", "*"))
  )
  .&&. (!. (C "foo" .>. V 1))
  .||. (C "t" .=. V true)

print expr
#load "../../paket-files/include-scripts/net46/include.fscheck.fsx"
open System
open System.Linq
open FsCheck.Arb
open System.Data
open FsCheck

let generatedt columns rows =
  let table = new DataTable()
  
  let arbStringGen =
      Arb.from<string>.Generator
  let arbStringColumnGen =
    arbStringGen
          .Where(fun s -> not (isNull s))
          .Where(fun s -> table.Columns.Contains s |> not)
          .Where(fun s -> s.Length > 0)
  let getSample gen = Gen.sample 10000 1 gen |> Seq.head
  let validTypesAndArb = 
    [|
    typeof<string>      , fun () -> box <| getSample Arb.from<string>.Generator
    typeof<int>         , fun () -> box <| getSample Arb.from<int>.Generator
    (*typeof<float>
    typeof<float32>
    typeof<int>
    typeof<int64>
    typeof<decimal>
    typeof<bool>
    typeof<DateTime>*)
    |]
    |> dict
  let validTypes = validTypesAndArb.Keys
  gen {
    
    for i in [1..columns] do
      let! columnType = Gen.elements validTypes
      let! columnName =
        Arb.from<string>
          .Generator
          .Where(fun s -> not (isNull s))
          .Where(fun s -> table.Columns.Contains s |> not)
          .Where(fun s -> s.Length > 0)
      table.Columns.Add(columnName, columnType) |> ignore
    for i in [1..rows] do
      [|
      for c in table.Columns.Cast<DataColumn>() do
        yield validTypesAndArb.[c.DataType] ()
      |]
      |> table.Rows.Add
      |> ignore
    return table
  }


let table = generatedt 30 30 |> Gen.sample 1 1 |> Seq.head

for c in table.Columns do
  printfn "%s %s" c.ColumnName c.DataType.Name
for r in table.Rows do
  printfn "%A" r.ItemArray

let t = generatedt 100
