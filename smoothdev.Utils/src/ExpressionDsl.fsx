open System

type Expression =
| ColumnName of string
| Constant   of LiteralValue
and LiteralValue =
| StringValue   of string
| IntValue      of int
| LongValue     of int64
| DoubleValue   of float
| SingleValue   of single
| DecimalValue  of decimal
| DateTimeValue of DateTime
| BooleanValue  of bool

type LiteralValueBuilderTypeClass private () =
  [<DefaultValue>]
  static val mutable private _resolve : LiteralValueBuilderTypeClass
  static member inline internal invoke x =
    let inline call (_ : ^a, x : ^b) = ((^a or ^b) : (static member makeLiteralValue : ^b -> LiteralValue) x)
    call (LiteralValueBuilderTypeClass._resolve, x)
  
  static member inline makeLiteralValue (x : string)   = StringValue x
  static member inline makeLiteralValue (x : int)      = IntValue x
  static member inline makeLiteralValue (x : DateTime) = DateTimeValue x
  static member inline makeLiteralValue (x : decimal)  = DecimalValue x
  static member inline makeLiteralValue (x : bool)     = BooleanValue x


type WildCardParts =
| Literal of string
| WildcardChar
| WildcardString

type LeftAndRight<'t> = { left: 't; right: 't }

type BooleanExpression =
| LessOrEqual of Operands
| LessThan    of Operands
| MoreThan    of Operands
| Equal       of Operands
| Like        of Expression * WildCardParts seq
| BeginsWith  of Expression * string
| EndsWith    of Expression * string
| Contains    of Expression * string
| Compound    of LogicOperator
| Not         of BooleanExpression
and LogicOperator =
| And of BooleanExpressions
| Or  of BooleanExpressions


and Operands = LeftAndRight<Expression>
and BooleanExpressions = LeftAndRight<BooleanExpression>

#nowarn "86" // operators
// those are abusive for now, will decide later how they'll look like
let (<=) a b = LessOrEqual { left = a; right = b }
let (>) a b = MoreThan { left = a; right = b }
let (=) a b = Equal { left = a; right = b }
let (!) a = Not a
let (&&) a b = Compound (And { left = a; right = b })
let (||) a b = Compound (Or  { left = a; right = b })

type LikeExpressionPart =
| Wildcard
| Char of char

let rec printValue value =
  match value with
  | StringValue   value -> value.Replace("'", "''") |> sprintf "'%s''"
  | DateTimeValue value -> string value             |> sprintf "'%s'"
  | IntValue      value -> value                    |> sprintf "%i"
  | BooleanValue  value -> value                    |> sprintf "%b"
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

let C name = ColumnName name
let inline V x = Constant (LiteralValueBuilderTypeClass.invoke x)

let expr =
  C "timestamp" <= V DateTime.Now
  && C "timestamp" > V (DateTime.Now.AddDays(-1.))
  && (Like (C "foo", [WildcardString ; Literal "foo"]))
  && (
    (BeginsWith (C "baz", "qux"))
    || (BeginsWith (C "baz", "*"))
  )
  && (! (C "foo" > V 1))
  || C "t" = V true

print expr
