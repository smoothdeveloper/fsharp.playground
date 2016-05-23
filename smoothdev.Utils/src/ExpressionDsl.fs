module ExpressionDsl

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
type LikeExpressionPart =
| Wildcard
| Char of char

let C name = ColumnName name
#if USE_SRTP
// SRTP is known to be slower to compile, but brings static checking
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

let inline V x = Constant (LiteralValueBuilderTypeClass.invoke x)
#else
let inline V (x: obj) =
  match x with
  | :? string   as x -> StringValue x
  | :? int      as x -> IntValue x
  | :? int64    as x -> LongValue x
  | :? DateTime as x -> DateTimeValue x
  | :? float    as x -> DoubleValue x
  | :? float32  as x -> SingleValue x
  | :? decimal  as x -> DecimalValue x
  | :? bool     as x -> BooleanValue x
  | _ -> failwithf "%s type not handled" (x.GetType().Name)
  |> Constant
#endif