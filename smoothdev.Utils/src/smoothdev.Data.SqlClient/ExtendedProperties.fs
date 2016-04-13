namespace smoothdev.Data.SqlClient

open System.Data.SqlClient
open System.Data

type CreateCommand = unit -> IDbCommand

type ExtendedPropertyContainer =
| Aggregate
| AsymmetricKey
| Assembly
| Certificate
| Contract
| Database
| Default
| EventNotification
| Filegroup 
| FileName
| Function
| MessageType
| PartitionFunction
| PartitionScheme
| PlanGuide
| Procedure
| Queue
| RemoteServiceBinding
| Route
| Rule
| Schema 
| User
| Service
| SymmetricKey
| Synonym
| Table
| Trigger
| Type
| View
| XMLSchemaCollection

type ExtendedPropertyContainerLocator = ExtendedPropertyContainer * string

module Extensions =
  type IDbCommand
    with
      member x.SetParameters (parameters :(string * obj) seq) =
        for name, value in parameters do
          let dbParam = 
            if x.Parameters.Contains(name) then 
              x.Parameters.[name] :?> IDbDataParameter
            else 
              let p = x.CreateParameter()
              x.Parameters.Add p |> ignore
              p.ParameterName <- name
              p
          dbParam.Value <- value

module ExtendedProperties =
  open Extensions
  let setProperty (createCommand: CreateCommand) (locator: ExtendedPropertyContainerLocator) propertyName propertyValue =
    let command = createCommand ()
    
    

    match locator with
    | Database, dbName ->
        
      [ "databaseName" , box dbName
        "propertyName" , box propertyName
        "propertyValue", box propertyValue
      ]
      |> command.SetParameters
 
      command.CommandText <- @"
if exists(select * from sys.extended_properties where class_desc = 'database' and name = @propertyName)
begin
  exec sys.sp_updateextendedproperty @name = @propertyName, @value = @propertyValue
end
else
begin
  exec sys.sp_addextendedproperty @name = @propertyName, @value = @propertyValue
end
"
    | _ -> failwithf "not implemented %A" locator

    command.ExecuteNonQuery()
    |> ignore

  let getProperty (createCommand: CreateCommand) (locator: ExtendedPropertyContainerLocator) propertyName =
    let command = createCommand ()

    match locator with
    | Database, dbName ->
      [ "databaseName" , box dbName
        "propertyName" , box propertyName
      ]
      |> command.SetParameters

      command.CommandText <- @"
if exists(select * from sys.extended_properties where class_desc = 'database' and name = @propertyName)
begin
  select [value] from sys.extended_properties where class_desc = 'database' and name = @propertyName
end
"
    let result = command.ExecuteScalar()
    if isNull result then None else Some result
