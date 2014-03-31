namespace SampleGenerativeTypesTypeProvider.Provider

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

type ModuleDefinition = {Identifier: string; Name: string; Documentation: string}

[<TypeProvider>]
type SampleDefinitionTypeProvider() as this = 
    inherit TypeProviderForNamespaces()

    let typeProviderNamespace = "Samples.TypeProviders.GenerativeTypes"
    let typeProviderAssembly = Assembly.GetExecutingAssembly()


    (*
    in a real provider, this would be data gathered dynamically when instanciating this provider 
    with static parameters
    *)
    let modules = [
      {Identifier = "foo"; Name = "FooModule"; Documentation = "This is module Foo"}
      ; {Identifier = "bar"; Name = "BarModule"; Documentation = "This is module Bar"}
    ]

    let thisProvider = ProvidedTypeDefinition(typeProviderAssembly, typeProviderNamespace, "ParametizedSampleDefinitionTypeProvider", None)

    let createModuleDefinitionType namespaceForModuleDefinition moduleDefinition =
      let providedModuleDefinitionType = ProvidedTypeDefinition(typeProviderAssembly, namespaceForModuleDefinition, moduleDefinition.Name, None)
      providedModuleDefinitionType.IsErased <- false
      
      let ctor = ProvidedConstructor(
                    parameters = []
                    , InvokeCode = fun args -> <@@ Console.WriteLine("constructor of module {0}", moduleDefinition.Identifier) @@>
                    )
      providedModuleDefinitionType.AddMember ctor
      
      let getIdentifierMethod = ProvidedMethod(
                                  "GetIdentifier"
                                  , parameters = []
                                  , returnType = typeof<string>
                                  , InvokeCode = fun args -> <@@ moduleDefinition.Identifier @@>
                                  )

      providedModuleDefinitionType.AddMember getIdentifierMethod

      providedModuleDefinitionType

    let thisProviderParameters = [
      ProvidedStaticParameter("Namespace", typeof<string>)
      // could add more/define other static parameters, to read definitions somewhere, etc.
    ]
    do thisProvider.DefineStaticParameters(
                      thisProviderParameters
                      , fun typeName args ->
                           // have to generate types here

                           let namespaceToGenerateTypesIn = args.[0] :?> string // get the first parameter, refer to thisProviderParameters above
           
                           // the provider itself is exposed in typeProviderNamespace
                           let provider = ProvidedTypeDefinition(typeProviderAssembly, typeProviderNamespace, typeName, None)

                           provider.IsErased <- false

                           let providedAssembly = new ProvidedAssembly(System.IO.Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".dll"))

                           // generate a simple type for each Module definition
                           modules
                            |> Seq.map (createModuleDefinitionType namespaceToGenerateTypesIn)
                            |> List.ofSeq
                            |> providedAssembly.AddTypes

                           provider
                    )
    do
      // finally, expose the dynamic type provider
      thisProvider.IsErased <- false
      this.AddNamespace(typeProviderNamespace, [thisProvider])

[<assembly:TypeProviderAssembly>]
do()