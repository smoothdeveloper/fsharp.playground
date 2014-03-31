// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#r "../SampleGeneratedTypesTypeProvider/bin/Debug/SampleGeneratedTypesTypeProvider.dll"
#load "UseProvider.fs"
open UseGenerativeTypesTypeProvider

let foo = new TheNamespace.FooModule()
foo.GetIdentifier()
// Define your library scripting code here
