module Control

open System

/// executes f and returns time spent executing it
let time f =
  let sw = System.Diagnostics.Stopwatch()
  sw.Start()
  f ()
  sw.Stop()
  sw.Elapsed

/// initializes a resource in first, pass it to inBetween and last, 
/// and if the resource is an IDisposable, disposes it (no need to do so in last)
let bracket first last inBetween =
   // based on https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Exception.html#v:bracket
  let realResource = first ()
  let disp (o: obj) = 
    match o with
    | :? IDisposable as x -> x
    | _ -> null
  use resource = disp realResource
  try
    inBetween realResource
  finally
    last realResource
