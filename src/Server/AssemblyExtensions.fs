[<AutoOpen>]
module AssemblyExtensions

open FileSystem

open System
open System.IO
open System.Reflection

type Assembly with
    static member ExecutingDir =
        let dir = Path.GetDirectoryName(Uri(Assembly.GetCallingAssembly().CodeBase).AbsolutePath)
        DirPath.New(dir)