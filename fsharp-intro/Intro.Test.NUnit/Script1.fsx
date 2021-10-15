#r "nuget:FsCheck"
#r "bin/Debug/net48/Intro.Test.NUnit.dll"

open FsCheck.Util
open Intro

quick ISet.Test.generatorValid

quick ISet.Test.unionCorrect

quick ISet.Test.unionValid
