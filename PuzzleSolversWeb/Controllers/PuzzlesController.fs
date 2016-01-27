namespace PuzzleSolversWeb.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Web
open System.Web.Mvc
open System.Web.Mvc.Ajax

type PuzzlesController() =
    inherit Controller()
    member this.Gchq () = this.View()

