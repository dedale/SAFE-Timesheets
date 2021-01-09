module Pages.Team

open Shared

open Elmish
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Router
open Fetch.Types
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type State = unit

type Msg = unit

let init (user: LoggedUser) =
    (), Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    state, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        Html.p "Team page"
    ]
