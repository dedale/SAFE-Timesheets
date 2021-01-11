module Pages.Home

open Client.Domain

open Shared

open Elmish
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

open System

type LoadWeeksResult = Result<MonthWeeks, string>

type State =
    { User : ApplicationUser
      Year : YearNumber
      Weeks : MonthWeeks option
      TryLoadWeeks : Deferred<LoadWeeksResult> }

type Msg =
    | LoadWeeks of PromiseStatus<LoadWeeksResult>
    | ChangeYearClicked of YearNumber

let loadWeeks year = promise {
    let props = [
        Method HttpMethod.GET
        Fetch.requestHeaders [ ContentType "application/json" ]
    ]
    let! res = Fetch.fetch (YearNumber.route year) props
    let! txt = res.text()
    return Decode.Auto.unsafeFromString<MonthWeeks> txt |> Ok
}

let onFailed (e: exn) = Error e.Message

let init user =
    let state =
        { User = user
          Year = YearNumber.current
          Weeks = None
          TryLoadWeeks = NotStarted }
    state, Cmd.ofMsg (ChangeYearClicked state.Year)

let update (msg: Msg) (state: State) =
    match msg with
    | LoadWeeks (Completed result) ->
        let nextState = { state with TryLoadWeeks = Resolved result }
        match result with
        | Ok weeks ->
            { nextState with Weeks = Some weeks }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | LoadWeeks _ -> state, Cmd.none

    | ChangeYearClicked newYear ->
        let nextState = { state with Year = newYear; TryLoadWeeks = InProgress }
        let nextCmd =
            Cmd.OfPromise.either loadWeeks nextState.Year id onFailed
            |> Cmd.map (Completed >> LoadWeeks)
        nextState, nextCmd

// TODO From server to use locale?
let monthPrefixes = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let renderMenu (state: State) (dispatch: Msg -> unit) =
    match state.User with
    | Anonymous ->
        Html.div [
            Html.h1 "Welcome, guest"
            Bulma.button.a [
                color.isInfo
                prop.style [ style.margin 5 ]
                prop.href (Router.format("login"))
                prop.text "Login"
            ]
        ]

    | LoggedIn user ->
        Html.div [
            Html.h1 (sprintf "Welcome, %s" user.Username.Value)
            if user.IsAdmin then
                Bulma.button.a [
                    color.isInfo
                    prop.style [ style.margin 5 ]
                    prop.href (Router.format("admin"))
                    prop.text "Admin"
                ]
            if not user.ManagedTeams.IsEmpty then
                Bulma.button.a [
                    color.isInfo
                    prop.style [ style.margin 5 ]
                    prop.href (Router.format("team"))
                    prop.text "Team"
                ]
            Bulma.button.a [
                color.isInfo
                prop.style [ style.margin 5 ]
                prop.href (Router.format("logout"))
                prop.text "Logout"
            ]
        ]

let centerColumn (element : ReactElement) =
    Bulma.columns [
        Bulma.column []
        Bulma.column [ element ]
        Bulma.column []
    ]

let centerTable (element : ReactElement) =
    Html.table [
        prop.style [ style.margin.auto ]
        prop.children [
            Html.tbody [
                Html.tr [
                    Html.td [
                        element
                    ]
                ]
            ]
        ]
    ]

let renderYearNavigation (state: State) (dispatch: Msg -> unit) =
    Html.div [
        spacing.py3
        prop.children [
            match YearNumber.create (state.Year.Value - 1) with
            | Ok previous ->
                Bulma.button.button [
                    //prop.style [ style.marginLeft length.auto ]
                    color.isLight; color.hasTextLink; button.isLarge
                    if state.TryLoadWeeks = InProgress
                    then button.isLoading
                    prop.onClick (fun _ -> ChangeYearClicked previous |> dispatch)
                    prop.children [
                        Bulma.icon [
                            Fa.i [ Fa.Solid.CaretLeft ] []
                        ]
                        Html.span (previous.Value.ToString())
                    ]
                ]
            | _ -> Html.none
            Bulma.button.button [
                button.isStatic; button.isLarge; spacing.mx3
                prop.text (state.Year.Value.ToString())
            ]
            match YearNumber.create (state.Year.Value + 1) with
            | Ok next ->
                Bulma.button.button [
                    //prop.style [ style.marginRight length.auto ]
                    color.isLight; color.hasTextLink; button.isLarge
                    if state.TryLoadWeeks = InProgress
                    then button.isLoading
                    prop.onClick (fun _ -> ChangeYearClicked next |> dispatch)
                    prop.children [
                        Html.span (next.Value.ToString())
                        Bulma.icon [
                            Fa.i [ Fa.Solid.CaretRight ] []
                        ]
                    ]
                ]
            | _ -> Html.none
        ]
    ] |> centerTable

let renderWeeks (state: State) (dispatch: Msg -> unit) =
    Html.div [
        match state.TryLoadWeeks with
        | Resolved (Error m) ->
            Bulma.message [
                color.isDanger
                prop.children [
                    Bulma.messageBody (sprintf "Error fetching weeks: %s" m)
                ]
            ]
        | _ -> Html.none
        match state.Weeks with
        | Some weeks ->
            Bulma.columns [
                prop.key (sprintf "weeks_%i" state.Year.Value)
                prop.style [ style.textAlign.center ]
                prop.children (
                    MonthWeeks.value weeks
                    |> List.map (fun (month, weeks) ->
                        Bulma.column [
                            spacing.px0
                            prop.children [
                                yield Html.span monthPrefixes.[MonthNumber.value month - 1]
                                for (week, isFull) in weeks do
                                    yield Bulma.button.button [
                                        button.isSmall; spacing.mx1; spacing.my1
                                        match isFull with
                                        | Some b -> color.isSuccess; if not b then color.isLight
                                        | _ -> color.isWhite
                                        prop.key (sprintf "week_%i_%i" state.Year.Value week.Number)
                                        prop.text week.Number
                                    ]
                                    yield Html.none
                            ]
                        ]
                    )
                )
            ] |> centerColumn
        | _ -> Html.none
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        renderMenu state dispatch

        match state.User with
        | LoggedIn _ ->
            renderYearNavigation state dispatch
            renderWeeks state dispatch
        | _ -> Html.none
    ]