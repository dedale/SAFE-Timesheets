module Pages.Admin

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

open System

type AddUserResult = Result<User, string>

type DelUserResult = Result<bool, string>

type AddTeamResult = Result<Team, string>

type DelTeamResult = Result<bool, string>

type Tab =
    | Users
    | Teams

type State =
    { User : LoggedUser
      Tab : Tab
      NewUsername : string
      Users : User list
      TryAddUser : Deferred<AddUserResult>
      TryDelUser : Deferred<DelUserResult>
      NewTeamName : string
      NewTeamManagerId : UserId
      Teams : Team list
      TryAddTeam : Deferred<AddTeamResult>
      TryDelTeam : Deferred<DelTeamResult>
    }

type Msg =
    | TabChanged of Tab
    | NewUsernameChanged of string
    | AddUserClicked of PromiseStatus<AddUserResult>
    | DelUserClicked of UserId * PromiseStatus<DelUserResult>
    | NewTeamNameChanged of string
    | NewTeamManagerChanged of UserId
    | AddTeamClicked of PromiseStatus<AddTeamResult>
    | DelTeamClicked of TeamId * PromiseStatus<DelTeamResult>

let (|UserAdded|_|) = function
    | AddUserClicked (Completed (Ok user)) -> Some user
    | _ -> None

let (|TeamAdded|_|) = function
    | AddTeamClicked (Completed (Ok team)) -> Some team
    | _ -> None

let dummyUsers =
    let create id login name =
        match UserLogin.create login with
        | Ok l ->
            { User.Id = UserId id
              Login = l
              Name = name }
        | Error m -> failwith m
    [ create 1 "jdoe123" "John DOE"
      create 2 "vphilipp123456" "Vianney PHILIPPE"
    ]

let addUser (username: string) = promise {
    let body = Encode.Auto.toString(0, username)
    let props = [
        Method HttpMethod.POST
        Fetch.requestHeaders [ ContentType "application/json" ]
        Body !^body
    ]
    let! res = Fetch.fetch Route.user props
    let! txt = res.text()
    return Decode.Auto.unsafeFromString<User> txt |> (Ok >> Completed)
}

let delUser (id: UserId) = promise {
    let props = [
        Method HttpMethod.DELETE
    ]
    let! res = Fetch.fetch (UserId.route id) props
    return res.Ok |> (Ok >> Completed)
}

let addTeam (name: string) (managerId: UserId) = promise {
    let body = Encode.Auto.toString(0, (name, managerId))
    let props = [
        Method HttpMethod.POST
        Fetch.requestHeaders [ ContentType "application/json" ]
        Body !^body
    ]
    let! res = Fetch.fetch Route.team props
    let! txt = res.text()
    return Decode.Auto.unsafeFromString<Team> txt |> (Ok >> Completed)
}

let delTeam (id: TeamId) = promise {
    let props = [
        Method HttpMethod.DELETE
    ]
    let! res = Fetch.fetch (TeamId.route id) props
    return res.Ok |> (Ok >> Completed)
}

let init (user: LoggedUser) =
    { User = user
      Tab = Tab.Users
      NewUsername = ""
      Users = dummyUsers
      TryAddUser = NotStarted
      TryDelUser = NotStarted
      NewTeamName = ""
      NewTeamManagerId = (List.head dummyUsers).Id
      Teams = [ { Id = TeamId 1; Name = "Cloud Ops" } ]
      TryAddTeam = NotStarted
      TryDelTeam = NotStarted
    }, Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | TabChanged newTab ->
        { state with Tab = newTab }, Cmd.none

    | NewUsernameChanged user ->
        { state with NewUsername = user }, Cmd.none

    | AddUserClicked Pending ->
        let nextState = { state with TryAddUser = InProgress }
        let OnFailed (e: exn) = Error e.Message |> (Completed >> AddUserClicked)
        let nextCmd = Cmd.OfPromise.either addUser state.NewUsername AddUserClicked OnFailed
        nextState, nextCmd

    | AddUserClicked (Completed addResult) ->
        let nextState = { state with TryAddUser = Resolved addResult }
        match addResult with
        | Ok user ->
            { nextState with Users = user :: nextState.Users; NewUsername = "" }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | DelUserClicked (id, Pending) ->
        let toMsg x = DelUserClicked (id, x)
        let nextState = { state with TryDelUser = InProgress }
        let OnFailed (e: exn) = Error e.Message |> Completed |> toMsg
        let nextCmd = Cmd.OfPromise.either delUser id toMsg OnFailed
        nextState, nextCmd

    | DelUserClicked (id, Completed delResult) ->
        let nextState = { state with TryDelUser = Resolved delResult }
        match delResult with
        | Ok deleted ->
            if deleted then
                { nextState with Users = nextState.Users |> List.where (fun user -> user.Id <> id) }, Cmd.none
            else
                nextState, Cmd.none
        | _ ->
            nextState, Cmd.none

    | NewTeamNameChanged team ->
        { state with NewTeamName = team }, Cmd.none

    | NewTeamManagerChanged id ->
        { state with NewTeamManagerId = id }, Cmd.none

    | AddTeamClicked Pending ->
        let nextState = { state with TryAddTeam = InProgress }
        let OnFailed (e: exn) = Error e.Message |> (Completed >> AddTeamClicked)
        let nextCmd = Cmd.OfPromise.either (uncurry addTeam) (state.NewTeamName, state.NewTeamManagerId) AddTeamClicked OnFailed
        nextState, nextCmd

    | AddTeamClicked (Completed addResult) ->
        let nextState = { state with TryAddTeam = Resolved addResult }
        match addResult with
        | Ok team ->
            { nextState with Teams = team :: nextState.Teams; NewTeamName = "" }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | DelTeamClicked (id, Pending) ->
        let toMsg x = DelTeamClicked (id, x)
        let nextState = { state with TryDelTeam = InProgress }
        let OnFailed (e: exn) = Error e.Message |> Completed |> toMsg
        let nextCmd = Cmd.OfPromise.either delTeam id toMsg OnFailed
        nextState, nextCmd

    | DelTeamClicked (id, Completed delResult) ->
        let nextState = { state with TryDelTeam = Resolved delResult }
        match delResult with
        | Ok deleted ->
            if deleted then
                { nextState with Teams = nextState.Teams |> List.where (fun team -> team.Id <> id) }, Cmd.none
            else
                nextState, Cmd.none
        | _ ->
            nextState, Cmd.none

let centered (children: ReactElement list) =
    Html.div [
        prop.style [
            style.margin.auto
            style.textAlign.center
            style.padding 20
            style.width (length.percent 100)
        ]
        prop.children children
    ]

let centerColumn (elements : ReactElement list) =
    Bulma.columns [
        Bulma.column []
        Bulma.column elements
        Bulma.column []
    ]

let renderUser (state: State) (dispatch: Msg -> unit) (user: User) =
    Bulma.columns [
        columns.isVCentered
        prop.children [
            Bulma.column [
                column.isOneThird
                prop.children [
                    Bulma.subtitle.p (UserLogin.value user.Login)
                ]
            ]
            Bulma.column [
                Bulma.subtitle.p user.Name
            ]
            Bulma.column [
                column.isNarrow
                prop.children [
                    Bulma.buttons [
                        Bulma.button.button [
                            color.isDanger
                            prop.onClick (fun _ -> (user.Id, Pending) |> DelUserClicked |> dispatch)
                            prop.children [
                                Fa.i [ Fa.Solid.Times ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderAddUser (state: State) (dispatch: Msg -> unit) =
    let error =
        if String.IsNullOrWhiteSpace state.NewUsername
        then Some ""
        elif state.Users |> List.exists (fun user -> UserLogin.value user.Login = state.NewUsername.Trim())
        then Some "User already exists."
        else None
    Html.div [
        prop.style [
            style.paddingBottom 30
        ]
        prop.children [
            Bulma.field.div [
                field.hasAddons
                prop.children [
                    Bulma.control.div [
                        control.hasIconsLeft
                        if Option.isSome error then control.hasIconsRight
                        prop.children [
                            Bulma.input.text [
                                prop.placeholder "Username"
                                prop.valueOrDefault state.NewUsername
                                prop.onChange (NewUsernameChanged >> dispatch)
                            ]
                            Bulma.icon [
                                icon.isSmall; icon.isLeft
                                prop.children [
                                    Fa.i [ Fa.Solid.User ] []
                                ]
                            ]
                            if Option.isSome error then
                                Bulma.icon [
                                    icon.isSmall; icon.isRight
                                    prop.children [
                                        Fa.i [ Fa.Solid.ExclamationTriangle ] []
                                    ]
                                ]
                        ]
                    ]
                    Bulma.control.div [
                        Bulma.button.button [
                            color.isPrimary
                            prop.disabled (Option.isSome error)
                            prop.onClick (fun _ -> dispatch (AddUserClicked Pending))
                            prop.children [
                                Fa.i [ Fa.Solid.Plus ] []
                            ]
                        ]
                    ]
                ]
            ]
            match error with
            | Some msg ->
                if not (String.IsNullOrEmpty msg) then
                    Bulma.help [
                        color.isDanger
                        prop.text msg
                    ]
            | None -> Html.none
        ]
    ]

let renderUsers (state: State) (dispatch: Msg -> unit) =
    Html.div [
        match state.TryAddUser with
        | Resolved (Error m) ->
            Bulma.message [
                color.isDanger
                prop.children [
                    Bulma.messageBody (sprintf "Error adding new user: %s" m)
                ]
            ]
        | _ -> Html.none
        Bulma.box (
            state.Users |> List.map (renderUser state dispatch)
        )
    ]

let renderTeam (state: State) (dispatch: Msg -> unit) (team: Team) =
    Bulma.columns [
        columns.isVCentered
        prop.children [
            Bulma.column [
                Bulma.subtitle.p team.Name
            ]
            Bulma.column [
                column.isNarrow
                prop.children [
                    Bulma.buttons [
                        Bulma.button.button [
                            color.isDanger
                            prop.onClick (fun _ -> (team.Id, Pending) |> DelTeamClicked |> dispatch)
                            prop.children [
                                Fa.i [ Fa.Solid.Times ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderAddTeam (state: State) (dispatch: Msg -> unit) =
    let error =
        if String.IsNullOrWhiteSpace state.NewTeamName
        then Some ""
        elif state.Teams |> List.exists (fun team -> team.Name = state.NewTeamName.Trim())
        then Some "Team already exists."
        else None
    Html.div [
        prop.style [
            style.paddingBottom 30
        ]
        prop.children [
            Bulma.field.div [
                prop.children [
                    Bulma.control.div [
                        control.hasIconsLeft
                        if Option.isSome error then control.hasIconsRight
                        prop.children [
                            Bulma.input.text [
                                prop.placeholder "Team"
                                prop.valueOrDefault state.NewTeamName
                                prop.onChange (NewTeamNameChanged >> dispatch)
                            ]
                            Bulma.icon [
                                icon.isSmall; icon.isLeft
                                prop.children [
                                    Fa.i [ Fa.Solid.Users ] []
                                ]
                            ]
                            if Option.isSome error then
                                Bulma.icon [
                                    icon.isSmall; icon.isRight
                                    prop.children [
                                        Fa.i [ Fa.Solid.ExclamationTriangle ] []
                                    ]
                                ]
                        ]
                    ]
                ]
            ]
            Bulma.field.div [
                Bulma.label "Manager"
                Bulma.control.div [
                    control.hasIconsLeft
                    prop.children [
                        Bulma.select (
                            state.Users |> List.map (fun u -> Html.option u.Name)
                        )
                        Bulma.icon [
                            icon.isSmall; icon.isLeft
                            prop.children [
                                Fa.i [ Fa.Solid.User ] []
                            ]
                        ]
                    ]
                ]
            ]
            Bulma.field.div [
                Bulma.button.button [
                    color.isInfo
                    prop.disabled (Option.isSome error)
                    if state.TryAddTeam = InProgress then button.isLoading
                    prop.onClick (fun _ -> dispatch (AddTeamClicked Pending))
                    prop.text "Add Team"
                ]
            ]
            match error with
            | Some msg ->
                if not (String.IsNullOrEmpty msg) then
                    Bulma.help [
                        color.isDanger
                        prop.text msg
                    ]
            | None -> Html.none
        ]
    ]

let renderTeams (state: State) (dispatch: Msg -> unit) =
    Html.div [
        match state.TryAddTeam with
        | Resolved (Error m) ->
            Bulma.message [
                color.isDanger
                prop.children [
                    Bulma.messageBody (sprintf "Error adding new team: %s" m)
                ]
            ]
        | _ -> Html.none
        Bulma.box (
            state.Teams |> List.map (renderTeam state dispatch)
        )
    ]

let render state dispatch =
    centered [
        Html.h1 [
            Html.strong ((UserLogin.value state.User.Username).ToUpper())
        ]

        Html.p "This is the admin page"

        Html.div [
            Bulma.button.a [
                color.isInfo
                prop.style [ style.margin 5 ]
                prop.href (Router.format(""))
                prop.text "Home"
            ]
            Bulma.button.a [
                color.isInfo
                prop.style [ style.margin 5 ]
                prop.href (Router.format("logout"))
                prop.text "Logout"
            ]
        ]

        Bulma.tabs [
            Html.ul [
                Html.li [
                    if state.Tab = Tab.Users then tab.isActive
                    prop.children [
                        Html.a [
                            if state.Tab <> Tab.Users
                            then prop.onClick (fun _ -> TabChanged Tab.Users |> dispatch)
                            prop.text "Users"
                        ]
                    ]
                ]
                Html.li [
                    if state.Tab = Tab.Teams then tab.isActive
                    prop.children [
                        Html.a [
                            if state.Tab <> Tab.Teams
                            then prop.onClick (fun _ -> TabChanged Tab.Teams |> dispatch)
                            prop.text "Teams"
                        ]
                    ]
                ]
            ]
        ]

        Html.div [
            prop.style [
                style.width 600
                style.textAlign.left
            ]
            prop.children [
                if state.Tab = Tab.Users then
                    renderAddUser state dispatch
                    renderUsers state dispatch
                if state.Tab = Tab.Teams then
                    renderAddTeam state dispatch
                    renderTeams state dispatch
            ]
        ]
    ]