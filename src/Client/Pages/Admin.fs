﻿module Pages.Admin

open Client.Domain
open Client.Navigation

open Shared

open Browser.Types
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

type LoadUsersResult = Result<User list, string>
type LoadTeamsResult = Result<Team list, string>
type LoadCostCentersResult = Result<CostCenter list, string>
type LoadTasksResult = Result<Task list, string>

// TODO Add user in team
type AddUserResult = Result<User, string>
type DelUserResult = Result<bool, string>

type AddTeamResult = Result<Team, string>
// TODO Edit
type DelTeamResult = Result<bool, string>

type AddCostCenterResult = Result<CostCenter, string>
// TODO Edit
type DelCostCenterResult = Result<bool, string>

type AddTaskResult = Result<Task, string>
// TODO Edit
type DelTaskResult = Result<bool, string>

type Tab =
    | Users
    | Teams
    | CostCenters
    | Tasks

type State =
    { User : LoggedUser
      Tab : Tab
      
      NewUsername : string
      Users : User list
      TryAddUser : Deferred<AddUserResult>
      TryDelUser : Deferred<DelUserResult>
      
      NewTeamName : string
      NewTeamManagerId : UserId option
      Teams : Team list
      TryAddTeam : Deferred<AddTeamResult>
      TryDelTeam : Deferred<DelTeamResult>
      
      NewCostCenterName : string
      CostCenters : CostCenter list
      TryAddCostCenter : Deferred<AddCostCenterResult>
      TryDelCostCenter : Deferred<DelCostCenterResult>

      NewTaskName : string
      NewTaskCostCenterId : CostCenterId option
      Tasks : Task list
      TryAddTask : Deferred<AddTaskResult>
      TryDelTask : Deferred<DelTaskResult>
    }

type Msg =
    | TabChanged of Tab
    // Init
    | LoadLists
    | LoadUsers of PromiseStatus<LoadUsersResult>
    | LoadTeams of PromiseStatus<LoadTeamsResult>
    | LoadCostCenters of PromiseStatus<LoadCostCentersResult>
    | LoadTasks of PromiseStatus<LoadTasksResult>
    // User
    | NewUsernameChanged of string
    | AddUserClicked of PromiseStatus<AddUserResult>
    | DelUserClicked of UserId * PromiseStatus<DelUserResult>
    // Team
    | NewTeamNameChanged of string
    | NewTeamManagerChanged of UserId
    | AddTeamClicked of PromiseStatus<AddTeamResult>
    | DelTeamClicked of TeamId * PromiseStatus<DelTeamResult>
    // CostCenter
    | NewCostCenterChanged of string
    | AddCostCenterClicked of PromiseStatus<AddCostCenterResult>
    | DelCostCenterClicked of CostCenterId * PromiseStatus<DelCostCenterResult>
    // Task
    | NewTaskNameChanged of string
    | NewTaskCostCenterChanged of CostCenterId
    | AddTaskClicked of PromiseStatus<AddTaskResult>
    | DelTaskClicked of TaskId * PromiseStatus<DelTaskResult>

let (|UserAdded|_|) = function
    | AddUserClicked (Completed (Ok user)) -> Some user
    | _ -> None

let (|TeamAdded|_|) = function
    | AddTeamClicked (Completed (Ok team)) -> Some team
    | _ -> None

let (|CostCenterAdded|_|) = function
    | AddCostCenterClicked (Completed (Ok costCenter)) -> Some costCenter
    | _ -> None

let (|TaskAdded|_|) = function
    | AddTaskClicked (Completed (Ok task)) -> Some task
    | _ -> None

let getAllJson route = promise {
    let props = [
        Method HttpMethod.GET
        Fetch.requestHeaders [ ContentType "application/json" ]
    ]
    let! res = Fetch.fetch route props
    return! res.text()
}

let loadUsers() = promise {
    let! txt = getAllJson Route.users
    return Decode.Auto.unsafeFromString<User list> txt |> Ok
}

let loadTeams() = promise {
    let! txt = getAllJson Route.teams
    return Decode.Auto.unsafeFromString<Team list> txt |> Ok
}

let loadCostCenters() = promise {
    let! txt = getAllJson Route.costCenters
    return Decode.Auto.unsafeFromString<CostCenter list> txt |> Ok
}

let loadTasks() = promise {
    let! txt = getAllJson Route.tasks
    return Decode.Auto.unsafeFromString<Task list> txt |> Ok
}

let addUser (username: string) = promise {
    let body = Encode.Auto.toString(0, username)
    let props = [
        Method HttpMethod.POST
        Fetch.requestHeaders [ ContentType "application/json" ]
        Body !^body
    ]
    let! res = Fetch.fetch Route.users props
    let! txt = res.text()
    return Decode.Auto.unsafeFromString<User> txt |> Ok
}

let delUser userId = promise {
    let props = [
        Method HttpMethod.DELETE
    ]
    let! res = Fetch.fetch (UserId.route userId) props
    return res.Ok |> Ok
}

let addTeam (name: string) (managerId: UserId) = promise {
    let body = Encode.Auto.toString(0, (name, managerId))
    let props = [
        Method HttpMethod.POST
        Fetch.requestHeaders [ ContentType "application/json" ]
        Body !^body
    ]
    let! res = Fetch.fetch Route.teams props
    let! txt = res.text()
    return Decode.Auto.unsafeFromString<Team> txt |> Ok
}

let delTeam teamId = promise {
    let props = [
        Method HttpMethod.DELETE
    ]
    let! res = Fetch.fetch (TeamId.route teamId) props
    return res.Ok |> Ok
}

let addCostCenter (name: string) = promise {
    let body = Encode.Auto.toString(0, name)
    let props = [
        Method HttpMethod.POST
        Fetch.requestHeaders [ ContentType "application/json" ]
        Body !^body
    ]
    let! res = Fetch.fetch Route.costCenters props
    let! txt = res.text()
    return Decode.Auto.unsafeFromString<CostCenter> txt |> Ok
}

let delCostCenter costCenterId = promise {
    let props = [
        Method HttpMethod.DELETE
    ]
    let! res = Fetch.fetch (CostCenterId.route costCenterId) props
    return res.Ok |> Ok
}

let addTask (name: string) (costCenterId: CostCenterId) = promise {
    let body = Encode.Auto.toString(0, (name, costCenterId))
    let props = [
        Method HttpMethod.POST
        Fetch.requestHeaders [ ContentType "application/json" ]
        Body !^body
    ]
    let! res = Fetch.fetch Route.tasks props
    let! txt = res.text()
    return Decode.Auto.unsafeFromString<Task> txt |> Ok
}

let delTask taskId = promise {
    let props = [
        Method HttpMethod.DELETE
    ]
    let! res = Fetch.fetch (TaskId.route taskId) props
    return res.Ok |> Ok
}

let init (user: LoggedUser) =
    { User = user
      Tab = Tab.Users
      // User
      NewUsername = ""
      Users = []
      TryAddUser = NotStarted
      TryDelUser = NotStarted
      // Team
      NewTeamName = ""
      NewTeamManagerId = None
      Teams = []
      TryAddTeam = NotStarted
      TryDelTeam = NotStarted
      // CostCenter
      NewCostCenterName = ""
      CostCenters = []
      TryAddCostCenter = NotStarted
      TryDelCostCenter = NotStarted
      // Task
      NewTaskName = ""
      NewTaskCostCenterId = None
      Tasks = []
      TryAddTask = NotStarted
      TryDelTask = NotStarted
    }, Cmd.ofMsg LoadLists

let onFailed (e: exn) = Error e.Message

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | TabChanged newTab ->
        { state with Tab = newTab }, Cmd.none

    | LoadLists ->
        let cmd load msg =
            Cmd.OfPromise.either load () id onFailed |> Cmd.map (Completed >> msg)

        state, Cmd.batch [
            cmd loadUsers LoadUsers
            cmd loadTeams LoadTeams
            cmd loadCostCenters LoadCostCenters
            cmd loadTasks LoadTasks
        ]

    | LoadUsers (Completed result) ->
        match result with
        | Ok values ->
            let defaultManagerId =
                if values.IsEmpty
                then None
                else values |> List.head |> (fun x -> x.Id) |> Some
            { state with Users = values; NewTeamManagerId = defaultManagerId }, Cmd.none
        | _ ->
            state, Cmd.none

    | LoadUsers _ -> state, Cmd.none

    | LoadTeams (Completed result) ->
        match result with
        | Ok values ->
            { state with Teams = values }, Cmd.none
        | _ ->
            state, Cmd.none

    | LoadTeams _ -> state, Cmd.none

    | LoadCostCenters (Completed result) ->
        match result with
        | Ok values ->
            let defaultCostCenterId =
                if values.IsEmpty
                then None
                else values |> List.head |> (fun x -> x.Id) |> Some
            { state with CostCenters = values; NewTaskCostCenterId = defaultCostCenterId }, Cmd.none
        | _ ->
            state, Cmd.none

    | LoadCostCenters _ -> state, Cmd.none

    | LoadTasks (Completed result) ->
        match result with
        | Ok values ->
            { state with Tasks = values }, Cmd.none
        | _ ->
            state, Cmd.none

    | LoadTasks _ -> state, Cmd.none

    | NewUsernameChanged user ->
        { state with NewUsername = user }, Cmd.none

    | AddUserClicked Pending ->
        let nextState = { state with TryAddUser = InProgress }
        let nextCmd =
            Cmd.OfPromise.either addUser state.NewUsername id onFailed
            |> Cmd.map (Completed >> AddUserClicked)
        nextState, nextCmd

    | AddUserClicked (Completed addResult) ->
        let nextState = { state with TryAddUser = Resolved addResult }
        match addResult with
        | Ok user ->
            { nextState with Users = user :: nextState.Users; NewUsername = "" }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | DelUserClicked (userId, Pending) ->
        let toMsg x = DelUserClicked (userId, x)
        let nextState = { state with TryDelUser = InProgress }
        let nextCmd = Cmd.OfPromise.either delUser userId id onFailed |> Cmd.map (Completed >> toMsg)
        nextState, nextCmd

    | DelUserClicked (userId, Completed delResult) ->
        let nextState = { state with TryDelUser = Resolved delResult }
        match delResult with
        | Ok deleted when deleted ->
            { nextState with Users = nextState.Users |> List.where (fun user -> user.Id <> userId) }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | NewTeamNameChanged team ->
        { state with NewTeamName = team }, Cmd.none

    | NewTeamManagerChanged managerId ->
        { state with NewTeamManagerId = Some managerId }, Cmd.none

    | AddTeamClicked Pending ->
        match state.NewTeamManagerId with
        | Some managerId ->
            let nextState = { state with TryAddTeam = InProgress }
            let nextCmd =
                Cmd.OfPromise.either (uncurry addTeam) (state.NewTeamName, managerId) id onFailed
                |> Cmd.map (Completed >> AddTeamClicked)
            nextState, nextCmd
        | _ ->
            // Not reachable: click disabled when no cost center
            state, Cmd.none

    | AddTeamClicked (Completed addResult) ->
        let nextState = { state with TryAddTeam = Resolved addResult }
        match addResult with
        | Ok team ->
            { nextState with Teams = team :: nextState.Teams; NewTeamName = "" }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | DelTeamClicked (teamId, Pending) ->
        let toMsg x = DelTeamClicked (teamId, x)
        let nextState = { state with TryDelTeam = InProgress }
        let nextCmd = Cmd.OfPromise.either delTeam teamId id onFailed |> Cmd.map (Completed >> toMsg)
        nextState, nextCmd

    | DelTeamClicked (teamId, Completed delResult) ->
        let nextState = { state with TryDelTeam = Resolved delResult }
        match delResult with
        | Ok deleted when deleted ->
            { nextState with Teams = nextState.Teams |> List.where (fun team -> team.Id <> teamId) }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | NewCostCenterChanged name ->
        { state with NewCostCenterName = name }, Cmd.none

    | AddCostCenterClicked Pending ->
        let nextState = { state with TryAddCostCenter = InProgress }
        let nextCmd =
            Cmd.OfPromise.either addCostCenter state.NewCostCenterName id onFailed
            |> Cmd.map (Completed >> AddCostCenterClicked)
        nextState, nextCmd

    | AddCostCenterClicked (Completed addResult) ->
        let nextState = { state with TryAddCostCenter = Resolved addResult }
        match addResult with
        | Ok costCenter ->
            { nextState with CostCenters = costCenter :: nextState.CostCenters; NewCostCenterName = "" }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | DelCostCenterClicked (costCenterId, Pending) ->
        let toMsg x = DelCostCenterClicked (costCenterId, x)
        let nextState = { state with TryDelCostCenter = InProgress }
        let nextCmd = Cmd.OfPromise.either delCostCenter costCenterId id onFailed |> Cmd.map (Completed >> toMsg)
        nextState, nextCmd

    | DelCostCenterClicked (costCenterId, Completed delResult) ->
        let nextState = { state with TryDelCostCenter = Resolved delResult }
        match delResult with
        | Ok deleted when deleted ->
            { nextState with CostCenters = nextState.CostCenters |> List.where (fun costCenter -> costCenter.Id <> costCenterId) }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | NewTaskNameChanged task ->
        { state with NewTaskName = task }, Cmd.none

    | NewTaskCostCenterChanged costCenterId ->
        { state with NewTaskCostCenterId = Some costCenterId }, Cmd.none

    | AddTaskClicked Pending ->
        match state.NewTaskCostCenterId with
        | Some costCenterId ->
            let nextState = { state with TryAddTask = InProgress }
            let nextCmd =
                Cmd.OfPromise.either (uncurry addTask) (state.NewTaskName, costCenterId) id onFailed
                |> Cmd.map (Completed >> AddTaskClicked)
            nextState, nextCmd
        | _ ->
            // Not reachable: click disabled when no cost center
            state, Cmd.none

    | AddTaskClicked (Completed addResult) ->
        let nextState = { state with TryAddTask = Resolved addResult }
        match addResult with
        | Ok task ->
            { nextState with Tasks = task :: nextState.Tasks; NewTaskName = "" }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | DelTaskClicked (taskId, Pending) ->
        let toMsg x = DelTaskClicked (taskId, x)
        let nextState = { state with TryDelTask = InProgress }
        let nextCmd = Cmd.OfPromise.either delTask taskId id onFailed |> Cmd.map (Completed >> toMsg)
        nextState, nextCmd

    | DelTaskClicked (taskId, Completed delResult) ->
        let nextState = { state with TryDelTask = Resolved delResult }
        match delResult with
        | Ok deleted when deleted ->
            { nextState with Tasks = nextState.Tasks |> List.where (fun task -> task.Id <> taskId) }, Cmd.none
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
                    Bulma.subtitle.p user.Login.Value
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
        elif state.Users |> List.exists (fun user -> user.Login.Value = state.NewUsername.Trim())
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

// TODO show manager of team
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
        elif state.Users.IsEmpty
        then Some "No users"
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
                        // https://stackoverflow.com/a/55375604/305023
                        Bulma.select [
                            match state.NewTeamManagerId with
                            | Some managerId -> prop.value managerId.Value
                            | _ -> ()
                            prop.onChange (Int32.Parse >> UserId >> NewTeamManagerChanged >> dispatch)
                            prop.children (
                                state.Users
                                |> List.map (fun u ->
                                    Html.option [
                                        prop.value u.Id.Value
                                        //prop.onClick (fun _ -> NewTeamManagerChanged u.Id |> dispatch)
                                        prop.text u.Name
                                    ]
                                )
                            )
                        ]
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
                    if state.TryAddTeam = InProgress
                    then button.isLoading
                    else prop.onClick (fun _ -> dispatch (AddTeamClicked Pending))
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

let renderCostCenter (state: State) (dispatch: Msg -> unit) (costCenter: CostCenter) =
    Bulma.columns [
        columns.isVCentered
        prop.children [
            Bulma.column [
                Bulma.subtitle.p costCenter.Name
            ]
            Bulma.column [
                column.isNarrow
                prop.children [
                    Bulma.buttons [
                        Bulma.button.button [
                            color.isDanger
                            prop.onClick (fun _ -> (costCenter.Id, Pending) |> DelCostCenterClicked |> dispatch)
                            prop.children [
                                Fa.i [ Fa.Solid.Times ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderAddCostCenter (state: State) (dispatch: Msg -> unit) =
    let error =
        if String.IsNullOrWhiteSpace state.NewCostCenterName
        then Some ""
        elif state.CostCenters |> List.exists (fun cc -> cc.Name = state.NewCostCenterName.Trim())
        then Some "Cost center already exists."
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
                                prop.placeholder "Cost center"
                                prop.valueOrDefault state.NewCostCenterName
                                prop.onChange (NewCostCenterChanged >> dispatch)
                            ]
                            Bulma.icon [
                                icon.isSmall; icon.isLeft
                                prop.children [
                                    Fa.i [ Fa.Solid.EuroSign ] []
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
                            prop.onClick (fun _ -> dispatch (AddCostCenterClicked Pending))
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

let renderCostCenters (state: State) (dispatch: Msg -> unit) =
    Html.div [
        match state.TryAddCostCenter with
        | Resolved (Error m) ->
            Bulma.message [
                color.isDanger
                prop.children [
                    Bulma.messageBody (sprintf "Error adding new cost center: %s" m)
                ]
            ]
        | _ -> Html.none
        Bulma.box (
            state.CostCenters |> List.map (renderCostCenter state dispatch)
        )
    ]

let renderTask (state: State) (dispatch: Msg -> unit) (task: Task) =
    Bulma.columns [
        columns.isVCentered
        prop.children [
            Bulma.column [
                Bulma.subtitle.p task.Name
            ]
            Bulma.column [
                column.isNarrow
                prop.children [
                    Bulma.buttons [
                        Bulma.button.button [
                            color.isDanger
                            prop.onClick (fun _ -> (task.Id, Pending) |> DelTaskClicked |> dispatch)
                            prop.children [
                                Fa.i [ Fa.Solid.Times ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderAddTask (state: State) (dispatch: Msg -> unit) =
    let error =
        if String.IsNullOrWhiteSpace state.NewTaskName
        then Some ""
        elif state.Tasks |> List.exists (fun task -> task.Name = state.NewTaskName.Trim())
        then Some "Task already exists."
        elif state.CostCenters.IsEmpty
        then Some "No cost center"
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
                                prop.placeholder "Task"
                                prop.valueOrDefault state.NewTaskName
                                prop.onChange (NewTaskNameChanged >> dispatch)
                            ]
                            Bulma.icon [
                                icon.isSmall; icon.isLeft
                                prop.children [
                                    Fa.i [ Fa.Solid.Tasks ] []
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
                Bulma.label "Cost Center"
                Bulma.control.div [
                    control.hasIconsLeft
                    prop.children [
                        // https://stackoverflow.com/a/55375604/305023
                        Bulma.select [
                            match state.NewTaskCostCenterId with
                            | Some costCenterId -> prop.defaultValue costCenterId.Value
                            | _ -> ()
                            prop.onChange (fun (ev: Event) -> ev.target?value |> CostCenterId |> NewTaskCostCenterChanged |> dispatch)
                            prop.children (
                                state.CostCenters
                                |> List.map (fun cc ->
                                    Html.option [
                                        prop.value cc.Id.Value
                                        prop.text cc.Name
                                    ]
                                )
                            )
                        ]
                        Bulma.icon [
                            icon.isSmall; icon.isLeft
                            prop.children [
                                Fa.i [ Fa.Solid.EuroSign ] []
                            ]
                        ]
                    ]
                ]
            ]
            Bulma.field.div [
                Bulma.button.button [
                    color.isInfo
                    prop.disabled (Option.isSome error)
                    if state.TryAddTask = InProgress
                    then button.isLoading
                    else prop.onClick (fun _ -> dispatch (AddTaskClicked Pending))
                    prop.text "Add Task"
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

let renderTasks (state: State) (dispatch: Msg -> unit) =
    Html.div [
        match state.TryAddTask with
        | Resolved (Error m) ->
            Bulma.message [
                color.isDanger
                prop.children [
                    Bulma.messageBody (sprintf "Error adding new task: %s" m)
                ]
            ]
        | _ -> Html.none
        Bulma.box (
            state.Tasks |> List.map (renderTask state dispatch)
        )
    ]

let render (state: State) dispatch =
    Html.div [
        renderNavigation (LoggedIn state.User) Url.Admin
 
        centered [
            Html.h1 [
                Html.strong (state.User.Username.Value.ToUpper())
            ]

            Html.p "This is the admin page"

            Bulma.tabs [
                Html.ul (
                    [ Tab.Users, "Users"
                      Tab.Teams, "Teams"
                      Tab.CostCenters, "Cost Centers"
                      Tab.Tasks, "Common Tasks"
                    ] |> List.map (fun (t, label) ->
                        Html.li [
                            if state.Tab = t then tab.isActive
                            prop.children [
                                Html.a [
                                    if state.Tab <> t
                                    then prop.onClick (fun _ -> TabChanged t |> dispatch)
                                    prop.text label
                                ]
                            ]
                        ]
                    )
                )
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
                    if state.Tab = Tab.CostCenters then
                        renderAddCostCenter state dispatch
                        renderCostCenters state dispatch
                    if state.Tab = Tab.Tasks then
                        renderAddTask state dispatch
                        renderTasks state dispatch
                ]
            ]
        ]
    ]
