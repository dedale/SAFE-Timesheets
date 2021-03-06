﻿module Pages.Team

open Client.Domain
open Client.Navigation

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

// TODO move user to another team
// TODO manage/export team timesheets

type LoadUsersResult = Result<User list, string>
type LoadCostCentersResult = Result<CostCenter list, string>
type LoadTasksResult = Result<Task list, string>

type AddTaskResult = Result<Task, string>
// TODO Edit
type DelTaskResult = Result<bool, string>

type Tab =
    | Users
    | Tasks

type State =
    { User : LoggedUser
      Team : Team
      Tab : Tab
      Users : User list
      CostCenters : CostCenter list

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
    | LoadCostCenters of PromiseStatus<LoadCostCentersResult>
    | LoadTasks of PromiseStatus<LoadTasksResult>
    // Tasks
    | NewTaskNameChanged of string
    | NewTaskCostCenterChanged of CostCenterId
    | AddTaskClicked of PromiseStatus<AddTaskResult>
    | DelTaskClicked of TaskId * PromiseStatus<DelTaskResult>

let (|TaskAdded|_|) = function
| AddTaskClicked (Completed (Ok task)) -> Some task
| _ -> None

let getValuesJson route = promise {
    let props = [
        Method HttpMethod.GET
        Fetch.requestHeaders [ ContentType "application/json" ]
    ]
    let! res = Fetch.fetch route props
    return! res.text()
}

let loadUsers teamId = promise {
    let! txt = TeamId.userRoute teamId |> getValuesJson
    return Decode.Auto.unsafeFromString<User list> txt |> Ok
}

let loadCostCenters() = promise {
    let! txt = getValuesJson Route.costCenters
    return Decode.Auto.unsafeFromString<CostCenter list> txt |> Ok
}

let loadTasks teamId = promise {
    let! txt = TeamId.taskRoute teamId |> getValuesJson
    return Decode.Auto.unsafeFromString<Task list> txt |> Ok
}

let addTask taskId (name: string) (costCenterId: CostCenterId) = promise {
    let body = Encode.Auto.toString(0, (name, costCenterId))
    let props = [
        Method HttpMethod.POST
        Fetch.requestHeaders [ ContentType "application/json" ]
        Body !^body
    ]
    let! res = Fetch.fetch (TeamId.taskRoute taskId) props
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
      // TODO Manage more than one team
      Team = user.ManagedTeams.Head
      Tab = Tab.Users
      Users = []
      CostCenters = []
      // Task
      NewTaskName = ""
      NewTaskCostCenterId = None
      Tasks = []
      TryAddTask = NotStarted
      TryDelTask = NotStarted
    }, Cmd.ofMsg LoadLists

let onFailed (e: exn) = Error e.Message

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    //printfn "Team:update %A %A" msg state
    match msg with
    | TabChanged newTab ->
        { state with Tab = newTab }, Cmd.none

    | LoadLists ->
        let cmd load =
            Cmd.OfPromise.either load () id onFailed

        state, Cmd.batch [
            Cmd.OfPromise.either loadUsers state.Team.Id id onFailed |> Cmd.map (Completed >> LoadUsers)
            Cmd.OfPromise.either loadCostCenters () id onFailed |> Cmd.map (Completed >> LoadCostCenters)
            Cmd.OfPromise.either loadTasks state.Team.Id id onFailed |> Cmd.map (Completed >> LoadTasks)
        ]

    | LoadUsers (Completed result) ->
        match result with
        | Ok values ->
            { state with Users = values }, Cmd.none
        | _ ->
            state, Cmd.none

    | LoadUsers _ -> state, Cmd.none

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

    | NewTaskNameChanged task ->
        { state with NewTaskName = task }, Cmd.none

    | NewTaskCostCenterChanged costCenterId ->
        { state with NewTaskCostCenterId = Some costCenterId }, Cmd.none

    | AddTaskClicked Pending ->
        match state.NewTaskCostCenterId with
        | Some costCenterId ->
            let nextState = { state with TryAddTask = InProgress }
            let nextCmd =
                Cmd.OfPromise.either (uncurry3 addTask) (state.Team.Id, state.NewTaskName, costCenterId) id onFailed
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
        | Ok deleted ->
            if deleted then
                { nextState with Tasks = nextState.Tasks |> List.where (fun task -> task.Id <> taskId) }, Cmd.none
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
        ]
    ]

let renderUsers (state: State) (dispatch: Msg -> unit) =
    Bulma.box (
        state.Users |> List.map (renderUser state dispatch)
    )

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
                        Bulma.select (
                            state.CostCenters |> List.map (fun cc -> Html.option cc.Name)
                        )
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

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        renderNavigation (LoggedIn state.User) Url.Team

        centered [
            Html.p (sprintf "%s Team page" state.Team.Name)

            Bulma.tabs [
                Html.ul (
                    [ Tab.Users, "Users"
                      Tab.Tasks, "Team Tasks"
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
                        ])
                )
            ]

            Html.div [
                prop.style [
                    style.width 600
                    style.textAlign.left
                ]
                prop.children [
                    if state.Tab = Tab.Users then
                        renderUsers state dispatch
                    if state.Tab = Tab.Tasks then
                        renderAddTask state dispatch
                        renderTasks state dispatch
                ]
            ]
        ]
    ]

