module Pages.Home

open Client.Domain

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

type LoadWeeksResult = Result<MonthWeeks, string>

type LoadTasksResult = Result<Task list, string>

type LoadActivitiesResult = Result<Activity list, string>

type AddActivityResult = Result<Activity, string>

type UpdateActivityResult = Result<Activity, string>

type DeleteActivityResult = Result<bool, string>

type ActiveWeek = private {
        monday : SafeDate
        friday : SafeDate
        week : Week
    } with
        member x.Monday = x.monday
        member x.Friday = x.friday
        member x.Number = x.week.Number
        member x.Year = x.week.Year
        member x.Age = Week.age x.week

module ActiveWeek =
    let create week =
        let Monday, Friday = Week.range week
        { ActiveWeek.monday = Monday
          friday = Friday
          week = week }
    let current = create Week.current

type PendingActivity = {
    Date : SafeDate
    TaskId : TaskId option
    DurationText : string
    Days : Result<WorkDays, string>
    Comment : string
}

module PendingActivity =
    let create date taskId =
        { Date = date
          TaskId = taskId
          DurationText = "0"
          Days = Ok WorkDays.zero
          Comment = ""
        }

type EditingActivity = {
    Id : ActivityId
    UserId : UserId
    Date : SafeDate
    TaskId : TaskId
    DurationText : string
    Days : Result<WorkDays, string>
    Comment : string
}

type State =
    { User : ApplicationUser
      Year : YearNumber
      Week : ActiveWeek
      Weeks : MonthWeeks option
      Tasks : Task list
      TryLoadWeeks : Deferred<LoadWeeksResult>
      TryLoadTasks : Deferred<LoadTasksResult>
      TryLoadActivities : Deferred<LoadActivitiesResult>
      // Activities
      Activities : Activity list
      NewActivity : PendingActivity
      TryAddActivity : Deferred<AddActivityResult>
      EditingActivity : EditingActivity option
      TryUpdateActivity : Deferred<UpdateActivityResult>
      TryDeleteActivity : Deferred<DeleteActivityResult>
    }
    // TODO: no add when week full
    // TODO: Adding new activity cannot exceed 5 days
    // TODO: Edit activity cannot exceed 5 days
    member x.TotalDays =
        x.Activities |> List.sumBy (fun a -> a.Days.Value)

type Msg =
    | LoadWeeks of PromiseStatus<LoadWeeksResult>
    | LoadTasks of PromiseStatus<LoadTasksResult>
    | LoadActivities of PromiseStatus<LoadActivitiesResult>
    | ChangeYearClicked of YearNumber
    | ChangeWeekClicked of Week
    | SetNewDate of Result<SafeDate, string>
    | SetNewTaskId of TaskId
    | SetNewDuration of string
    | SetNewComment of string
    | AddActivityClicked of PromiseStatus<AddActivityResult>
    | CancelAddClicked
    | EditActivityClicked of Activity
    | DeleteActivityClicked of ActivityId * PromiseStatus<DeleteActivityResult>
    | UpdateDate of Result<SafeDate, string>
    | UpdateTaskId of TaskId
    | UpdateDuration of string
    | UpdateComment of string
    | CancelEditClicked
    | SaveActivityClicked of PromiseStatus<AddActivityResult>

let loadWeeks user year = promise {
    match user with
    | Anonymous ->
        return Error ""
    | LoggedIn loggedIn ->
        let props = [
            Method HttpMethod.GET
            Fetch.requestHeaders [ ContentType "application/json" ]
        ]
        let! res = Fetch.fetch (Week.yearRoute loggedIn.Id year) props
        let! txt = res.text()
        return Decode.Auto.unsafeFromString<MonthWeeks> txt |> Ok
}

let loadTasks user = promise {
    match user with
    | Anonymous ->
        return Ok []
    | LoggedIn loggedIn ->
        let props = [
            Method HttpMethod.GET
            Fetch.requestHeaders [ ContentType "application/json" ]
        ]
        let getTasks route = promise {
            let! res = Fetch.fetch route props
            let! txt = res.text()
            return Decode.Auto.unsafeFromString<Task list> txt
        }
        let! taskLists =
            loggedIn.Teams
            |> List.fold (fun l team -> TeamId.taskRoute team.Id :: l) [ Route.tasks ]
            |> List.map getTasks
            |> Promise.Parallel
        return
            taskLists
            |> List.ofArray
            |> List.concat
            |> Ok
}

let loadActivities user (week: Week) = promise {
    match user with
    | Anonymous ->
        return Ok []
    | LoggedIn loggedIn ->
        let props = [
            Method HttpMethod.GET
            Fetch.requestHeaders [ ContentType "application/json" ]
        ]
        let! res = Fetch.fetch (Week.activityRoute loggedIn.Id week) props
        let! txt = res.text()
        return Decode.Auto.unsafeFromString<Activity list> txt |> Ok
}

let onFailed (e: exn) = Error e.Message

// TODO default date = first day not complete

let init user =
    let state =
        { State.User = user
          Year = YearNumber.current
          Week = ActiveWeek.current
          Weeks = None
          Tasks = []
          TryLoadWeeks = NotStarted
          TryLoadTasks = NotStarted
          TryLoadActivities = NotStarted
          // Activities
          Activities = []
          NewActivity = PendingActivity.create SafeDate.today None
          TryAddActivity = NotStarted
          EditingActivity = None
          TryUpdateActivity = NotStarted
          TryDeleteActivity = NotStarted
        }
    state, Cmd.batch [
        ChangeYearClicked state.Year |> Cmd.ofMsg
        // Must load tasks before activities
        Cmd.OfPromise.either loadTasks user id onFailed |> Cmd.map (Completed >> LoadTasks)
    ]

let addActivity (newActivity: NewActivity) = promise {
    //printfn "addActivity"
    let body = Encode.Auto.toString(0, newActivity)
    let props = [
        Method HttpMethod.POST
        Fetch.requestHeaders [ ContentType "application/json" ]
        Body !^body
    ]
    let! res = Fetch.fetch Route.activities props
    let! txt = res.text()
    return Decode.Auto.unsafeFromString<Activity> txt |> Ok
}

let updateActivity (updatedActivity: Activity) = promise {
    let body = Encode.Auto.toString(0, updatedActivity)
    let props = [
        Method HttpMethod.PUT
        Fetch.requestHeaders [ ContentType "application/json" ]
        Body !^body
    ]
    let! res = Fetch.fetch Route.activities props
    let! txt = res.text()
    return Decode.Auto.unsafeFromString<Activity> txt |> Ok
}

let deleteActivity (activityId: ActivityId) = promise {
    let props = [
        Method HttpMethod.DELETE
    ]
    let! res = Fetch.fetch (ActivityId.route activityId) props
    return res.Ok |> Ok
}

let updateEditing (state: State) (update: EditingActivity -> EditingActivity option) =
    match state.EditingActivity with
    | Some updating ->
        match update updating with
        | Some _ as newUpdating ->
            { state with EditingActivity = newUpdating }
        | _ -> state
    | _ -> state

let updateWeeks (state: State) (activities: Activity list) =
    let newSumDays =
        activities
        |> List.map (fun activity -> activity.Days.Value)
        |> List.sum
    let newStatus =
        if newSumDays = WorkDays.max.Value then
            Some WeekStatus.Full
        elif newSumDays = WorkDays.zero.Value then
            match state.Week.Age with
            | WeekAge.Future -> None
            | age -> WeekStatus.Incomplete age |> Some
        else
            WeekStatus.Incomplete state.Week.Age |> Some
    match state.Weeks with
    | Some weeks -> weeks |> MonthWeeks.update state.Week.week newStatus |> Some
    | _ -> None

let update (msg: Msg) (state: State) =
    //printfn "%A" msg
    match msg with
    | LoadWeeks (Completed result) ->
        let nextState = { state with TryLoadWeeks = Resolved result }
        match result with
        | Ok weeks ->
            { nextState with Weeks = Some weeks }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | LoadWeeks _ -> state, Cmd.none

    | LoadTasks (Completed result) ->
        let nextState = { state with TryLoadTasks = Resolved result }
        match result with
        | Ok tasks ->
            let defaultTaskId =
                match tasks with
                | t :: _ -> Some t.Id
                | _ -> None
            let newActivity = { state.NewActivity with TaskId = defaultTaskId }
            let nextCmd =
                Cmd.OfPromise.either (uncurry loadActivities) (state.User, state.Week.week) id onFailed
                |> Cmd.map (Completed >> LoadActivities)
            { nextState with Tasks = tasks; NewActivity = newActivity }, nextCmd
        | _ ->
            nextState, Cmd.none

    | LoadTasks _ -> state, Cmd.none

    | LoadActivities (Completed result) ->
        let nextState = { state with TryLoadActivities = Resolved result }
        match result with
        | Ok activities ->
            { nextState with Activities = activities }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | LoadActivities _ -> state, Cmd.none

    | ChangeYearClicked newYear ->
        let nextState = { state with Year = newYear; TryLoadWeeks = InProgress }
        let nextCmd =
            Cmd.OfPromise.either (uncurry loadWeeks) (state.User, nextState.Year) id onFailed
            |> Cmd.map (Completed >> LoadWeeks)
        nextState, nextCmd

    | ChangeWeekClicked newWeek ->
        let week = ActiveWeek.create newWeek
        // TODO not Friday
        let pending = { state.NewActivity with Date = week.Friday }
        let nextState = { state with Week = week; TryLoadActivities = InProgress; NewActivity = pending }
        let nextCmd =
            Cmd.OfPromise.either (uncurry loadActivities) (state.User, newWeek) id onFailed
            |> Cmd.map (Completed >> LoadActivities)
        nextState, nextCmd

    | SetNewDate date ->
        printfn "SetNewDate %A" date
        match date with
        | Ok d ->
            let newActivity = { state.NewActivity with Date = d }
            { state with NewActivity = newActivity }, Cmd.none
        | _ ->
            state, Cmd.none

    | SetNewTaskId taskId ->
        let newActivity = { state.NewActivity with TaskId = Some taskId }
        { state with NewActivity = newActivity }, Cmd.none

    | SetNewDuration text ->
        // TODO support .001 (begin with .)
        match Double.TryParse text with
        | (true, f) ->
            let newActivity = { state.NewActivity with Days = WorkDays.create f; DurationText = text }
            { state with NewActivity = newActivity }, Cmd.none
        | (false,_) ->
            state, Cmd.none

    | SetNewComment text ->
        let newActivity = { state.NewActivity with Comment = text }
        { state with NewActivity = newActivity }, Cmd.none

    | AddActivityClicked Pending ->
        // TODO week days <= 5
        match state.User, state.NewActivity.TaskId, state.NewActivity.Days with
        | LoggedIn loggedIn, Some taskId, Ok days ->
            let nextState = { state with TryAddActivity = InProgress }
            let newActivity =
                { NewActivity.UserId = loggedIn.Id
                  Date = state.NewActivity.Date
                  TaskId = taskId
                  Days = days
                  Comment = state.NewActivity.Comment
                }
            let nextCmd =
                Cmd.OfPromise.either addActivity newActivity id onFailed
                |> Cmd.map (Completed >> AddActivityClicked)
            nextState, nextCmd
        // unreachable
        | _ -> state, Cmd.none

    | AddActivityClicked (Completed addResult) ->
        let nextState = { state with TryAddActivity = Resolved addResult }
        match addResult with
        | Ok activity ->
            let defaultTaskId =
                match state.Tasks with
                | t :: ts -> Some t.Id
                | _ -> None
            // TODO not Friday
            let pending = PendingActivity.create state.Week.Friday defaultTaskId
            let newActivities = activity :: nextState.Activities
            let newWeeks = updateWeeks nextState newActivities
            { nextState with Activities = newActivities; NewActivity = pending; Weeks = newWeeks }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | CancelAddClicked ->
        let defaultTaskId =
            match state.Tasks with
            | t :: ts -> Some t.Id
            | _ -> None
        // TODO not Friday
        let pending = PendingActivity.create state.Week.Friday defaultTaskId
        { state with NewActivity = pending }, Cmd.none

    | EditActivityClicked activity ->
        let editable =
            { EditingActivity.Id = activity.Id
              UserId = activity.UserId
              Date = activity.Date
              TaskId = activity.TaskId
              DurationText = activity.Days.Value.ToString()
              Days = Ok activity.Days
              Comment = activity.Comment
            }
        { state with EditingActivity = Some editable }, Cmd.none

    | UpdateDate date ->
        let update (editing: EditingActivity) =
            match date with
            | Ok d -> Some { editing with Date = d }
            | _ -> None
        updateEditing state update, Cmd.none

    | UpdateTaskId taskId ->
        let update (editing: EditingActivity) = Some { editing with TaskId = taskId }
        updateEditing state update, Cmd.none

    | UpdateDuration text ->
        let update (editing: EditingActivity) =
            // TODO support .001 (begin with .)
            match Double.TryParse text with
            | (true, f) -> Some { editing with Days = WorkDays.create f; DurationText = text }
            | (false,_) -> None
        updateEditing state update, Cmd.none

    | UpdateComment text ->
        let update (editing: EditingActivity) = Some { editing with Comment = text }
        updateEditing state update, Cmd.none

    | CancelEditClicked ->
        { state with EditingActivity = None }, Cmd.none

    | SaveActivityClicked Pending ->
        // TODO week days <= 5
        match state.EditingActivity with
        | Some updating ->
            match updating.Days with
            | Ok days ->
                let nextState = { state with TryUpdateActivity = InProgress }
                let activity =
                    { Activity.Id = updating.Id
                      UserId = updating.UserId
                      Date = updating.Date
                      TaskId = updating.TaskId
                      Days = days
                      Comment = updating.Comment
                    }
                let nextCmd =
                    Cmd.OfPromise.either updateActivity activity id onFailed
                    |> Cmd.map (Completed >> SaveActivityClicked)
                nextState, nextCmd
            | _ -> state, Cmd.none
        // unreachable
        | _ -> state, Cmd.none

    | SaveActivityClicked (Completed updateResult) ->
        let nextState = { state with TryUpdateActivity = Resolved updateResult }
        match updateResult with
        | Ok updated ->
            let newActivities =
                state.Activities
                |> List.map (fun activity ->
                    if activity.Id = updated.Id then
                        { activity with
                            TaskId = updated.TaskId
                            Date = updated.Date
                            Days = updated.Days
                            Comment = updated.Comment
                        }
                    else activity)
            let newWeeks = updateWeeks nextState newActivities
            { nextState with Activities = newActivities; EditingActivity = None; Weeks = newWeeks }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | DeleteActivityClicked (activityId, Pending) ->
        let toMsg x = DeleteActivityClicked (activityId, x)
        let nextState = { state with TryDeleteActivity = InProgress }
        let nextCmd =
            Cmd.OfPromise.either deleteActivity activityId id onFailed
            |> Cmd.map (Completed >> toMsg)
        nextState, nextCmd

    | DeleteActivityClicked (activityId, Completed delResult) ->
        let nextState = { state with TryDeleteActivity = Resolved delResult }
        match delResult with
        | Ok deleted when deleted ->
            let newActivities = nextState.Activities |> List.where (fun activity -> activity.Id <> activityId)
            let newWeeks = updateWeeks nextState newActivities
            { nextState with Activities = newActivities; Weeks = newWeeks }, Cmd.none
        | _ ->
            nextState, Cmd.none

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
                                        | Some status ->
                                            match status with
                                            | WeekStatus.Full -> color.isSuccess
                                            | WeekStatus.Incomplete WeekAge.Old -> color.isDanger
                                            | WeekStatus.Incomplete WeekAge.Current -> color.isPrimary; color.isLight
                                            | WeekStatus.Incomplete WeekAge.Future -> color.isPrimary; color.isLight
                                        | _ -> color.isWhite
                                        prop.key (sprintf "week_%i_%i" state.Year.Value week.Number)
                                        prop.disabled state.EditingActivity.IsSome
                                        prop.onClick (fun _ -> ChangeWeekClicked week |> dispatch)
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

let renderActivity (taskNameOfId: Map<TaskId, string>) (activity: Activity) (state: State) dispatch =
    Html.tr [
        Html.td [
            prop.style [
                style.verticalAlign.middle
                style.textAlign.center
            ]
            prop.text (activity.Date.Value.ToString("dd/MM"))
        ]
        Html.td [
            prop.style [ style.verticalAlign.middle ]
            prop.text (Map.find activity.TaskId taskNameOfId)
        ]
        Html.td [
            prop.style [ style.verticalAlign.middle ]
            prop.text (activity.Days.Value.ToString())
        ]
        Html.td [
            prop.style [ style.verticalAlign.middle ]
            prop.text activity.Comment
        ]
        Html.td [
            spacing.py1
            prop.style [ style.verticalAlign.middle ]
            prop.children [
                Bulma.button.button [
                    color.isWarning; button.isSmall
                    prop.disabled state.EditingActivity.IsSome
                    prop.onClick (fun _ -> EditActivityClicked activity |> dispatch)
                    prop.children [
                        Fa.i [ Fa.Solid.Edit ] []
                    ]
                ]
                Bulma.button.button [
                    color.isDanger; button.isSmall; spacing.mx2
                    prop.disabled state.EditingActivity.IsSome
                    prop.onClick (fun _ -> (activity.Id, Pending) |> DeleteActivityClicked |> dispatch)
                    prop.children [
                        Fa.i [ Fa.Solid.Times ] []
                    ]
                ]
            ]
        ]
    ]

let renderEditActivity (activity: EditingActivity) (state: State) dispatch =
    Html.tr [
        Html.td [
            let hasPrev = activity.Date.Value > state.Week.Monday.Value
            let hasNext = activity.Date.Value < state.Week.Friday.Value
            prop.style [
                style.verticalAlign.middle
                if hasPrev && not hasNext then style.textAlign.left
                if not hasPrev && hasNext then style.textAlign.right
            ]
            prop.children [
                if hasPrev then
                    Html.a [
                        color.isLight; color.hasTextLink
                        prop.onClick (fun _ -> UpdateDate (SafeDate.tryPrev activity.Date) |> dispatch)
                        prop.children [
                            Bulma.icon [
                                Fa.i [ Fa.Solid.CaretSquareLeft ] []
                            ]
                        ]
                    ]
                Html.span (activity.Date.Value.ToString("dd/MM"))
                if hasNext then
                    Html.a [
                        color.isLight; color.hasTextLink
                        prop.onClick (fun _ -> UpdateDate (SafeDate.tryNext activity.Date) |> dispatch)
                        prop.children [
                            Bulma.icon [
                                Fa.i [ Fa.Solid.CaretSquareRight ] []
                            ]
                        ]
                    ]
            ]
        ]
        Html.td [
            Bulma.control.div [
                prop.children [
                    Bulma.select [
                        input.isFocused
                        prop.value activity.TaskId.Value
                        prop.onChange (Int32.Parse >> TaskId >> UpdateTaskId >> dispatch)
                        prop.children (
                            state.Tasks
                            |> List.map (fun u ->
                                Html.option [
                                    prop.value u.Id.Value
                                    prop.text u.Name
                                ]
                            )
                        )
                    ]
                ]
            ]
        ]
        let daysOk =
            match activity.Days with
            | Ok days ->
                let sum = 
                    state.Activities
                    |> List.sumBy (fun a ->
                        if a.Id = activity.Id
                        then days.Value
                        else a.Days.Value)
                days.Value > 0. && sum <= WorkDays.max.Value
            | _ -> false
        Html.td [
            Bulma.control.div [
                Bulma.input.text [
                    prop.style [ style.width 70 ]
                    input.isFocused
                    if not daysOk then color.isDanger
                    prop.valueOrDefault activity.DurationText
                    prop.onTextChange (UpdateDuration >> dispatch)
                ]
            ]
        ]
        Html.td [
            Bulma.control.div [
                prop.children [
                    Bulma.input.text [
                        input.isFocused
                        prop.valueOrDefault activity.Comment
                        prop.placeholder "Comment"
                        prop.onTextChange (UpdateComment >> dispatch)
                    ]
                ]
            ]
        ]
        Html.td [
            prop.style [ style.verticalAlign.middle ]
            prop.children [
                Bulma.button.button [
                    color.isWarning; button.isSmall
                    prop.onClick (fun _ -> CancelEditClicked |> dispatch)
                    prop.children [
                        Fa.i [ Fa.Solid.Ban ] []
                    ]
                ]
                Bulma.button.button [
                    color.isPrimary; button.isSmall; spacing.mx2
                    prop.onClick (fun _ -> SaveActivityClicked Pending |> dispatch)
                    prop.children [
                        Fa.i [ Fa.Solid.Save ] []
                    ]
                ]
            ]
        ]
    ]

let renderActivities (state: State) dispatch =
    let taskNameOfId =
        state.Tasks
        |> Seq.map (fun t -> t.Id, t.Name)
        |> Map.ofSeq
    state.Activities
    |> List.sortBy (fun a -> a.Date.Value)
    |> List.map (fun activity ->
        match state.EditingActivity with
        | Some updating when updating.Id = activity.Id ->
            renderEditActivity updating state dispatch
        | _ ->
            renderActivity taskNameOfId activity state dispatch)

let renderNewActivity (state: State) dispatch =
    let newActivity = state.NewActivity
    Html.tr [
        Html.td [
            let hasPrev = newActivity.Date.Value > state.Week.Monday.Value
            let hasNext = newActivity.Date.Value < state.Week.Friday.Value
            prop.style [
                style.verticalAlign.middle
                if hasPrev && not hasNext then style.textAlign.left
                if not hasPrev && hasNext then style.textAlign.right
            ]
            prop.children [
                if hasPrev then
                    Html.a [
                        color.isLight; color.hasTextLink
                        prop.onClick (fun _ -> SetNewDate (SafeDate.tryPrev newActivity.Date) |> dispatch)
                        prop.children [
                            Bulma.icon [
                                Fa.i [ Fa.Solid.CaretSquareLeft ] []
                            ]
                        ]
                    ]
                Html.span (newActivity.Date.Value.ToString("dd/MM"))
                if hasNext then
                    Html.a [
                        color.isLight; color.hasTextLink
                        prop.onClick (fun _ -> SetNewDate (SafeDate.tryNext newActivity.Date) |> dispatch)
                        prop.children [
                            Bulma.icon [
                                Fa.i [ Fa.Solid.CaretSquareRight ] []
                            ]
                        ]
                    ]
            ]
        ]
        Html.td [
            Bulma.control.div [
                prop.children [
                    Bulma.select [
                        input.isFocused
                        match newActivity.TaskId with
                        | Some taskId -> prop.value taskId.Value
                        | _ -> ()
                        prop.onChange (Int32.Parse >> TaskId >> SetNewTaskId >> dispatch)
                        prop.children (
                            state.Tasks
                            |> List.map (fun u ->
                                Html.option [
                                    prop.value u.Id.Value
                                    prop.text u.Name
                                ]
                            )
                        )
                    ]
                ]
            ]
        ]
        let daysOk =
            match newActivity.Days with
            | Ok days ->
                days.Value > 0. && state.TotalDays + days.Value <= WorkDays.max.Value
            | _ -> false
        Html.td [
            Bulma.control.div [
                Bulma.input.text [
                    prop.style [ style.width 70 ]
                    input.isFocused
                    if not daysOk then color.isDanger
                    prop.valueOrDefault newActivity.DurationText
                    prop.onTextChange (SetNewDuration >> dispatch)
                ]
            ]
        ]
        Html.td [
            Bulma.control.div [
                prop.children [
                    Bulma.input.text [
                        input.isFocused
                        prop.valueOrDefault newActivity.Comment
                        prop.placeholder "Comment"
                        prop.onTextChange (SetNewComment >> dispatch)
                    ]
                ]
            ]
        ]
        Html.td [
            prop.style [ style.verticalAlign.middle ]
            prop.children [
                Bulma.button.button [
                    color.isWarning; button.isSmall
                    prop.disabled (not daysOk)
                    prop.onClick (fun _ -> AddActivityClicked Pending |> dispatch)
                    prop.children [
                        Fa.i [ Fa.Solid.Plus ] []
                    ]
                ]
                Bulma.button.button [
                    color.isWarning; button.isSmall; spacing.mx2
                    prop.disabled (not daysOk)
                    prop.onClick (fun _ -> CancelAddClicked |> dispatch)
                    prop.children [
                        Fa.i [ Fa.Solid.Ban ] []
                    ]
                ]
            ]
        ]
    ]

let renderWeekActivity (state: State) dispatch =
    Html.div [
        Html.div [
            prop.style [ style.margin 10 ]
        ]
        Bulma.button.button [
            button.isStatic; button.isLarge; spacing.mx3
            prop.text (sprintf "Week %i of %i" state.Week.Number state.Week.Year)
        ] |> centerTable
        Html.table [
            prop.style [
                style.marginLeft length.auto
                style.marginRight length.auto
                style.textAlign.left
            ]
            prop.children [
                Html.thead [
                    Html.tr [
                        Html.td [
                            prop.style [ style.textAlign.center ]
                            prop.text "Date"
                        ]
                        Html.td "Task"
                        Html.td "Duration"
                        Html.td "Comment"
                    ]
                ]
                Html.tbody [
                    for row in renderActivities state dispatch do
                        row
                    if state.TotalDays < WorkDays.max.Value then
                        renderNewActivity state dispatch
                ]
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        renderMenu state dispatch

        match state.User with
        | LoggedIn user ->
            if not user.IsAdmin then
                renderYearNavigation state dispatch
                renderWeeks state dispatch
                renderWeekActivity state dispatch
        | _ -> Html.none
    ]