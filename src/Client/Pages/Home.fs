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

type AddActivityResult = Result<Activity, string>

type ActiveWeek = private {
        monday : SafeDate
        friday : SafeDate
        week : Week
    } with
        member x.Monday = x.monday
        member x.Friday = x.friday
        member x.Number = x.week.Number
        member x.Year = x.week.Year

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
    let create () =
        { Date = SafeDate.today
          TaskId = None
          DurationText = "0"
          Days = Ok WorkDays.zero
          Comment = ""
        }

type State =
    { User : ApplicationUser
      Year : YearNumber
      Week : ActiveWeek
      Weeks : MonthWeeks option
      Tasks : Task list
      TryLoadWeeks : Deferred<LoadWeeksResult>
      TryLoadTasks : Deferred<LoadTasksResult>
      // Activities
      Activities : Activity list
      NewActivity : PendingActivity
      TryAddActivity : Deferred<AddActivityResult>
    }

type Msg =
    | LoadWeeks of PromiseStatus<LoadWeeksResult>
    | LoadTasks of PromiseStatus<LoadTasksResult>
    | ChangeYearClicked of YearNumber
    | ChangeWeekClicked of Week
    | SetNewDate of Result<SafeDate, string>
    | SetNewTaskId of TaskId
    | SetNewDuration of string
    | SetNewComment of string
    | AddActivityClicked of PromiseStatus<AddActivityResult>

let loadWeeks year = promise {
    let props = [
        Method HttpMethod.GET
        Fetch.requestHeaders [ ContentType "application/json" ]
    ]
    let! res = Fetch.fetch (YearNumber.route year) props
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
          // Activities
          Activities = []
          NewActivity = PendingActivity.create()
          TryAddActivity = NotStarted
        }
    state, Cmd.batch [
        ChangeYearClicked state.Year |> Cmd.ofMsg
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
                if tasks.IsEmpty
                then None
                else tasks |> List.head |> (fun x -> x.Id) |> Some
            let newActivity = { state.NewActivity with TaskId = defaultTaskId }
            { nextState with Tasks = tasks; NewActivity = newActivity }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | LoadTasks _ -> state, Cmd.none

    | ChangeYearClicked newYear ->
        let nextState = { state with Year = newYear; TryLoadWeeks = InProgress }
        let nextCmd =
            Cmd.OfPromise.either loadWeeks nextState.Year id onFailed
            |> Cmd.map (Completed >> LoadWeeks)
        nextState, nextCmd

    | ChangeWeekClicked newWeek ->
        // TODO
        state, Cmd.none

    | SetNewDate date ->
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
        //printfn "%A |===| %A |===| %A" state.User state.NewActivity.TaskId state.NewActivity.Days
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
            { nextState with Activities = activity :: nextState.Activities; NewActivity = PendingActivity.create() }, Cmd.none
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
                                        | Some b -> if b then color.isSuccess else color.isInfo
                                        | _ -> color.isWhite
                                        prop.key (sprintf "week_%i_%i" state.Year.Value week.Number)
                                        // TODO prop.onClick (fun _ -> )
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
                    prop.children [
                        Fa.i [ Fa.Solid.Edit ] []
                    ]
                ]
                Bulma.button.button [
                    color.isDanger; button.isSmall; spacing.mx2
                    prop.children [
                        Fa.i [ Fa.Solid.Times ] []
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
    |> List.map (fun activity -> renderActivity taskNameOfId activity state dispatch)

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
            | Ok days -> days.Value > 0.
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
                    let disabled = not daysOk
                    prop.disabled disabled
                    prop.onClick (fun _ -> AddActivityClicked Pending |> dispatch)
                    prop.children [
                        Fa.i [ Fa.Solid.Plus ] []
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