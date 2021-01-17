module Shared

module Route =
    let login = "/api/login"
    let users = "/api/users"
    let teams = "/api/teams"
    let costCentersCamel = "/api/cost_centers"
    let costCentersSpinal = "/api/cost-centers"
    let costCenters = costCentersCamel
    let tasks = "/api/tasks"
    let activities = "/api/activities"

open System
open System.Globalization

type UserCredentials =
    { Username : string
      Password : string
      // Guid used for React key attribute
      // cf. https://stackoverflow.com/a/40668682/305023
      // cf. https://reactjs.org/docs/lists-and-keys.html
      PasswordId : Guid }

// Json web token type
// cf. https://jwt.io/
type JWT = string

type UserLogin = private Login of string

module UserLogin =
    open System.Text.RegularExpressions

    let create login =
        if Regex.IsMatch(login, "^[a-z][a-z_\d]{0,13}$")
        then Login login |> Ok
        else Error (sprintf "'%s' login is invalid" login)

    let value (Login login) = login

type UserLogin with
    member x.Value = UserLogin.value x

// Not using int64. If needed:
// https://thoth-org.github.io/Thoth.Json/
// Decoding int64
//let extra = Extra.empty |> Extra.withInt64
type UserId = UserId of int

module UserId =
    let value (UserId i) = i
    let route (UserId i) = sprintf "/api/users/%i" i
    let teamRoute (UserId i) = sprintf "/api/users/%i/teams" i

type UserId with
    member x.Value = UserId.value x

// Attribute needed to avoid InvalidOperationException: A parameterless default constructor
// or one matching signature (System.Int64 Id, System.String Name, System.String Alias) is
// required for Database+Types+Bird materialization
[<CLIMutable>]
type User = {
    Id: UserId
    Login: UserLogin
    Name: string
}

type TeamId = TeamId of int

module TeamId =
    let value (TeamId i) = i
    let route (TeamId i) = sprintf "/api/teams/%i" i
    let userRoute (TeamId i) = sprintf "/api/teams/%i/users" i
    let taskRoute (TeamId i) = sprintf "/api/teams/%i/tasks" i

type TeamId with
    member x.Value = TeamId.value x

[<CLIMutable>]
type Team = {
    Id: TeamId
    Name: string
}

type LoggedUser =
    { Id : UserId
      Username : UserLogin
      Token : JWT
      IsAdmin : bool
      Teams : Team list
      ManagedTeams : Team list }

type CostCenterId = CostCenterId of int

module CostCenterId =
    let value (CostCenterId i) = i

    // https://blog.restcase.com/5-basic-rest-api-design-guidelines/
    // https://blog.octo.com/en/design-a-rest-api/#case_uri
    let routeCamel (CostCenterId i) = sprintf "/api/cost_centers/%i" i
    let routeSpinal (CostCenterId i) = sprintf "/api/cost-centers/%i" i
    let route = routeCamel

type CostCenterId with
    member x.Value = CostCenterId.value x

[<CLIMutable>]
type CostCenter = {
    Id: CostCenterId
    Name: string
}

type TaskId = TaskId of int

module TaskId =
    let value (TaskId i) = i
    let route (TaskId i) = sprintf "/api/tasks/%i" i

type TaskId with
    member x.Value = TaskId.value x

[<CLIMutable>]
type Task = {
    Id: TaskId
    Name: string
    CostCenterId: CostCenterId
}

[<CLIMutable>]
type TeamMember = {
    UserId: UserId
    TeamId: TeamId
}

[<CLIMutable>]
type TeamManager = {
    TeamId: TeamId
    ManagerId: UserId
}

type YearNumber = private YearNumber of int

module YearNumber =
    // 2007-01-01 is a Monday
    let min = 2007
    let max = 2050
    let current = YearNumber DateTime.UtcNow.Year
    let create year =
        if year < min then
            Error (sprintf "Year should be >= %i" min)
        elif year > max then
            Error (sprintf "Year should be <= %i" max)
        else
            YearNumber year |> Ok
    let value (YearNumber y) = y
    let route (YearNumber y) = sprintf "/api/years/%i" y

type YearNumber with
    member x.Value = YearNumber.value x

type MonthNumber = private MonthNumber of int

module MonthNumber =
    let create month =
        if month < 0 then
            Error "month should be >= 1"
        elif month = 0 then
            Error "months are 1-indexed, sorry"
        elif month > 12 then
            Error "month should be <= 12"
        else
            MonthNumber month |> Ok

    let ofDate (date: DateTime) = MonthNumber date.Month

    let value (MonthNumber month) = month

type MonthNumber with
    member x.Value = MonthNumber.value x

type SafeDate = private SafeDate of DateTimeOffset

module SafeDate =
    let createUnsafe (date: DateTime) =
        DateTimeOffset(date.Year, date.Month, date.Day, 0, 0, 0, TimeSpan.Zero) |> SafeDate

    let create (date: DateTime) =
        match YearNumber.create date.Year with
        | Error m -> Error m
        | Ok _ -> createUnsafe date |> Ok

    let value (SafeDate d) = d.Date
    let offset (SafeDate d) = d

    let today = createUnsafe DateTime.UtcNow

    let min = DateTime(YearNumber.min, 1, 1) |> createUnsafe

    let tryPrev (SafeDate date) = create (date.Date.AddDays(-1.))
    let tryNext (SafeDate date) = create (date.Date.AddDays(1.))

    let private format = "yyyy-MM-dd HH:mm:ss"

    let ofSqlite s =
        DateTimeOffset.Parse(s, CultureInfo.InvariantCulture).Date |> createUnsafe

    let toSqlite (SafeDate date) =
        date.ToString(format, CultureInfo.InvariantCulture)

type SafeDate with
    member x.Value = SafeDate.value x
    member x.Offset = SafeDate.offset x

[<RequireQualifiedAccess>]
type WeekAge =
    | Old
    | Current
    | Future

let rec private findMonday (date: DateTime) =
    if date.DayOfWeek = DayOfWeek.Monday
    then date
    else findMonday (date.AddDays -1.)

let private January4 year = DateTime(year, 1, 4)

type Week = private {
        number: int
        year: int
    } with
        member x.Number = x.number
        member x.Year = x.year

module Week =
    let start (date: SafeDate) =
        let Monday = findMonday date.Value
        match SafeDate.create Monday with
        | Ok d -> d
        | _ -> failwith "Invalid date"

    let finish (date: SafeDate) =
        let Friday = ((start date).Value.AddDays 4.)
        match SafeDate.create Friday with
        | Ok d -> d
        | _ -> failwith "Invalid date"

    // https://fr.wikipedia.org/wiki/Num%C3%A9rotation_ISO_des_semaines
    // Using Jan 4 rule
    let ofDate (date: SafeDate) =
        let value = date.Value
        let Monday1, year =
            let thisYear = January4 value.Year |> findMonday
            // First days of year before first Monday when Monday <= Jan 4
            if thisYear > value
            then January4 (value.Year - 1) |> findMonday, value.Year - 1
            else
                let nextYear = January4 (value.Year + 1) |> findMonday
                // Last days of year when first Monday of next year > Jan 4
                if nextYear <= value
                then nextYear, value.Year + 1
                else thisYear, value.Year
        // Round because of daylight saving time
        let number = int (Math.Round((value - Monday1).TotalDays)) / 7 + 1
        { number = number; year = year }

    let create number year =
        if number <= 0
        then Error "Number should be > 0"
        elif number >= 54
        then Error "Number should be <= 53"
        else
            let created = SafeDate.create ((January4 year).AddDays(7. * float (number - 1)))
            match created with
            | Error m -> Error m
            | Ok date ->
                let expected = ofDate date
                if expected.Number = number
                then { number = number; year = year } |> Ok
                else Error (sprintf "No week %d in year %d (should be %d)" number year expected.Number)

    let first year =
        match create 1 year with
        | Ok week -> week
        | Error m -> failwith m
    let last year =
        (January4 (year + 1)).AddDays(-7.)
        |> SafeDate.create
        |> (fun x ->
            match x with
            | Ok date -> ofDate date
            | Error m -> failwith m)

    let Monday (week: Week) =
        (January4 week.Year).AddDays(7. * float (week.Number - 1))
        |> findMonday
        |> SafeDate.create
        |> (fun d ->
            match d with
            | Ok dd -> dd
            | Error m -> failwith m)

    let range (week: Week) =
        let Monday = Monday week
        let Friday =
            Monday.Value.AddDays 4.
            |> (fun d -> d.Date)
            |> SafeDate.createUnsafe
        Monday, Friday

    let current = ofDate SafeDate.today

    let activityRoute (UserId i) (week: Week) = sprintf "/api/users/%i/activities/%i/%i" i week.Year week.Number
    let yearRoute (UserId i) (YearNumber y) = sprintf "/api/users/%i/years/%i" i y

    let age (week: Week) =
        let current = ofDate SafeDate.today
        if week.Year < current.Year
        then WeekAge.Old
        elif week.Year = current.Year && week.Number < current.Number
        then WeekAge.Old
        elif week.Year = current.Year && week.Number = current.Number
        then WeekAge.Current
        else WeekAge.Future

[<RequireQualifiedAccess>]
type WeekStatus =
    | Full
    | Incomplete of WeekAge

// List of weeks grouped by months
type MonthWeeks = private Weeks of (MonthNumber * (Week * WeekStatus option) list) list

module MonthWeeks =
    let create year (tryIsFull: Week -> WeekStatus option) =
        let first = Week.first year
        let last = Week.last year
        let monthWeeks =
            { first.Number .. last.Number }
            |> Seq.map (fun n ->
                let week = Week.create n year
                match week with
                | Ok w ->
                    w, Week.range w |> fst
                | Error m -> failwith m)
            |> Seq.groupBy (fun (_, date) ->
                let d = date.Value
                DateTime(d.Year, d.Month, 1))
            |> Seq.map (fun (month1st, weeks) ->
                MonthNumber.ofDate month1st,
                Seq.map fst weeks
                |> Seq.map (fun w -> w, tryIsFull w)
                |> List.ofSeq)
            |> List.ofSeq
        Weeks monthWeeks

    let value (Weeks weeks) = weeks

    let update (week: Week) (status: WeekStatus option) (Weeks monthWeeks) =
        let weekMonth =
            let Monday = Week.Monday week
            let date = Monday.Value
            DateTime(date.Year, date.Month, 1) |> MonthNumber.ofDate
        monthWeeks
        |> List.map (fun (month, weeks) ->
            if weekMonth.Value = month.Value
            then month, weeks |> List.map (fun (w, s) ->
                if w.Number = week.Number
                then w, status
                else w, s)
            else month, weeks)
        |> Weeks

type WorkDays = private WorkDays of float

module WorkDays =
    let create (days: float) =
        if days <= 0. then
            Error "days should be > 0"
        elif days > 5. then
            Error "days should be <= 5"
        else
            WorkDays(days) |> Ok

    let value (WorkDays d) = d
    let zero = WorkDays 0.
    let one = WorkDays 1.
    let max = WorkDays 5.

type WorkDays with
    member x.Value = WorkDays.value x

type ActivityId = ActivityId of int

module ActivityId =
    let value (ActivityId i) = i
    let route (ActivityId i) = sprintf "/api/activities/%i" i

type ActivityId with
    member x.Value = ActivityId.value x

[<CLIMutable>]
type Activity = {
    Id: ActivityId
    UserId: UserId
    Date: SafeDate
    TaskId: TaskId
    Days: WorkDays
    Comment: string
}

type NewActivity = {
    UserId : UserId
    Date : SafeDate
    TaskId : TaskId
    Days : WorkDays
    Comment : string
}

type WeekDays = {
    Week : Week
    Days : WorkDays
}