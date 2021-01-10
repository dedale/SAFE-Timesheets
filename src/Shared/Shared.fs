module Shared

module Route =
    let login = "/api/login"
    let user = "/api/user"
    let team = "/api/team"
    let costCenterCamel = "/api/cost_center"
    let costCenterSpinal = "/api/cost-center"
    let costCenter = costCenterCamel
    let task = "/api/task"

open System

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
    let value (UserId id) = id
    let route (UserId id) = sprintf "/api/user/%i" id

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

type LoggedUser =
    { Username : UserLogin
      Token : JWT
      IsAdmin : bool
      IsManager : bool }

type TeamId = TeamId of int

module TeamId =
    let value (TeamId id) = id
    let route (TeamId id) = sprintf "/api/team/%i" id

type TeamId with
    member x.Value = TeamId.value x

[<CLIMutable>]
type Team = {
    Id: TeamId
    Name: string
}

type CostCenterId = CostCenterId of int

module CostCenterId =
    let value (CostCenterId id) = id

    // https://blog.restcase.com/5-basic-rest-api-design-guidelines/
    // https://blog.octo.com/en/design-a-rest-api/#case_uri
    let routeCamel (CostCenterId id) = sprintf "/api/cost_center/%i" id
    let routeSpinal (CostCenterId id) = sprintf "/api/cost-center/%i" id
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
    let value (TaskId id) = id
    let route (TaskId id) = sprintf "/api/task/%i" id

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

[<CLIMutable>]
type TeamTask = {
    TeamId: TeamId
    TaskId: TaskId
}

type YearNumber = private YearNumber of int

module YearNumber =
    // 2007-01-01 is a Monday
    let min = 2007
    let max = 2050
    let create year =
        if year < min then
            Error (sprintf "Year should be >= %i" min)
        elif year > max then
            Error (sprintf "Year should be <= %i" max)
        else
            YearNumber year |> Ok

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

type SafeDate = private SafeDate of DateTime

module SafeDate =
    let create (date: DateTime) =
        if date.TimeOfDay <> TimeSpan.Zero then
            Error "TimeOfDay should be zero"
        else
            match YearNumber.create date.Year with
            | Error m -> Error m
            | Ok _ -> SafeDate date |> Ok

    let value (SafeDate d) = d

    let today = SafeDate DateTime.Today

    let min = SafeDate (DateTime(YearNumber.min, 1, 1))

type SafeDate with
    member x.Value = SafeDate.value x

let rec private findMonday (date: DateTime) =
    if date.DayOfWeek = DayOfWeek.Monday
    then date
    else findMonday (date.AddDays -1.)

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
        let Friday = (start date).Value.AddDays 4.
        match SafeDate.create Friday with
        | Ok d -> d
        | _ -> failwith "Invalid date"

    // https://fr.wikipedia.org/wiki/Num%C3%A9rotation_ISO_des_semaines
    // Using Jan 4 rule
    let ofDate (date: SafeDate) =
        let value = date.Value
        let Monday1, year =
            let thisYear = DateTime(value.Year, 1, 4) |> findMonday
            // First days of year before first Monday when Monday <= Jan 4
            if thisYear > value
            then DateTime(value.Year - 1, 1, 4) |> findMonday, value.Year - 1
            else
                let nextYear = DateTime(value.Year + 1, 1, 4) |> findMonday
                // Last days of year when first Monday of next year > Jan 4
                if nextYear <= value
                then nextYear, value.Year + 1
                else thisYear, value.Year
        let number = (value - Monday1).Days / 7 + 1
        { number = number; year = year }

    let create number year =
        if number <= 0
        then Error "Number should be > 0"
        elif number >= 54
        then Error "Number should be <= 53"
        else
            match SafeDate.create (DateTime(year, 1, 4).AddDays(7. * float (number - 1))) with
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
        DateTime(year + 1, 1, 4).AddDays(-7.)
        |> SafeDate.create
        |> (fun x ->
            match x with
            | Ok date -> ofDate date
            | Error m -> failwith m)

    let range (week: Week) =
        let Monday = DateTime(week.Year, 1, 4).AddDays(7. * float (week.Number - 1)) |> findMonday
        let Friday = Monday.AddDays 4.
        match SafeDate.create Monday, SafeDate.create Friday with
        | Ok M, Ok F -> M, F
        | Error m, _ -> failwith m
        | _, Error m -> failwith m

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

type WorkDays with
    member x.Value = WorkDays.value x

type ActivityId = ActivityId of int

module ActivityId =
    let value (ActivityId id) = id

type ActivityId with
    member x.Value = ActivityId.value x

// No CLIMutable attribute because DTO type is different
type Activity = {
    Id: ActivityId
    Date: SafeDate
    UserId: UserId
    TaskId: TaskId
    Days: WorkDays
    Comment: string option
}
