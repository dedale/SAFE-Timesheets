module Database.Tests

open Shared

open FileSystem
open Database

open Shared.Tests

open Expecto
open FSharp.Data.Dapper
open System

type TempDb private (temp, connection, connectionF) =
    new () =
        let temp = new TempFile(ext0 = ".sqlite3")
        let connection = new FileConnection(temp.File)
        let connectionF () = Connection.SqliteConnection connection.Value
        new TempDb(temp :> IDisposable, connection :> IDisposable, connectionF)
    interface IDisposable with
        member __.Dispose() =
            connection.Dispose()
            temp.Dispose()
    member __.ConnectionF = connectionF
    member __.Create() = async {
        let queries = Queries.Schema connectionF
        let! _ = queries.CreateTables()
        ()
    }
    member __.NewUser login name = async {
        let queries = Queries.User connectionF
        let! newId = queries.New login name None None
        let! created = queries.GetSingleByLogin login
        return
            match created with
            | Some x ->
                Expect.equal newId (Some x.Id) "User id mismatch"
                x
            | _ -> failwith (sprintf "User '%s' not created" login)
    }
    member __.NewTeam name managerId = async {
        let queries = Queries.Team connectionF
        let! newId = queries.New name managerId
        let! created = queries.GetSingleByName name
        return
            match created with
            | Some x ->
                Expect.equal newId (Some x.Id) "Team id mismatch"
                x
            | _ -> failwith (sprintf "Team '%s' not created" name)
    }
    member __.NewCostCenter name = async {
        let queries = Queries.CostCenter connectionF
        let! newId = queries.New name
        let! created = queries.GetSingleByName name
        return
            match created with
            | Some x ->
                Expect.equal newId (Some x.Id) "CostCenter id mismatch"
                x
            | _ -> failwith (sprintf "CostCenter '%s' not created" name)
    }
    member __.NewTask name (costCenterId: CostCenterId) = async {
        let queries = Queries.Task connectionF
        let! newId = queries.New name costCenterId
        let! created = queries.GetSingleByName name
        return
            match created with
            | Some x ->
                Expect.equal newId (Some x.Id) "Task id mismatch"
                x
            | _ -> failwith (sprintf "task '%s' not created" name)
    }
    member _.NewTeamTask teamId name costCenterId = async {
        let queries = Queries.Task connectionF
        let! created = queries.NewTeamTask teamId name costCenterId
        return
            match created with
            | Some x -> x
            | _ -> failwith (sprintf "task '%s' not created" name)
    }

let user = testList "User" [
    testCaseAsync "All" <| async {
        use db = new TempDb()
        do! db.Create()
        let! john = db.NewUser "jdoe" "John DOE"
        Expect.equal john.Id (UserId 1) ""
        Expect.equal john.Login (UserLogin.create "jdoe" |> OkOrFail) ""
        Expect.equal john.Name "John DOE" ""

        let! users = (Queries.User db.ConnectionF).GetAll()
        Expect.hasCountOf users 1u (fun _ -> true) ""
    }
]

let team = testList "Team" [
    testCaseAsync "All" <| async {
        use db = new TempDb()
        do! db.Create()
        let! manager = db.NewUser "manager" "Mike ANAGER"
        let! team = db.NewTeam "R&D" manager.Id
        Expect.equal team.Name "R&D" ""

        let! teams = (Queries.Team db.ConnectionF).GetAll()
        Expect.hasCountOf teams 1u (fun _ -> true) ""
    }

    testCaseAsync "Team tasks" <| async {
        use db = new TempDb()
        do! db.Create()
        let! manager = db.NewUser "manager" "Mike ANAGER"
        let! team = db.NewTeam "R&D" manager.Id
        let! costCenter = db.NewCostCenter "Cost Center 1"
        let! taskId = db.NewTeamTask team.Id (sprintf "Team %i Task 1" team.Id.Value) costCenter.Id
        Expect.equal taskId.Value 1 ""
    }
]

let teamMember = testList "TeamMember" [
    testCaseAsync "New team member" <| async {
        use db = new TempDb()
        do! db.Create()
        let! john = db.NewUser "jdoe" "John DOE"
        let! manager = db.NewUser "manager" "Mike ANAGER"
        let! team = db.NewTeam "The Missing" manager.Id

        let queries = Queries.TeamMember db.ConnectionF
        let! _ = queries.New john.Id team.Id
        let! members = queries.GetAll()
        Expect.hasCountOf members 1u (fun _ -> true) ""
        let first = members |> Seq.head
        Expect.equal first.UserId john.Id ""
        Expect.equal first.TeamId team.Id ""
    }

    testCaseAsync "User & team must exist" <| async {
        use db = new TempDb()
        do! db.Create()
        let! manager = db.NewUser "manager" "Mike ANAGER"
        let! team = db.NewTeam "The Missing" manager.Id
        let! john = db.NewUser "jdoe" "John DOE"

        let queries = Queries.TeamMember db.ConnectionF

        Expect.throws (fun () ->
            queries.New (UserId (john.Id.Value + 1)) team.Id
            |> Async.RunSynchronously
            |> ignore) "UserId foreign key missing in TeamMember"

        Expect.throws (fun () ->
            queries.New john.Id (TeamId (team.Id.Value + 1))
            |> Async.RunSynchronously
            |> ignore) "TeamId foreign key missing in TeamMember"
    }
]

let teamManager = testList "TeamManager" [
    testCaseAsync "New team manager" <| async {
        use db = new TempDb()
        do! db.Create()
        let! manager = db.NewUser "manager" "Mike ANAGER"
        let! team = db.NewTeam "The Team" manager.Id

        let queries = Queries.TeamManager db.ConnectionF
        let! teamManagers = queries.GetAll()
        Expect.hasCountOf teamManagers 1u (fun _ -> true) ""
        let first = teamManagers |> Seq.head
        Expect.equal first.TeamId team.Id ""
        Expect.equal first.ManagerId manager.Id ""
    }

    testCaseAsync "Team & manager must exist" <| async {
        use db = new TempDb()
        do! db.Create()
        let! manager = db.NewUser "manager" "Mike ANAGER"
        let! team = db.NewTeam "The Team" manager.Id

        let queries = Queries.TeamManager db.ConnectionF

        Expect.throws (fun () ->
            queries.New (TeamId (team.Id.Value + 1)) manager.Id
            |> Async.RunSynchronously
            |> ignore) "TeamId foreign key missing in TeamManager"

        Expect.throws (fun () ->
            queries.New team.Id (UserId (manager.Id.Value + 1))
            |> Async.RunSynchronously
            |> ignore) "ManagerId foreign key missing in TeamManager"
    }
]

let costCenter = testList "CostCenter" [
    testCaseAsync "All" <| async {
        use db = new TempDb()
        do! db.Create()
        let! costCenter = db.NewCostCenter "Projects"
        Expect.equal costCenter.Name "Projects" ""

        let! costCenters = (Queries.CostCenter db.ConnectionF).GetAll()
        Expect.hasCountOf costCenters 1u (fun _ -> true) ""
    }
]

let task = testList "Task" [
    testCaseAsync "New task" <| async {
        use db = new TempDb()
        do! db.Create()
        let! costCenter = db.NewCostCenter "Projects"
        let! task = db.NewTask "Timesheet" costCenter.Id
        Expect.equal task.Name "Timesheet" ""
        Expect.equal task.CostCenterId costCenter.Id ""
    }

    testCaseAsync "Cost center must exist" <| async {
        use db = new TempDb()
        do! db.Create()
        let! costCenter = db.NewCostCenter "Projects"

        let queries = Queries.Task db.ConnectionF

        Expect.throws (fun () ->
            queries.New "Task name" (CostCenterId (costCenter.Id.Value + 1))
            |> Async.RunSynchronously
            |> ignore) "CostCenterId foreign key missing in Task"
    }
]

let activity = testList "Activity" [
    testCaseAsync "New activity" <| async {
        use db = new TempDb()
        do! db.Create()
        let! user = db.NewUser "jdoe" "John DOE"
        let! costCenter = db.NewCostCenter "Projects"
        let! task = db.NewTask "Timesheet" costCenter.Id
        let queries = Queries.Activity db.ConnectionF
        let today = DateTime(2021, 1, 4) |> SafeDate.create |> OkOrFail
        let comment = "Rewrite with F#, Fable, SAFE-Stack, Expecto, Dapper, etc."
        let! _ = queries.New user.Id today task.Id 1. comment
        let! activities = queries.GetWeek user.Id (Week.ofDate today)
        Expect.hasCountOf activities 1u (fun _ -> true) ""
        let first = activities |> Seq.head
        Expect.equal first.Date today ""
        Expect.equal first.TaskId task.Id ""
        Expect.equal first.UserId user.Id ""
        Expect.equal first.Comment comment ""
    }

    testCaseAsync "Get week" <| async {
        use db = new TempDb()
        do! db.Create()
        let! user = db.NewUser "jdoe" "John DOE"
        let! otherUser = db.NewUser "other" "Owen THER"
        let! costCenter = db.NewCostCenter "Projects"
        let! task = db.NewTask "Timesheet" costCenter.Id
        let queries = Queries.Activity db.ConnectionF
        let prevFriday = DateTime(2021, 1, 1) |> SafeDate.create |> OkOrFail 
        let! _ = queries.New user.Id prevFriday task.Id 1. "Previous week"
        let Monday = DateTime(2021, 1, 4) |> SafeDate.create |> OkOrFail 
        let! _ = queries.New user.Id Monday task.Id 1. "Monday"
        let Monday = DateTime(2021, 1, 4) |> SafeDate.create |> OkOrFail 
        let! _ = queries.New otherUser.Id Monday task.Id 1. "Monday other user"
        let Friday = DateTime(2021, 1, 8) |> SafeDate.create |> OkOrFail 
        let! _ = queries.New user.Id Friday task.Id 1. "Friday"
        let Friday = DateTime(2021, 1, 8) |> SafeDate.create |> OkOrFail 
        let! _ = queries.New otherUser.Id Friday task.Id 1. "Friday other user"
        let nextMonday = DateTime(2021, 1, 11) |> SafeDate.create |> OkOrFail 
        let! _ = queries.New user.Id nextMonday task.Id 1. "Next week"
        let week =
            match Week.create 1 2021 with
            | Ok w -> w
            | Error m -> failtest m
        let! activities = queries.GetWeek user.Id week
        let sorted = Array.ofSeq activities |> Array.sortBy (fun a -> a.Date.Value)
        Expect.hasCountOf sorted 2u (fun _ -> true) "Should take only select week & user"
        Expect.equal (sorted.[0].Comment) "Monday" ""
        Expect.equal (sorted.[1].Comment) "Friday" ""
    }

    testCaseAsync "Delete activity" <| async {
        use db = new TempDb()
        do! db.Create()
        let! user = db.NewUser "jdoe" "John DOE"
        let! costCenter = db.NewCostCenter "Projects"
        let! task = db.NewTask "Timesheet" costCenter.Id
        let queries = Queries.Activity db.ConnectionF
        let Monday = DateTime(2021, 1, 4) |> SafeDate.create |> OkOrFail 
        let! _ = queries.New user.Id Monday task.Id 1. "Monday"
        let Friday = DateTime(2021, 1, 8) |> SafeDate.create |> OkOrFail 
        let! _ = queries.New user.Id Friday task.Id 1. "Friday"
        let week =
            match Week.create 1 2021 with
            | Ok w -> w
            | Error m -> failtest m
        let! activities = queries.GetWeek user.Id week
        let first = activities |> Seq.head
        let! _ = queries.Delete first.Id
        let! activities = queries.GetWeek user.Id week
        Expect.hasCountOf activities 1u (fun _ -> true) ""
    }
]

let all =
    testList "Database"
        [
            user
            team
            teamMember
            teamManager
            costCenter
            task
            activity
        ]
