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
        let schema = Queries.Schema connectionF
        let! _ = schema.CreateTables()
        ()
    }
    member __.NewUser login name = async {
        let user = Queries.User connectionF
        let! id = user.New login name None None
        let! created = user.GetSingleByLogin login
        return
            match created with
            | Some x ->
                Expect.equal id (Some x.Id) "User id mismatch"
                x
            | _ -> failwith (sprintf "User '%s' not created" login)
    }
    member __.NewTeam name = async {
        let team = Queries.Team connectionF
        let! id = team.New name
        let! created = team.GetSingleByName name
        return
            match created with
            | Some x ->
                Expect.equal id (Some x.Id) "Team id mismatch"
                x
            | _ -> failwith (sprintf "Team '%s' not created" name)
    }
    member __.NewCostCenter name = async {
        let costCenter = Queries.CostCenter connectionF
        let! id = costCenter.New name
        let! created = costCenter.GetSingleByName name
        return
            match created with
            | Some x ->
                Expect.equal id (Some x.Id) "CostCenter id mismatch"
                x
            | _ -> failwith (sprintf "CostCenter '%s' not created" name)
    }
    member __.NewTask name (costCenterId: CostCenterId) = async {
        let task = Queries.Task connectionF
        let! id = task.New name costCenterId
        let! created = task.GetSingleByName name
        return
            match created with
            | Some x ->
                Expect.equal id (Some x.Id) "Task id mismatch"
                x
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
        let! team = db.NewTeam "R&D"
        Expect.equal team.Name "R&D" ""

        let! teams = (Queries.Team db.ConnectionF).GetAll()
        Expect.hasCountOf teams 1u (fun _ -> true) ""
    }
]

let teamMember = testList "TeamMember" [
    testCaseAsync "New team member" <| async {
        use db = new TempDb()
        do! db.Create()
        let! john = db.NewUser "jdoe" "John DOE"
        let! team = db.NewTeam "The Missing"

        let teamMember = Queries.TeamMember db.ConnectionF
        let! _ = teamMember.New john.Id team.Id
        let! members = teamMember.GetAll()
        Expect.hasCountOf members 1u (fun _ -> true) ""
        let first = members |> Seq.head
        Expect.equal first.UserId john.Id ""
        Expect.equal first.TeamId team.Id ""
    }

    testCaseAsync "User & team must exist" <| async {
        use db = new TempDb()
        do! db.Create()
        let! john = db.NewUser "jdoe" "John DOE"
        let! team = db.NewTeam "The Missing"

        let teamMember = Queries.TeamMember db.ConnectionF

        Expect.throws (fun () ->
            teamMember.New (UserId (john.Id.Value + 1)) team.Id
            |> Async.RunSynchronously
            |> ignore) "UserId foreign key missing in TeamMember"

        Expect.throws (fun () ->
            teamMember.New john.Id (TeamId (team.Id.Value + 1))
            |> Async.RunSynchronously
            |> ignore) "TeamId foreign key missing in TeamMember"
    }
]

let teamManager = testList "TeamManager" [
    testCaseAsync "New team manager" <| async {
        use db = new TempDb()
        do! db.Create()
        let! team = db.NewTeam "The Team"
        let! manager = db.NewUser "manager" "Mike ANAGER"

        let teamManager = Queries.TeamManager db.ConnectionF
        let! _ = teamManager.New team.Id manager.Id
        let! teamManagers = teamManager.GetAll()
        Expect.hasCountOf teamManagers 1u (fun _ -> true) ""
        let first = teamManagers |> Seq.head
        Expect.equal first.TeamId team.Id ""
        Expect.equal first.ManagerId manager.Id ""
    }

    testCaseAsync "Team & manager must exist" <| async {
        use db = new TempDb()
        do! db.Create()
        let! team = db.NewTeam "The Team"
        let! manager = db.NewUser "manager" "Mike ANAGER"

        let teamManager = Queries.TeamManager db.ConnectionF

        Expect.throws (fun () ->
            teamManager.New (TeamId (team.Id.Value + 1)) manager.Id
            |> Async.RunSynchronously
            |> ignore) "TeamId foreign key missing in TeamManager"

        Expect.throws (fun () ->
            teamManager.New team.Id (UserId (manager.Id.Value + 1))
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

        let task = Queries.Task db.ConnectionF

        Expect.throws (fun () ->
            task.New "Task name" (CostCenterId (costCenter.Id.Value + 1))
            |> Async.RunSynchronously
            |> ignore) "CostCenterId foreign key missing in Task"
    }
]

let teamTask = testList "TeamTask" [
    testCaseAsync "New team task" <| async {
        use db = new TempDb()
        do! db.Create()
        let! team = db.NewTeam "The Team"
        let! costCenter = db.NewCostCenter "Projects"
        let! task = db.NewTask "Timesheet" costCenter.Id

        let teamTask = Queries.TeamTask db.ConnectionF
        let! _ = teamTask.New team.Id task.Id
        let! teamTasks = teamTask.GetAll()
        Expect.hasCountOf teamTasks 1u (fun _ -> true) ""
        let first = teamTasks |> Seq.head
        Expect.equal first.TeamId team.Id ""
        Expect.equal first.TaskId task.Id ""
    }

    testCaseAsync "Team & task must exist" <| async {
        use db = new TempDb()
        do! db.Create()
        let! team = db.NewTeam "The Team"
        let! costCenter = db.NewCostCenter "Projects"
        let! task = db.NewTask "Timesheet" costCenter.Id

        let teamTask = Queries.TeamTask db.ConnectionF

        Expect.throws (fun () ->
            teamTask.New (TeamId (team.Id.Value + 1)) task.Id
            |> Async.RunSynchronously
            |> ignore) "TeamId foreign key missing in TeamTask"

        Expect.throws (fun () ->
            teamTask.New team.Id (TaskId (task.Id.Value + 1))
            |> Async.RunSynchronously
            |> ignore) "TaskId foreign key missing in TeamTask"
    }
]

let activity = testList "Activity" [
    testCaseAsync "New activity" <| async {
        use db = new TempDb()
        do! db.Create()
        let! user = db.NewUser "jdoe" "John DOE"
        let! costCenter = db.NewCostCenter "Projects"
        let! task = db.NewTask "Timesheet" costCenter.Id
        let activity = Queries.Activity db.ConnectionF
        let today = SafeDate.today
        let comment = "Rewrite with F#, Fable, SAFE-Stack, Expecto, Dapper, etc."
        let! _ = activity.New today user.Id task.Id 1. comment
        let! activities = activity.GetAll()
        Expect.hasCountOf activities 1u (fun _ -> true) ""
        let first = activities |> Seq.head
        Expect.equal first.Date today ""
        Expect.equal first.TaskId task.Id ""
        Expect.equal first.UserId user.Id ""
        Expect.equal first.Comment (Some comment) ""
    }

    testCaseAsync "Get week" <| async {
        use db = new TempDb()
        do! db.Create()
        let! user = db.NewUser "jdoe" "John DOE"
        let! otherUser = db.NewUser "other" "Owen THER"
        let! costCenter = db.NewCostCenter "Projects"
        let! task = db.NewTask "Timesheet" costCenter.Id
        let activity = Queries.Activity db.ConnectionF
        let prevFriday = DateTime(2021, 1, 1) |> SafeDate.create |> OkOrFail 
        let! _ = activity.New prevFriday user.Id task.Id 1. "Previous week"
        let Monday = DateTime(2021, 1, 4) |> SafeDate.create |> OkOrFail 
        let! _ = activity.New Monday user.Id task.Id 1. "Monday"
        let Monday = DateTime(2021, 1, 4) |> SafeDate.create |> OkOrFail 
        let! _ = activity.New Monday otherUser.Id task.Id 1. "Monday other user"
        let Friday = DateTime(2021, 1, 8) |> SafeDate.create |> OkOrFail 
        let! _ = activity.New Friday user.Id task.Id 1. "Friday"
        let Friday = DateTime(2021, 1, 8) |> SafeDate.create |> OkOrFail 
        let! _ = activity.New Friday otherUser.Id task.Id 1. "Friday other user"
        let nextMonday = DateTime(2021, 1, 11) |> SafeDate.create |> OkOrFail 
        let! _ = activity.New nextMonday user.Id task.Id 1. "Next week"
        let week =
            match Week.create 1 2021 with
            | Ok w -> w
            | Error m -> failtest m
        let! activities = activity.Get user.Id week
        let sorted = Array.ofSeq activities |> Array.sortBy (fun a -> a.Date.Value)
        Expect.hasCountOf sorted 2u (fun _ -> true) "Should take only select week & user"
        Expect.equal (sorted.[0].Comment) (Some "Monday") ""
        Expect.equal (sorted.[1].Comment) (Some "Friday") ""
    }

    testCaseAsync "Delete activity" <| async {
        use db = new TempDb()
        do! db.Create()
        let! user = db.NewUser "jdoe" "John DOE"
        let! costCenter = db.NewCostCenter "Projects"
        let! task = db.NewTask "Timesheet" costCenter.Id
        let activity = Queries.Activity db.ConnectionF
        let Monday = DateTime(2021, 1, 4) |> SafeDate.create |> OkOrFail 
        let! _ = activity.New Monday user.Id task.Id 1. "Monday"
        let Friday = DateTime(2021, 1, 8) |> SafeDate.create |> OkOrFail 
        let! _ = activity.New Friday user.Id task.Id 1. "Friday"
        let week =
            match Week.create 1 2021 with
            | Ok w -> w
            | Error m -> failtest m
        let! activities = activity.Get user.Id week
        let first = activities |> Seq.head
        let! _ = activity.Delete first.Id
        let! activities = activity.Get user.Id week
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
            teamTask
            activity
        ]
