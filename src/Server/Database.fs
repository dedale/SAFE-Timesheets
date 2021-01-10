module Database

open Shared

open FileSystem

open Dapper
open FSharp.Data.Dapper
open Microsoft.Data.Sqlite
open System

type FileConnection (sqlite) =
    new (file: FilePath) =
        let connectionString = sprintf "Data Source = %s;" file.Path
        new FileConnection(new SqliteConnection(connectionString))
    member __.Value = sqlite
    interface IDisposable with
        member __.Dispose() = sqlite.Dispose()

let boxCustom x =
    match box x with
    | :? SafeDate as d -> SafeDate.value d |> box
    | :? WorkDays as d -> WorkDays.value d |> box
    | o -> o

module TypeHandlers =

    type UserIdHandler() =
        inherit SqlMapper.TypeHandler<UserId>()

        override __.SetValue(param, id) = param.Value <- UserId.value id

        override __.Parse(value: obj) = value :?> Int64 |> int |> UserId

        static member Register() = SqlMapper.AddTypeHandler(UserIdHandler())

    type TeamIdHandler() =
        inherit SqlMapper.TypeHandler<TeamId>()

        override __.SetValue(param, id) = param.Value <- TeamId.value id

        override __.Parse(value: obj) = value :?> Int64 |> int |> TeamId

        static member Register() = SqlMapper.AddTypeHandler(TeamIdHandler())

    type TaskIdHandler() =
        inherit SqlMapper.TypeHandler<TaskId>()

        override __.SetValue(param, id) = param.Value <- TaskId.value id

        override __.Parse(value: obj) = value :?> Int64 |> int |> TaskId

        static member Register() = SqlMapper.AddTypeHandler(TaskIdHandler())

    type CostCenterIdHandler() =
        inherit SqlMapper.TypeHandler<CostCenterId>()

        override __.SetValue(param, id) = param.Value <- CostCenterId.value id

        override __.Parse(value: obj) = value :?> Int64 |> int |> CostCenterId

        static member Register() = SqlMapper.AddTypeHandler(CostCenterIdHandler())

    type ActivityIdHandler() =
        inherit SqlMapper.TypeHandler<ActivityId>()

        override __.SetValue(param, id) = param.Value <- ActivityId.value id

        override __.Parse(value: obj) = value :?> Int64 |> int |> ActivityId

        static member Register() = SqlMapper.AddTypeHandler(ActivityIdHandler())

    type UserLoginHandler() =
        inherit SqlMapper.TypeHandler<UserLogin>()

        override __.SetValue(param, login) =
            param.Value <- UserLogin.value login

        override __.Parse(value: obj) =
            let date = value :?> string
            match UserLogin.create date with
            | Ok x -> x
            | _ -> failwith (sprintf "Failed to parse '%A' as Login" value)

        static member Register() =
            SqlMapper.AddTypeHandler(UserLoginHandler())

    type SafeDateOptionHandler() =
        inherit SqlMapper.TypeHandler<SafeDate option>()

        override __.SetValue(param, date) =
            let valueOrNull =
                match date with
                | Some d -> SafeDate.value d |> box
                | None   -> null
            param.Value <- valueOrNull

        override __.Parse(value: obj) =
            if Object.ReferenceEquals(value, null) || value = box DBNull.Value
            then None
            else
                let date = value :?> string |> DateTime.Parse
                match SafeDate.create date with
                | Ok x -> Some x
                | _ -> None

        static member Register() =
            SqlMapper.AddTypeHandler(SafeDateOptionHandler())

    type SafeDateHandler() =
        inherit SqlMapper.TypeHandler<SafeDate>()

        override __.SetValue(param, date) =
            param.Value <- SafeDate.value date

        override __.Parse(value: obj) =
            let date = value :?> string |> DateTime.Parse
            match SafeDate.create date with
            | Ok x -> x
            | _ -> failwith (sprintf "Failed to parse '%A' as SafeDate" value)

        static member Register() =
            SqlMapper.AddTypeHandler(SafeDateHandler())

    type WorkDaysHandler() =
        inherit SqlMapper.TypeHandler<WorkDays>()

        override __.SetValue(param, days) =
            param.Value <- WorkDays.value days

        override __.Parse(value: obj) =
            let days = value :?> float
            match WorkDays.create days with
            | Ok x -> x
            | _ -> failwith (sprintf "Failed to parse '%A' as WorkDays" value)

        static member Register() =
            SqlMapper.AddTypeHandler(WorkDaysHandler())

    let RegisterTypes () =
        UserIdHandler.Register()
        TeamIdHandler.Register()
        TaskIdHandler.Register()
        CostCenterIdHandler.Register()
        ActivityIdHandler.Register()
        UserLoginHandler.Register()
        SafeDateHandler.Register()
        SafeDateOptionHandler.Register()
        WorkDaysHandler.Register()

module Types =

    [<CLIMutable>]
    type Activity = {
        Id: ActivityId
        Date: SafeDate
        UserId: UserId
        TaskId: TaskId
        Days: WorkDays
        Comment: string option
        // To quickly find incomplete weeks: sum days by year by week
        Year: int
        Week: int
    }

    module Activity =
        let toDomain (activity : Activity) =
            { Shared.Activity.Id = activity.Id
              Date = activity.Date
              UserId = activity.UserId
              TaskId = activity.TaskId
              Days = activity.Days
              Comment = activity.Comment }

module Queries =

    type Schema (connectionF: unit -> Connection) =

        let querySingleOptionIntAsync = querySingleOptionAsync<int> connectionF

        member __.CreateTables () = querySingleOptionIntAsync {
            script """
                DROP TABLE IF EXISTS Activity;
                CREATE TABLE Activity (
                    Id INTEGER NOT NULL PRIMARY KEY,
                    Date DATE NOT NULL,
                    -- Date VARCHAR(8) NOT NULL,
                    UserId INTEGER NOT NULL,
                    TaskId INTEGER NOT NULL,
                    Days FLOAT NOT NULL,
                    Comment VARCHAR(255) NULL,
                    Year INTEGER NOT NULL,
                    Week INTEGER NOT NULL,
                    FOREIGN KEY(UserId) REFERENCES User(Id),
                    FOREIGN KEY(TaskId) REFERENCES Task(Id)
                );

                -- a user can be member of multiple teams (JV) ?
                DROP TABLE IF EXISTS TeamMember;
                CREATE TABLE TeamMember (
                    UserId INTEGER NOT NULL,
                    TeamId INTEGER NOT NULL,
                    -- Start DATE NULL,
                    -- End DATE NULL,
                    FOREIGN KEY(UserId) REFERENCES User(Id),
                    FOREIGN KEY(TeamId) REFERENCES Team(Id)
                );

                DROP TABLE IF EXISTS TeamManager;
                CREATE TABLE TeamManager (
                    ManagerId INTEGER NOT NULL,
                    TeamId INTEGER NOT NULL,
                    -- Start DATE NULL,
                    -- End DATE NULL,
                    FOREIGN KEY(ManagerId) REFERENCES User(Id),
                    FOREIGN KEY(TeamId) REFERENCES Team(Id)
                );

                DROP TABLE IF EXISTS TeamTask;
                CREATE TABLE TeamTask (
                    TeamId INTEGER NOT NULL,
                    TaskId INTEGER NOT NULL,
                    FOREIGN KEY(TeamId) REFERENCES Team(Id),
                    FOREIGN KEY(TaskId) REFERENCES Task(Id)
                );
                
                -- TODO unique logins
                DROP TABLE IF EXISTS User;
                CREATE TABLE User (
                    Id INTEGER NOT NULL PRIMARY KEY,
                    Login VARCHAR(15) NOT NULL,
                    Name VARCHAR(255) NOT NULL,
                    Start DATE NULL,
                    End DATE NULL
                );

                DROP TABLE IF EXISTS Task;
                CREATE TABLE Task (
                    Id INTEGER NOT NULL PRIMARY KEY,
                    Name VARCHAR(255) NOT NULL,
                    CostCenterId INTEGER NOT NULL,
                    FOREIGN KEY(CostCenterId) REFERENCES CostCenter(Id)
                );

                DROP TABLE IF EXISTS Team;
                CREATE TABLE Team (
                    Id INTEGER NOT NULL PRIMARY KEY,
                    Name VARCHAR(255) NOT NULL
                );

                DROP TABLE IF EXISTS CostCenter;
                CREATE TABLE CostCenter (
                    Id INTEGER NOT NULL PRIMARY KEY,
                    Name VARCHAR(255) NOT NULL
                );
            """
        }

    type User (connectionF: unit -> Connection) =

        let querySingleIntOptionAsync = querySingleOptionAsync<int> connectionF
        let querySingleUserOptionAsync = querySingleOptionAsync<Shared.User> connectionF
        let querySeqUserAsync = querySeqAsync<Shared.User> connectionF

        member __.New login name start finish = querySingleIntOptionAsync {
            script "INSERT INTO User (Login, Name, Start, End) VALUES (@Login, @Name, @Start, @End)"
            parameters (dict ["Login", box login; "Name", box name; "Start", boxCustom start; "End", boxCustom finish])
        }

        member __.GetAll() = querySeqUserAsync {
            script "SELECT * FROM User"
        }

        member __.GetSingleByLogin login = querySingleUserOptionAsync {
            script "SELECT * FROM User WHERE Login = @Login LIMIT 1"
            parameters (dict ["Login", box login])
        }

    type Team (connectionF: unit -> Connection) =

        let querySingleIntOptionAsync = querySingleOptionAsync<int> connectionF
        let querySingleTeamOptionAsync = querySingleOptionAsync<Shared.Team> connectionF
        let querySeqTeamAsync = querySeqAsync<Shared.Team> connectionF

        member __.New name = querySingleIntOptionAsync {
            script "INSERT INTO Team (Name) VALUES (@Name)"
            parameters (dict ["Name", box name])
        }

        member __.GetAll() = querySeqTeamAsync {
            script "SELECT * FROM Team"
        }

        member __.GetSingleByName name = querySingleTeamOptionAsync {
            script "SELECT * FROM Team WHERE Name = @Name LIMIT 1"
            parameters (dict ["Name", box name])
        }

    type TeamMember (connectionF: unit -> Connection) =

        let querySingleIntOptionAsync = querySingleOptionAsync<int> connectionF
        let querySeqTeamMemberAsync = querySeqAsync<Shared.TeamMember> connectionF

        member __.New (userId: UserId) (teamId: TeamId) = querySingleIntOptionAsync {
            script "INSERT INTO TeamMember (UserId, TeamId) VALUES (@UserId, @TeamId)"
            parameters (dict ["UserId", box userId; "TeamId", box teamId])
        }

        member __.GetAll() = querySeqTeamMemberAsync {
            script "SELECT * FROM TeamMember"
        }

    type TeamManager (connectionF: unit -> Connection) =

        let querySingleIntOptionAsync = querySingleOptionAsync<int> connectionF
        let querySeqTeamMemberAsync = querySeqAsync<Shared.TeamManager> connectionF

        member __.New (teamId: TeamId) (managerId: UserId) = querySingleIntOptionAsync {
            script "INSERT INTO TeamManager (TeamId, ManagerId) VALUES (@TeamId, @ManagerId)"
            parameters (dict ["TeamId", box teamId; "ManagerId", box managerId])
        }

        member __.GetAll() = querySeqTeamMemberAsync {
            script "SELECT * FROM TeamManager"
        }

    type CostCenter (connectionF: unit -> Connection) =

        let querySingleIntOptionAsync = querySingleOptionAsync<int> connectionF
        let querySingleCostCenterOptionAsync = querySingleOptionAsync<Shared.CostCenter> connectionF
        let querySeqCostCenterAsync = querySeqAsync<Shared.CostCenter> connectionF

        member __.New name = querySingleIntOptionAsync {
            script "INSERT INTO CostCenter (Name) VALUES (@Name)"
            parameters (dict ["Name", box name])
        }

        member __.GetAll() = querySeqCostCenterAsync {
            script "SELECT * FROM CostCenter"
        }

        member __.GetSingleByName name = querySingleCostCenterOptionAsync {
            script "SELECT * FROM CostCenter WHERE Name = @Name LIMIT 1"
            parameters (dict ["Name", box name])
        }
 
    type Task (connectionF: unit -> Connection) =

        let querySingleIntOptionAsync = querySingleOptionAsync<int> connectionF
        let querySingleTaskOptionAsync = querySingleOptionAsync<Shared.Task> connectionF
        let querySeqTaskAsync = querySeqAsync<Shared.Task> connectionF

        member __.New name (costCenterId: CostCenterId) = querySingleIntOptionAsync {
            script "INSERT INTO Task (Name, CostCenterID) VALUES (@Name, @CostCenterId)"
            parameters (dict ["Name", box name; "CostCenterId", box costCenterId])
        }

        member __.GetAll() = querySeqTaskAsync {
            script "SELECT * FROM Task"
        }

        member __.GetSingleByName name = querySingleTaskOptionAsync {
            script "SELECT * FROM Task WHERE Name = @Name LIMIT 1"
            parameters (dict ["Name", box name])
        }

    type TeamTask (connectionF: unit -> Connection) =

        let querySingleIntOptionAsync = querySingleOptionAsync<int> connectionF
        let querySeqTeamMemberAsync = querySeqAsync<Shared.TeamTask> connectionF

        member __.New (teamId: TeamId) (taskId: TaskId) = querySingleIntOptionAsync {
            script "INSERT INTO TeamTask (TeamId, TaskId) VALUES (@TeamId, @TaskId)"
            parameters (dict ["TeamId", box teamId; "TaskId", box taskId])
        }

        member __.GetAll() = querySeqTeamMemberAsync {
            script "SELECT * FROM TeamTask"
        }

    type Activity (connectionF: unit -> Connection) =

        let querySingleIntOptionAsync = querySingleOptionAsync<int> connectionF
        let querySeqActivityAsync = querySeqAsync<Types.Activity> connectionF

        member __.New date (userId: UserId) (taskId: TaskId) days comment =
            let week = Week.ofDate date
            querySingleIntOptionAsync {
                script """
                    INSERT INTO Activity (Date, UserId, TaskId, Days, Comment, Year, Week)
                    VALUES (@Date, @UserId, @TaskId, @Days, @Comment, @Year, @Week)
                """
                parameters (dict [
                    "Date", boxCustom date
                    "UserId", box userId
                    "TaskId", box taskId
                    "Days", boxCustom days
                    "Comment", box comment
                    "Year", box week.Year
                    "Week", box week.Number
                ])
            }

        member __.GetAll() = querySeqActivityAsync {
            script "SELECT * FROM Activity"
        }

        member __.Get (userId: UserId) (week: Week) =
            let Monday, Friday = Week.range week
            querySeqActivityAsync {
                script "SELECT * FROM Activity WHERE UserId = @UserId AND Date >= @Monday AND DATE <= @Friday"
                parameters (dict ["UserId", box userId; "Monday", box Monday; "Friday", box Friday])
            }

        member __.Delete id = querySingleIntOptionAsync {
            script "DELETE FROM Activity WHERE Id = @Id"
            parameters (dict ["Id", box id])
        }
