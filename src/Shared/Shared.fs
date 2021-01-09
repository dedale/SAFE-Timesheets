module Shared

module Route =
    let login = "/api/login"
    let user = "/api/user"
    let team = "/api/team"

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

// Not using int64. If needed:
// https://thoth-org.github.io/Thoth.Json/
// Decoding int64
//let extra = Extra.empty |> Extra.withInt64
type UserId = UserId of int

module UserId =
    let value (UserId id) = id
    let route (UserId id) = sprintf "/api/user/%i" id

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

type Team = {
    Id: TeamId
    Name: string
}
