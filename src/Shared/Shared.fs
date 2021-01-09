module Shared

module Route =
    let login = "/api/login"

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

type UserId = UserId of Int64

module UserId =
    let value (UserId id) = id

type User = {
    Id: UserId
    Login: UserLogin
    Name: string
}

type LoggedUser =
    { Username : UserLogin
      Token : JWT
      IsAdmin : bool }
