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

type LoggedUser =
    { Username : string
      Token : JWT }
