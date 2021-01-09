// cf. SAFE-BookStore

/// Login web part and functions for API web part request authorisation with JWT.
module Handlers.Login

module JsonWebToken =
    open System
    open System.IdentityModel.Tokens.Jwt
    //open System.IO
    open System.Security.Claims

    let private createPassPhrase() =
        let crypto = System.Security.Cryptography.RandomNumberGenerator.Create()
        let randomNumber = Array.init 32 byte
        crypto.GetBytes(randomNumber)
        randomNumber

    // TODO improve this token file (should not exist?)
    let secret =
        createPassPhrase()
        |> System.Text.Encoding.UTF8.GetString
        //let fi = FileInfo("./temp/token.txt")
        //if not fi.Exists then
        //    let passPhrase = createPassPhrase()
        //    if not fi.Directory.Exists then
        //        fi.Directory.Create()
        //    File.WriteAllBytes(fi.FullName, passPhrase)
        //File.ReadAllBytes(fi.FullName)
        //|> System.Text.Encoding.UTF8.GetString

    let issuer = "timesheets.io"

    let private algorithm = Microsoft.IdentityModel.Tokens.SecurityAlgorithms.HmacSha256

    let generateToken username =
        [ Claim(JwtRegisteredClaimNames.Sub, username);
          Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) ]
        |> Saturn.Auth.generateJWT (secret, algorithm) issuer (DateTime.UtcNow.AddHours(1.0))

open Shared

open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn.ControllerHelpers

let validate (credentials: Shared.UserCredentials) =
    // TODO check that user exist in repository
    match UserLogin.create credentials.Username with
    | Ok login ->
        { Username = login
          Token = JsonWebToken.generateToken credentials.Username
          IsAdmin = credentials.Username = "admin"
        } |> Ok
    | Error m -> Error m

/// Authenticates a user and returns a token in the HTTP body.
let login (next : HttpFunc) (ctx : HttpContext) = task {
    let! credentials = ctx.BindJsonAsync<Shared.UserCredentials>()
    return!
        match validate credentials with
        | Ok user ->
            ctx.WriteJsonAsync user
        | Error m ->
            Response.unauthorized ctx "Bearer" "" (sprintf "User '%s' can't be logged in: %s." credentials.Username m)
}
