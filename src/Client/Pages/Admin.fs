module Pages.Admin

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

type AddUserResult = Result<User, string>

type DelUserResult = Result<bool, string>

type State =
    { User : LoggedUser
      NewUsername : string
      Users : User list
      TryAddUser : Deferred<AddUserResult>
      TryDelUser : Deferred<DelUserResult>
    }

type Msg =
    | NewUsernameChanged of string
    | AddUserClicked of PromiseStatus<AddUserResult>
    | DelUserClicked of UserId * PromiseStatus<DelUserResult>

let (|UserAdded|_|) = function
    | AddUserClicked (Completed (Ok user)) -> Some user
    | _ -> None

let dummyUsers =
    let create id login name =
        match UserLogin.create login with
        | Ok l ->
            { User.Id = UserId id
              Login = l
              Name = name }
        | Error m -> failwith m
    [ create 1 "jdoe123" "John DOE"
      create 2 "vphilipp123456" "Vianney PHILIPPE"
    ]

let addUser (username: string) = promise {
    let body = Encode.Auto.toString(0, username)
    let props = [
        Method HttpMethod.POST
        Fetch.requestHeaders [ ContentType "application/json" ]
        Body !^body
    ]
    let! res = Fetch.fetch Route.user props
    let! txt = res.text()
    return Decode.Auto.unsafeFromString<User> txt |> (Ok >> Completed)
}

let delUser (id: UserId) = promise {
    let props = [
        Method HttpMethod.DELETE
    ]
    let! res = Fetch.fetch (UserId.route id) props
    return res.Ok |> (Ok >> Completed)
}

let init (user: LoggedUser) =
    { User = user
      NewUsername = ""
      Users = dummyUsers
      TryAddUser = NotStarted
      TryDelUser = NotStarted }, Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | NewUsernameChanged user ->
        { state with NewUsername = user }, Cmd.none

    | AddUserClicked Pending ->
        let nextState = { state with TryAddUser = InProgress }
        let OnFailed (e: exn) = Error e.Message |> (Completed >> AddUserClicked)
        let nextCmd = Cmd.OfPromise.either addUser state.NewUsername AddUserClicked OnFailed
        nextState, nextCmd

    | AddUserClicked (Completed addResult) ->
        let nextState = { state with TryAddUser = Resolved addResult }
        match addResult with
        | Ok user ->
            { nextState with Users = user :: nextState.Users; NewUsername = "" }, Cmd.none
        | _ ->
            nextState, Cmd.none

    | DelUserClicked (id, Pending) ->
        let toMsg x = DelUserClicked (id, x)
        let nextState = { state with TryDelUser = InProgress }
        let OnFailed (e: exn) = Error e.Message |> Completed |> toMsg
        let nextCmd = Cmd.OfPromise.either delUser id toMsg OnFailed
        nextState, nextCmd

    | DelUserClicked (id, Completed delResult) ->
        let nextState = { state with TryDelUser = Resolved delResult }
        match delResult with
        | Ok deleted ->
            if deleted then
                { nextState with Users = nextState.Users |> List.where (fun user -> user.Id <> id) }, Cmd.none
            else
                nextState, Cmd.none
        | _ ->
            nextState, Cmd.none

let centered (children: ReactElement list) =
    Html.div [
        prop.style [
            style.margin.auto
            style.textAlign.center
            style.padding 20
            style.width (length.percent 100)
        ]
        prop.children children
    ]

let centerColumn (elements : ReactElement list) =
    Bulma.columns [
        Bulma.column []
        Bulma.column elements
        Bulma.column []
    ]

let renderUser (state: State) (dispatch: Msg -> unit) (user: User) =
    Bulma.columns [
        columns.isVCentered
        prop.children [
            Bulma.column [
                column.isOneThird
                prop.children [
                    Bulma.subtitle.p (UserLogin.value user.Login)
                ]
            ]
            Bulma.column [
                Bulma.subtitle.p user.Name
            ]
            Bulma.column [
                column.isNarrow
                prop.children [
                    Bulma.buttons [
                        Bulma.button.button [
                            color.isDanger
                            prop.onClick (fun _ -> (user.Id, Pending) |> DelUserClicked |> dispatch)
                            prop.children [
                                Fa.i [ Fa.Solid.Times ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderAddUser (state: State) (dispatch: Msg -> unit) =
    let error =
        if String.IsNullOrWhiteSpace state.NewUsername
        then Some ""
        elif state.Users |> List.exists (fun user -> UserLogin.value user.Login = state.NewUsername.Trim())
        then Some "User already exists."
        else None
    Html.div [
        prop.style [
            style.paddingBottom 30
        ]
        prop.children [
            Bulma.field.div [
                field.hasAddons
                prop.children [
                    Bulma.control.div [
                        control.hasIconsLeft
                        if Option.isSome error then control.hasIconsRight
                        prop.children [
                            Bulma.input.text [
                                prop.placeholder "Username"
                                prop.valueOrDefault state.NewUsername
                                prop.onChange (NewUsernameChanged >> dispatch)
                            ]
                            Bulma.icon [
                                icon.isSmall; icon.isLeft
                                prop.children [
                                    Fa.i [ Fa.Solid.User ] []
                                ]
                            ]
                            if Option.isSome error then
                                Bulma.icon [
                                    icon.isSmall; icon.isRight
                                    prop.children [
                                        Fa.i [ Fa.Solid.ExclamationTriangle ] []
                                    ]
                                ]
                        ]
                    ]
                    Bulma.control.div [
                        Bulma.button.button [
                            color.isPrimary
                            prop.disabled (Option.isSome error)
                            prop.onClick (fun _ -> dispatch (AddUserClicked Pending))
                            prop.children [
                                Fa.i [ Fa.Solid.Plus ] []
                            ]
                        ]
                    ]
                ]
            ]
            match error with
            | Some msg ->
                if not (String.IsNullOrEmpty msg) then
                    Bulma.help [
                        color.isDanger
                        prop.text msg
                    ]
            | None -> Html.none
        ]
    ]

let renderUsers (state: State) (dispatch: Msg -> unit) =
    Bulma.box (
        state.Users |> List.map (renderUser state dispatch)
    )

let render state dispatch =
    centered [
        Html.h1 [
            Html.strong ((UserLogin.value state.User.Username).ToUpper())
        ]

        Html.p "This is the admin page"

        Html.div [
            Bulma.button.a [
                color.isInfo
                prop.style [ style.margin 5 ]
                prop.href (Router.format(""))
                prop.text "Home"
            ]
            Bulma.button.a [
                color.isInfo
                prop.style [ style.margin 5 ]
                prop.href (Router.format("logout"))
                prop.text "Logout"
            ]
        ]

        Bulma.tabs [
            Html.ul [
                Html.li [
                    tab.isActive
                    prop.children [
                        Html.a [ prop.text "Users" ]
                    ]
                ]
                Html.li [
                    Html.a [ prop.text "Others" ]
                ]
            ]
        ]

        Html.div [
            prop.style [
                style.width 600
                style.textAlign.left
            ]
            prop.children [
                renderAddUser state dispatch
                renderUsers state dispatch
            ]
        ]
    ]