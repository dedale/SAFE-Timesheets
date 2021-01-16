module PublicHolidays

open Shared

open System

// TODO
// - USA: https://en.wikipedia.org/wiki/Holidays_with_paid_time_off_in_the_United_States
// - Marrocco: https://fr.wikipedia.org/wiki/F%C3%AAtes_et_jours_f%C3%A9ri%C3%A9s_au_Maroc

// https://fr.wikipedia.org/wiki/Calcul_de_la_date_de_P%C3%A2ques
let Easter year =
    let n = year % 19
    let c, u = Math.DivRem(year, 100)
    let s, t = Math.DivRem(c, 4)
    let p = (c + 8) / 25
    let q = (c - p + 1) / 3
    let e = (19 * n + c - s - q + 15) % 30
    let b, d = Math.DivRem(u, 4)
    let L = (2 * t + 2 * b - e - d + 32) % 7
    let h = (n + 11 * e + 22 * L) / 451
    let (m, j) = Math.DivRem(e + L - 7 * h + 114, 31)
    DateTime(year, m, j + 1)
    
// https://en.wikipedia.org/wiki/Public_holidays_in_France
let ofYear year =
    let EasterSunday = Easter year
    [
        DateTime(year, 1, 1), "New Year's Day"
        EasterSunday.AddDays(1.), "Easter Monday"
        DateTime(year, 5, 1), "Labour Day"
        DateTime(year, 5, 8), "Victory Day"
        EasterSunday.AddDays(39.), "Ascension Day"
        EasterSunday.AddDays(50.), "Whit Monday"
        DateTime(year, 7, 14), "Bastille Day"
        DateTime(year, 8, 15), "Assumption of Mary"
        DateTime(year, 11, 1), "All Saints' Day"
        DateTime(year, 11, 11), "Armistice Day"
        DateTime(year, 12, 25), "Christmas Day"
    ]
    |> List.map (fun (d, name) -> SafeDate.create d, name)
    |> List.map (fun (r, name) ->
        match r with
        | Ok d -> d, name
        | Error m -> failwith m)
