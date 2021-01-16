module PublicHolidays.Tests

open Shared

open Expecto

open System

let holidays = testList "Holidays" [
    testList "Easter" ([
        DateTime(2006, 4, 16)
        DateTime(2007, 4, 8)
        DateTime(2008, 3, 23)
        DateTime(2009, 4, 12)
        DateTime(2010, 4, 4)
        DateTime(2011, 4, 24)
        DateTime(2012, 4, 8)
        DateTime(2013, 3, 31)
        DateTime(2014, 4, 20)
        DateTime(2015, 4, 5)
        DateTime(2016, 3, 27)
        DateTime(2017, 4, 16)
        DateTime(2018, 4, 1)
        DateTime(2019, 4, 21)
        DateTime(2020, 4, 12)
        DateTime(2021, 4, 4)
    ] |> List.map (fun easter ->
        test (easter.Year.ToString()) {
            Expect.equal (PublicHolidays.Easter easter.Year) easter ""
        })
    )

    test "2021" {
        let expected =
            [
                DateTime(2021, 1, 1)
                DateTime(2021, 4, 5)
                DateTime(2021, 5, 1)
                DateTime(2021, 5, 8)
                DateTime(2021, 5, 13)
                DateTime(2021, 5, 24)
                DateTime(2021, 7, 14)
                DateTime(2021, 8, 15)
                DateTime(2021, 11, 1)
                DateTime(2021, 11, 11)
                DateTime(2021, 12, 25)
            ]
        let computed = PublicHolidays.ofYear 2021 |> List.map (fun (d, _) -> SafeDate.value d)
        Expect.sequenceEqual computed expected ""
    }
]

let all =
    testList "PublicHolidays"
        [
            holidays
        ]
