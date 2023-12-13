open AdventOfCode2023.Code
open AdventOfCodeCommon

[
    ((D1, A), Day1.a >> string)
    ((D1, B), Day1.b >> string)
    ((D2, A), Day2.a >> string)
    ((D2, B), Day2.b >> string)
    ((D3, A), Day3.a >> string)
    ((D3, B), Day3.b >> string)
    ((D4, A), Day4.a >> string)
    ((D4, B), Day4.b >> string)
    ((D5, A), Day5.a >> string)
    ((D5, B), Day5.b >> string)
    ((D6, A), Day6.a >> string)
    ((D6, B), Day6.b >> string)
]
|> Map.ofList
|> Program.execute