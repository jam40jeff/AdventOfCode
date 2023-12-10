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
]
|> Map.ofList
|> Program.execute