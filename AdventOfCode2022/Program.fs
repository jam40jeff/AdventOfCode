open AdventOfCode2022.Code
open AdventOfCodeCommon

[
    ((D1, A), Day1.a >> string)
    ((D1, B), Day1.b >> string)
    ((D2, A), Day2.a >> string)
    ((D2, B), Day2.b >> string)
]
|> Map.ofList
|> Program.execute