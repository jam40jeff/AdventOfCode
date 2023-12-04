open AdventOfCode2023.Code
open AdventOfCodeCommon

[
    ((D1, A), Day1.a >> string)
    ((D1, B), Day1.b >> string)
]
|> Map.ofList
|> Program.execute