﻿open AdventOfCode2022.Code
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
    ((D7, A), Day7.a >> string)
    ((D7, B), Day7.b >> string)
    ((D8, A), Day8.a >> string)
    ((D8, B), Day8.b >> string)
    ((D9, A), Day9.a >> string)
    ((D9, B), Day9.b >> string)
    ((D10, A), Day10.a >> string)
    ((D10, B), Day10.b >> string)
    ((D11, A), Day11.a >> string)
    ((D11, B), Day11.b >> string)
    ((D12, A), Day12.a >> string)
    ((D12, B), Day12.b >> string)
]
|> Map.ofList
|> Program.execute