module AdventOfCode2021.Code.Day16

open System
open Checked
open type AdventOfCodeInput.Input2021

type private Bits = LittleEndian of bool list | BigEndian of bool list

module private Bits =
    let private getBigEndianBits = function LittleEndian bits -> bits |> List.rev | BigEndian bits -> bits
    let private getLittleEndianBits = function LittleEndian bits -> bits | BigEndian bits -> bits |> List.rev
    let length = getBigEndianBits >> List.length
    let toDecimal = getLittleEndianBits >> Seq.mapi (fun i bit -> if bit then pown 2L i else 0L) >> Seq.sum
    let combine bits = bits |> Seq.collect getBigEndianBits |> Seq.toList |> BigEndian
    let take n =
        let rec split first i second =
            if i = n then LittleEndian first,BigEndian second
            else match second with x::xs -> split (x::first) (i + 1) xs | [] -> failwith $"Can't take {n} bits."
        getBigEndianBits >> split [] 0
    let takeOne bits =
        let bits = getBigEndianBits bits
        match bits with x::xs -> x,BigEndian xs | [] -> failwith "Can't take one bit."
    let toString = getBigEndianBits >> Seq.map (fun bit -> if bit then "1" else "0") >> String.concat ""
    let fromHex =
        let bits =
            [
                ('0',"0000")
                ('1',"0001")
                ('2',"0010")
                ('3',"0011")
                ('4',"0100")
                ('5',"0101")
                ('6',"0110")
                ('7',"0111")
                ('8',"1000")
                ('9',"1001")
                ('A',"1010")
                ('B',"1011")
                ('C',"1100")
                ('D',"1101")
                ('E',"1110")
                ('F',"1111")
            ]
            |> Seq.map (fun (s,bits) -> s,bits |> Seq.map (Char.GetNumericValue >> int >> (=) 1) |> Seq.toList |> BigEndian)
            |> Map.ofSeq
        Seq.map (fun c -> bits |> Map.find c) >> combine

type private Packet = { Version : int; Contents : PacketContents }
and private PacketContents = Literal of int64 | Operator of Operator
and private Operator =
    | Sum of Packet list
    | Product of Packet list
    | Minimum of Packet list
    | Maximum of Packet list
    | GreaterThan of Packet*Packet
    | LessThan of Packet*Packet
    | EqualTo of Packet*Packet
and private OperatorLength = Bits of int | SubPackets of int

let private parsePacket bits =
    let rec parsePacket bits =
        let initialLength = bits |> Bits.length
        let versionBits,bits = bits |> Bits.take 3
        let version = versionBits |> Bits.toDecimal |> int
        let typeIdBits,bits = bits |> Bits.take 3
        let typeId = typeIdBits |> Bits.toDecimal |> int
        
        let rec parseLiteral bits =
            let rec getAllBits bits =
                let hasMore,bits = bits |> Bits.takeOne
                let numberBits,bits = bits |> Bits.take 4
                let rest,bits = if hasMore then getAllBits bits else [],bits
                numberBits::rest,bits
            let literal,bits = bits |> getAllBits
            literal |> Bits.combine |> Bits.toDecimal,bits
        
        let contents,bits =
            match typeId with
            | 4 ->
                let literal,bits = parseLiteral bits
                Literal literal,bits
            | typeId ->
                let lengthTypeId,bits = bits |> Bits.takeOne
                let lengthBits,bits = bits |> Bits.take (if lengthTypeId then 11 else 15)
                let length = lengthBits |> Bits.toDecimal |> int
                let lengthType = (if lengthTypeId then SubPackets else Bits) length
                let subPackets,bits =
                    match lengthType with
                    | SubPackets subPacketsLength ->
                        let packets,bits =
                            [1..subPacketsLength]
                            |> Seq.fold
                                (fun (packets,bits) _ ->
                                    let packet,bits,_ = parsePacket bits
                                    packet::packets,bits)
                                ([],bits)
                        packets |> List.rev,bits
                    | Bits bitsLength ->
                        let packets,bits,_ =
                            Seq.initInfinite id
                            |> Seq.scan
                                (fun o _ ->
                                    match o with
                                    | None -> None
                                    | Some (packets,bits,prevBitsConsumed) ->
                                        if prevBitsConsumed >= bitsLength then None
                                        else
                                            let packet,bits,bitsConsumed = parsePacket bits
                                            let bitsConsumed = prevBitsConsumed + bitsConsumed
                                            Some (packet::packets,bits,bitsConsumed))
                                (Some ([],bits,0))
                            |> Seq.takeWhile Option.isSome
                            |> Seq.choose id
                            |> Seq.last
                        packets |> List.rev,bits
                let getTwoPackets() =
                    match subPackets with
                    | [packet1;packet2] -> (packet1,packet2)
                    | _ -> failwithf "Expected two sub-packets."
                let operator =
                    match typeId with
                    | 0 -> Sum subPackets
                    | 1 -> Product subPackets
                    | 2 -> Minimum subPackets
                    | 3 -> Maximum subPackets
                    | 5 -> getTwoPackets() |> GreaterThan
                    | 6 -> getTwoPackets() |> LessThan
                    | 7 -> getTwoPackets() |> EqualTo
                    | typeId -> failwithf $"Unknown type ID: %i{typeId}"
                Operator operator,bits
        
        let bitsConsumed = initialLength - (bits |> Bits.length)
        { Version = version; Contents = contents },bits,bitsConsumed
    
    let packet,_,_ = parsePacket bits
    packet

let a() =
    let input = Day16 |> Bits.fromHex
    
    let rec getVersionNumberSum packet =
        let sum =
            match packet.Contents with
            | Literal _ -> 0L
            | Operator operator ->
                let subPackets =
                    match operator with
                    | Sum packets -> packets
                    | Product packets -> packets
                    | Minimum packets -> packets
                    | Maximum packets -> packets
                    | GreaterThan (packet1,packet2) -> [packet1;packet2]
                    | LessThan (packet1,packet2) -> [packet1;packet2]
                    | EqualTo (packet1,packet2) -> [packet1;packet2]
                subPackets |> List.sumBy getVersionNumberSum
        sum + (packet.Version |> int64)
    
    input |> parsePacket |> getVersionNumberSum

let b() =
    let input = Day16 |> Bits.fromHex
    
    let rec evaluate packet =
        match packet.Contents with
        | Literal literal -> literal
        | Operator operator ->
            match operator with
            | Sum packets -> packets |> Seq.sumBy evaluate
            | Product packets -> packets |> Seq.map evaluate |> Seq.reduce (*)
            | Minimum packets -> packets |> Seq.map evaluate |> Seq.min
            | Maximum packets -> packets |> Seq.map evaluate |> Seq.max
            | GreaterThan (packet1,packet2) -> if evaluate packet1 > evaluate packet2 then 1 else 0
            | LessThan (packet1,packet2) -> if evaluate packet1 < evaluate packet2 then 1 else 0
            | EqualTo (packet1,packet2) -> if evaluate packet1 = evaluate packet2 then 1 else 0
    
    input |> parsePacket |> evaluate