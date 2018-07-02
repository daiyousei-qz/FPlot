module FPlot.Core

open Newtonsoft.Json

type PropertyGroup private(state: Map<string, obj>) = struct

    static member internal empty = PropertyGroup(Map.empty)

    member internal m.State = state

    member internal m.update key value =
        PropertyGroup(state |> Map.add key value)

    member internal m.tryFind key =
        state |> Map.tryFind key

    end

[<AutoOpen>]
module internal PropertyGroup =

    // generic update
    let update key value (data: PropertyGroup) =
        data.update key value

    // specialized update(for type inference)
    let updateObj key (value: obj) data =
        update key value data

    let updateBool key (value: bool) data =
        update key value data

    let updateInt key (value: int) data =
        update key value data

    let updateFloat key (value: float) data =
        update key value data

    let updateStr key (value: string) data =
        update key value data

    let updateSeq key (value: seq<_>) data =
        update key value data

    let updateFloatSeq key (value: seq<float>) data =
        update key value data

    let updateStrSeq key (value: seq<string>) data =
        update key value data

    let updatePropGroup key (value: PropertyGroup) data =
        update key (value.State) data

    //
    let toJsonString (data: PropertyGroup) =
        let z = 
            data.State 
            |> Map.filter (fun _ v -> v <> null)
        z |> JsonConvert.SerializeObject

let internal boxPropertyGroup (data: PropertyGroup) =
    if data.State <> Map.empty 
    then box (PropertyGroup.toJsonString data)
    else box null

// color
//

[<RequireQualifiedAccess>]
type Color =
    | Default
    | Named of string
    | RGB of int*int*int

let internal boxColor color =
    match color with
    | Color.Default ->
        null
    | Color.Named(name) ->
        name
    | Color.RGB(r, g, b) -> 
        sprintf "RGB(%d,%d,%d)" r g b

module Color =
    let red = Color.Named "red"
    let green = Color.Named "green"
    let blue = Color.Named "blue"
    let yellow = Color.Named "yellow"

// font
//

type Font =
    | Font of PropertyGroup

let internal boxFont (Font(data)) =
    box (data.State)

module Font =
    let private applyUpdate update (Font(data)) =
        data |> update |> Font

    let create fontFamily =
        PropertyGroup.empty
        |> updateStr "family" fontFamily
        |> Font

    let withSize sz font =
        font |> applyUpdate (updateInt "size" sz)

    let withColor color font =
        font |> applyUpdate (update "color" (boxColor color))
