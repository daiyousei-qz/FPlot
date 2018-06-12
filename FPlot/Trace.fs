module FPlot.Trace

open System
open FPlot.Core

[<RequireQualifiedAccess>]
type TraceVisibility =
    | Yes
    | No
    | LegendOnly

let private boxTraceVisibility vis =
    match vis with
    | TraceVisibility.Yes           -> box true
    | TraceVisibility.No            -> box false
    | TraceVisibility.LegendOnly    -> box "legendonly"

[<RequireQualifiedAccess>]
type TraceFill =
    | None
    | ToZeroY
    | ToZeroX
    | ToNextY
    | ToNextX
    | ToSelf
    | ToNext

let private boxTraceFill fill =
    match fill with
    | TraceFill.None    -> "none"
    | TraceFill.ToZeroY -> "tozeroy"
    | TraceFill.ToZeroX -> "tozerox"
    | TraceFill.ToNextY -> "tonexty"
    | TraceFill.ToNextX -> "tonextx"
    | TraceFill.ToSelf  -> "toself"
    | TraceFill.ToNext  -> "tonext"
    |> box

[<RequireQualifiedAccess>]
type TextPosition =
    | TopLeft
    | TopCenter
    | TopRight
    | MiddleLeft
    | MiddleCenter
    | MiddleRight
    | BottomLeft
    | BottomCenter
    | BottomRight

let private boxTextPosition pos =
    match pos with
    | TextPosition.TopLeft      -> "top left"
    | TextPosition.TopCenter    -> "top center"
    | TextPosition.TopRight     -> "top right"
    | TextPosition.MiddleLeft   -> "middle left"
    | TextPosition.MiddleCenter -> "middle center"
    | TextPosition.MiddleRight  -> "middle right"
    | TextPosition.BottomLeft   -> "bottom left"
    | TextPosition.BottomCenter -> "bottom center"
    | TextPosition.BottomRight  -> "bottom right"
    |> box


// [Marker]
// represents style of the major element of the trace
//
type Marker =
    | Marker of PropertyGroup

let private boxMarker (Marker(data)) =
    box (data.State)

[<RequireQualifiedAccess>]
type MarkerSymbol =
    | Circle
    | Square
    | Diamond
    | Cross
    | X
    | Triangle
    | TriangleDown
    | TriangleLeft
    | TriangleRight
    | TriangleNE
    | TriangleSE
    | TriangleSW
    | TriangleNW
    | Pantagon
    | Hexagon
    | Octagon
    | Star
    | Hexagram
    | StarTriangle
    | StarSquare
    | StarDiamond
    | DiamondTall
    | DiamondWide
    | HourGlass

[<RequireQualifiedAccess>]
type MarkerStyle =
    | Open

module Marker =
    let private applyUpdate update (Marker(data)) =
        data |> update |> Marker

    let defaultMarker =
        PropertyGroup.empty |> Marker

    let withOpacity opacity marker =
        marker |> applyUpdate (updateInt "opacity" opacity)

    let withSize sz marker =
        marker |> applyUpdate (updateInt "size" sz)

    let withMaxDisplayed num marker =
        marker |> applyUpdate (updateInt "maxdisplayed" num)

    let withColor color marker =
        marker |> applyUpdate (update "color" (boxColor color))

// [Line]
// represents style of some line element in the graph
//
type Line =
    | Line of PropertyGroup

let private boxLine (Line(data)) =
    box (data.State)

[<RequireQualifiedAccess>]
type LineShape =
    | Linear
    | Spline
    | HV
    | VH
    | HVH
    | VHV

let private boxLineShape shape =
    match shape with
    | LineShape.Linear  -> "linear"
    | LineShape.Spline  -> "spline"
    | LineShape.HV      -> "hv"
    | LineShape.VH      -> "vh"
    | LineShape.HVH     -> "hvh"
    | LineShape.VHV     -> "vhv"
    |> box

[<RequireQualifiedAccess>]
type LineDash =
    | Solid
    | Dot
    | Dash
    | LongDash
    | DashDot
    | LongDashDot
    | CustomDash of int list

let private boxLineDash dash =
    match dash with
    | LineDash.Solid            -> "solid"
    | LineDash.Dot              -> "dot"
    | LineDash.Dash             -> "dash"
    | LineDash.LongDash         -> "longdash"
    | LineDash.DashDot          -> "dashdot"
    | LineDash.LongDashDot      -> "longdashdot"
    | LineDash.CustomDash(_)    -> 
        failwith "not implemented"
    |> box

module Line =
    let private applyUpdate update (Line(data)) =
        data |> update |> Line

    let defaultLine =
        PropertyGroup.empty |> Line

    let withWidth width line =
        line |> applyUpdate (updateInt "width" width)

    let withColor color line =
        line |> applyUpdate (update "color" (boxColor color))

    let withShape shape line =
        line |> applyUpdate (update "shape" (boxLineShape shape))

    let withDash dash line =
        line |> applyUpdate (update "dash" (boxLineDash dash))

type ScatterMode =
    { ShowLine: bool
      ShowMarker: bool
      ShowText: bool }

    static member none =
        { ShowLine = false; ShowMarker = false; ShowText = false }

let private boxScatterMode mode =
    seq { if mode.ShowLine   then yield "line"
          if mode.ShowMarker then yield "marker"
          if mode.ShowText   then yield "text" }
    |> fun xs -> String.Join("+", xs)
    |> function | "" -> "none" | s -> s
    |> box

type Scatter =
    | Scatter of PropertyGroup

type Bar =
    | Bar of PropertyGroup
    
[<RequireQualifiedAccess>]
type Trace =
    | TraceScatter of Scatter
    | TraceBar of Bar


[<AutoOpen>]
module private TraceImpl =
    let applyVisibility apply vis =
        apply (update "visible" (boxTraceVisibility vis))

    let applyShowLegend apply toggle =
        apply (updateBool "showlegend" toggle)

    let applyLegendGroup apply groupName =
        apply (updateBool "legendgroup" groupName)

    let applyOpacity apply opacity =
        apply (updateFloat "opacity" opacity)
   
    let applyName apply name =
        apply (updateStr "name" name)

    let applyXs apply xs =
        apply (updateSeq "x" xs)

    let applyYs apply ys =
        apply (updateSeq "y" ys)

    let applyText apply textList =
        apply (updateStrSeq "text" textList)

    let applyHoverText apply textList =
        apply (updateStrSeq "hovertext" textList)

    let applyScatterMode apply mode =
        apply (update "mode" (boxScatterMode mode))

    let applyLine apply marker =
        apply (update "line" (boxLine marker))

    let applyConnectGaps apply toggle =
        apply (updateBool "connectgaps" toggle)

    let applyFill apply fill =
        apply (update "fill" (boxTraceFill fill))

    let applyFillColor apply color =
        apply (update "fillcolor" (boxColor color))

    let applyMarker apply marker =
        apply (update "marker" (boxMarker marker))

    let applyTextPosition apply pos =
        apply (update "textposition" (boxTextPosition pos))

    let applyTextFont apply font =
        apply (update "textfont" (boxFont font))

// Scatter
//
module Scatter =
    let private applyUpdate update (Scatter(data)) =
        data |> update |> Scatter

    let create xs ys =
        PropertyGroup.empty
        |> updateStr "type" "scatter"
        |> updateSeq "x" xs
        |> updateSeq "y" ys
        |> Scatter

    let createWithDefaultX ys =
        PropertyGroup.empty
        |> updateStr "type" "scatter"
        |> updateSeq "y" ys
        |> Scatter

    let withVisibility vis scatter =
        applyVisibility applyUpdate vis scatter

    let withShowLegend toggle scatter =
        applyShowLegend applyUpdate toggle scatter

    let withLegendGroup group scatter =
        applyShowLegend applyUpdate group scatter

    let withOpacity opacity scatter =
        applyOpacity applyUpdate opacity scatter

    let withName name scatter =
        applyName applyUpdate name scatter

    let withText textList scatter =
        applyText applyUpdate textList scatter

    let withHoverText textList scatter =
        applyHoverText applyUpdate textList scatter

    let withMode mode scatter =
        applyScatterMode applyUpdate mode scatter

    let withLine line scatter =
        applyLine applyUpdate line scatter

    let withFill fill scatter =
        applyFill applyUpdate fill scatter

    let withFillColor color scatter =
        applyFillColor applyUpdate color scatter

    let withMarker marker scatter =
        applyMarker applyUpdate marker scatter

    // selected

    // unselected

    let withTextPosition pos scatter =
        applyTextPosition applyUpdate pos scatter

    let withTextFont font scatter =
        applyTextFont applyUpdate font scatter

    let toTrace scatter =
        Trace.TraceScatter scatter

// Bar
//
module Bar =
    let private applyUpdate update (Bar(data)) =
        data |> update |> Bar

    let create xs ys =
        PropertyGroup.empty
        |> updateStr "type" "bar"
        |> updateSeq "x" xs
        |> updateSeq "y" ys
        |> Bar

    let withVisibility vis bar =
        applyVisibility applyUpdate vis bar

    let withShowLegend toggle bar =
        applyShowLegend applyUpdate toggle bar

    let withLegendGroup group bar =
        applyShowLegend applyUpdate group bar

    let withOpacity opacity bar =
        applyOpacity applyUpdate opacity bar

    let withName name bar =
        applyName applyUpdate name bar

    let withText textList bar =
        applyText applyUpdate textList bar

    let withHoverText textList bar =
        applyHoverText applyUpdate textList bar

    let withMarker marker bar =
        applyMarker applyUpdate marker bar

    let toTrace bar =
        Trace.TraceBar bar
