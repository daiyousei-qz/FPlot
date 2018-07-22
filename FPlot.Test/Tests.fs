module Tests

open System
open Xunit
open FPlot
open FPlot.Core
open FPlot.Layout
open FPlot.Trace
open FPlot.TraceTemplates

[<Fact>]
let ``My test`` () =
    let trace = 
        Scatter.createWithDefaultX (Seq.init 10 (fun x -> x*2))
        |> Scatter.withName "normal"
        |> Scatter.withMarker (Marker.defaultMarker 
                               |> Marker.withSize 5
                               |> Marker.withColor Color.yellow)
        |> Scatter.withLine (Line.defaultLine
                             |> Line.withDash LineDash.Dot
                             |> Line.withColor Color.green)
        |> Scatter.toTrace

    let ys = Seq.init 10 float
    let trace2 =
        { ScatterTemplate.createWithDefaultXs ys with
              Name = "normal"
              Visibility = TraceVisibility.No
              Marker = (Marker.defaultMarker 
                               |> Marker.withSize 10
                               |> Marker.withColor Color.green)}
        |> ScatterTemplate.toTrace

    let layout =
        Layout.defaultLayout

    Chart.show { Layout = layout; Traces = [trace2] }

    Assert.True(true)
