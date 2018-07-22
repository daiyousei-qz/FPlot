module FPlot.TraceTemplates

open FPlot.Core
open FPlot.Trace

type MarkerTemplate =
    { _MarkerTemplateTag : unit }

type LineTemplate =
    { _LineTemplateTag : unit }

// Scatter
//

type ScatterTemplate<'TX, 'TY> =
    { _ScatterTamplateTag : unit 
      Xs : seq<'TX>
      Ys : seq<'TY>
      Visibility : TraceVisibility
      ShowLegend : bool
      LegendGroup : string
      Opacity : float
      Name : string
      Text : seq<string>
      HoverText : seq<string>
      Mode : ScatterMode
      Line : Line
      Fill : TraceFill
      FillColor : Color
      Marker : Marker
      TextPosition : TextPosition }

module ScatterTemplate =
    let private createEmpty () =
        { _ScatterTamplateTag = ()
          Xs = null
          Ys = null 
          Visibility = TraceVisibility.Default
          ShowLegend = true
          LegendGroup = null
          Opacity = 1.
          Name = null
          Text = null
          HoverText = null
          Mode = ScatterMode.defaultMode
          Line = Line.defaultLine
          Fill = TraceFill.Default
          FillColor = Color.Default
          Marker = Marker.defaultMarker 
          TextPosition = TextPosition.Default }

    let create xs ys =
        { createEmpty () with Xs = xs; Ys = ys }

    let createWithDefaultXs ys =
        { createEmpty () with Ys = ys }

    let toTrace (t: ScatterTemplate<_, _>) =
        let initScatter =
            if t.Xs = null 
            then Scatter.createWithDefaultX t.Ys
            else Scatter.create t.Xs t.Ys

        [ Scatter.withVisibility t.Visibility
          Scatter.withShowLegend t.ShowLegend
          Scatter.withLegendGroup t.LegendGroup
          Scatter.withOpacity t.Opacity
          Scatter.withName t.Name
          Scatter.withText t.Text
          Scatter.withHoverText t.HoverText
          Scatter.withMode t.Mode
          Scatter.withLine t.Line
          Scatter.withFill t.Fill
          Scatter.withFillColor t.FillColor
          Scatter.withMarker t.Marker
          Scatter.withTextPosition t.TextPosition ]
        |> List.fold (|>) initScatter
        |> Scatter.toTrace

// Bar
//

