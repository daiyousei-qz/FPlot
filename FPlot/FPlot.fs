namespace FPlot

open System
open FPlot.Core
open FPlot.Trace
open FPlot.Layout

module Templates =
    type ScatterTemplate<'TX, 'TY> =
        { _ScatterTamplateTag: unit 
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

    let private createEmptyScatterTemplate () =
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

    let createScatterTemplate xs ys =
        { createEmptyScatterTemplate () with Xs = xs; Ys = ys }

    let translateScatterTemplate (t: ScatterTemplate<_, _>) =
        let initScatter =
            if t.Xs = null 
            then Scatter.createWithDefaultX t.Ys
            else Scatter.create t.Xs t.Ys

        let (Scatter(data) as scatter) =
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
            |> List.fold (fun x f -> f x) initScatter
            //|> Scatter.toTrace

        let json = 
            PropertyGroup.toJsonString data
        
        Scatter.toTrace scatter
        

type Chart =
    { Layout: Layout
      Traces: Trace list }

module Chart =
    open System.Runtime.InteropServices
    open System.Diagnostics

    let extraceTraceData trace =
        match trace with
        | Trace.TraceScatter(Scatter(data)) -> data
        | Trace.TraceBar(Bar(data))         -> data

    let extractLayoutData layout =
        match layout with
        | Layout(data) -> data

    let toInlineHtml id chart =
        let width, height =
            let layoutData = chart.Layout |> extractLayoutData

            let w = layoutData.tryFind "weight" |> Option.map unbox<int> |> Option.defaultValue 900
            let h = layoutData.tryFind "height" |> Option.map unbox<int> |> Option.defaultValue 500

            w, h

        let dataJson =
            chart.Traces
            |> List.map (extraceTraceData >> PropertyGroup.toJsonString)
            |> fun xs -> String.Join(", ", xs)
            |> sprintf "[%s]"

        // TODO: finish this
        let layoutJson =
            "{}"
        
        Html.inlineTemplate
            .Replace("[ID]", id)
            .Replace("[WIDTH]", string width)
            .Replace("[HEIGHT]", string height)
            .Replace("[DATA]", dataJson)
            .Replace("[LAYOUT]", layoutJson)

    let toHtml id chart =
        Html.pageTemplate.Replace("[CHART]", toInlineHtml id chart)

    let openInBrowser (url: string) =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            let url = url.Replace("&", "^&")

            Process.Start(
                ProcessStartInfo("cmd", sprintf "/c start %s" url, CreateNoWindow = true))
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
            Process.Start("xdg-open", url)
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
            Process.Start("open", url)
        else
            failwith "invalid platform"

        |> ignore
            

    let show chart =
        let tempId = Guid.NewGuid().ToString()
        let tempPath = System.IO.Path.GetTempPath()
        let file = sprintf "%s.html" tempId
        let path = System.IO.Path.Combine(tempPath, file)
        let html = toHtml tempId chart

        System.IO.File.WriteAllText(path, html)
        openInBrowser path




     