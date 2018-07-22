namespace FPlot

open FPlot.Core
open FPlot.Trace
open FPlot.Layout

type Chart =
    { Layout: Layout
      Traces: Trace list }

module Chart =
    open System
    open System.IO
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
        let tempPath = Path.GetTempPath()
        let file = sprintf "%s.html" tempId
        let path = Path.Combine(tempPath, file)
        let html = toHtml tempId chart

        File.WriteAllText(path, html)
        openInBrowser path




     