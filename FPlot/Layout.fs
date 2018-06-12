module FPlot.Layout

open FPlot.Core

type Margin =
    { LeftMargin: int
      RightMargin: int
      TopMargin: int
      BottomMargin: int
      Padding: int
      AutoExpand: bool }

type Layout =
    | Layout of PropertyGroup

module Layout =
    let private applyUpdate update (Layout(data)) =
        data |> update |> Layout

    let defaultLayout =
        PropertyGroup.empty |> Layout

    let withFont font layout =
        layout |> applyUpdate (update "font" (boxFont font))

    let withTitle title layout =
        layout |> applyUpdate (updateStr "title" title)

    let withTitleFont font layout =
        layout |> applyUpdate (update "titlefont" (boxFont font))

    let withWidth width layout =
        layout |> applyUpdate (updateInt "width" width)

    let withHeight height layout =
        layout |> applyUpdate (updateInt "height" height)

    let withPaperBackground color layout =
        layout |> applyUpdate (update "paper_bgcolor" (boxColor color))

    let withPlotBackground color layout =
        layout |> applyUpdate (update "plot_bgcolor" (boxColor color))

    let withSaparators s layout =
        layout |> applyUpdate (updateStr "separators" s)

