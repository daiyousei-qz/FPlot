module FPlot.Html

let pageTemplate =
    """<!DOCTYPE html>
        <html>
            <head>
                <meta charset="UTF-8" />
                <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
            </head>
            <body>
                [CHART]
            </body>
        </html>
    """

let inlineTemplate =
    """<div id="[ID]" style="width: [WIDTH]px; height: [HEIGHT]px;"></div>
    <script>
        var data = [DATA];
        var layout = [LAYOUT];
        Plotly.newPlot('[ID]', data, layout);
    </script>
    """

let jsTemplate =
    """<script>
        var data = [DATA];
        var layout = [LAYOUT];
        Plotly.newPlot('[ID]', data, layout);
    </script>
    """
