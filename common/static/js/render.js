function resizeSVG () {
    window._debugme = $(this);
    var cdiv      = $(this).parent();
    var svg       = cdiv.children("svg");
    var svgWidth  = parseInt(svg.attr("width"),  20);
    var svgHeight = parseInt(svg.attr("height"), 20);
    var delta     = 0;
    if ($(this).attr("id") === "inc") {
        delta = 10;
    } else if ($(this).attr("id") === "dec") {
        delta = -10;
    }
    svg.attr("width"
             , svgWidth + delta);
    svg.attr("height"
             , svgHeight + delta);                          
}

$(function () {
    window._debugme = $(".csvg");
    $(".csvg").children("button").click(resizeSVG);
});
