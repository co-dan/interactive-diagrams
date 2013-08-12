function resizeSVG () {
    window._debugme = $(this);
    var cdiv      = $(this).parent().parent();
    var svg       = cdiv.children("svg");
    var svgWidth  = parseInt(svg.attr("width"),  10);
    var svgHeight = parseInt(svg.attr("height"), 10);
    var delta     = 0;
    if ($(this).attr("id") === "inc") {
        delta = 35;
    } else if ($(this).attr("id") === "dec") {
        delta = -35;
    }
    svg.attr("width"
             , svgWidth + delta);
    svg.attr("height"
             , svgHeight + delta);                          
}

$(function () {
    $(".thumbnail")
        .children(".btn-group")
        .children("button")
        .click(resizeSVG);
});
