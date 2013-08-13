function resizeSVG () {
    var csvg      = $(this).parent()
                           .parent()
                           .find(".csvg");
    var svg       = csvg.find("svg");
    var svgWidth  = parseInt(svg.attr("width"),  10);
    var svgHeight = parseInt(svg.attr("height"), 10);
    var csvgWidth  = parseInt(csvg.attr("width"),  10);
    var csvgHeight = parseInt(csvg.attr("height"), 10);

    var delta     = 0;
    if ($(this).attr("id") === "inc") {
        delta = 35;
    } else if ($(this).attr("id") === "dec") {
        delta = -35;
    }
    setSVGsz(svg, svgWidth + delta, svgHeight + delta);
    //setSVGsz(csvg, csvgWidth + delta, csvgHeight + delta);
}

function setSVGsz(svg, width, height) {
    console.log(svg);
    svg.attr("width", width);
    svg.attr("heighth", height);
    // svg.attr("viewBox", "0 0 " + width + " " + height);
}

// Svg rendering stuff
$(function () {
    // $(".thumbnail")
    //     .children(".btn-group")
    //     .children("button")
    //     .click(resizeSVG);
    // $(".thumbnail")
    //     .children("csvg")
    //     .children("svg")
    //     .css("overflow", "scroll");
    $(".thumbnail").css("overflow", "hidden");
    
    var csvg = $(".csvg");
    csvg.svgPan('viewport', true, true, false, 0.2);
    window.debug = csvg;
});

// Direct links to 'edit' and 'view' views
$(function () {
    var hash = document.location.hash;
    var prefix = "_";
    if (hash) {
        $('.nav-tabs a[href='+hash.replace(prefix,"")+']').tab('show');
    }
    
    // Change hash for page-reload
    $('.nav a').on('shown', function (e) {
        window.location.hash = e.target.hash.replace("#", "#" + prefix);
    });
});

// Literate haskell support
$(function () {
    $('input[type="checkbox"][name="literate"]').change(function () {
        var mode;
        if (this.checked) {
            mode = "literatehaskell";
        } else {
            mode = "haskell"; 
        }
        cm.setOption("mode", mode);
    });
})
