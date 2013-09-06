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
