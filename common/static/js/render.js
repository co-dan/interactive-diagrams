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
});

//Force submit button
$(function () {
    $("#forcebtn").click(function () {
        $('#mainform').get(0).setAttribute('action', '/force');
        $('#mainform').get(0).submit();
        $('#mainform').get(0).setAttribute('action', '/new');
    });
});

//Add tooltips
$(function () {
    $("#addImports").popover({ "html"    : true
                             , "trigger" : 'hover'
                             , "delay"   : 
                               { "show" : 300
                               , "hide" : 100
                               }
                             , "content" : 
                               "Import some standard modules "
                               + "like <code>Diagrams.Prelude</code>,"
                               + " <code>Diagrams.Backend.SVG</code>,"
                               + " <code>Data.List</code>, etc. See"
                               + " the &laquo;About&raquo;  page for more info."});    
    $('[data-toggle="popover"]').popover();  
});
