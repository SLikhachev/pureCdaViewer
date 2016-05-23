(function($){
  $(function(){
    
    $('.button-collapse').sideNav({
      //menuWidth: 300, // Default is 240
      edge: 'left', // Choose the horizontal origin
      closeOnClick: true // Closes side-nav on <a> clicks, useful for Angular/Meteor
    });
  
    $('.collapsible').collapsible({
      accordion : true
    });
  
    $(".dropdown-button").dropdown({ 
            hover: true,
            constrain_width: false, // Constrains width of dropdown to the activator
            gutter: 0, // Spacing from edge
            belowOrigin: true,
            alignment: 'left'
    });
    
    $('.modal-trigger').leanModal();
     
    $('select').material_select();

    $('#folds-all').on("click", function () {
      $('.collapsible > li > .collapsible-header').each(function () {
        if ( $(this).hasClass("active") ) {
           $(this).trigger('click.collapse') 
        }
      })
    });

    $('#unfolds-all').on("click", function () {
      $('.collapsible > li > .collapsible-header').each(function () {
        if ( ! $(this).hasClass("active") ) {
           $(this).trigger('click.collapse')
        }
      })
    });

    var Months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

    function parceDate (d) {
      var yer = d.substring(0, 4);
      var mon = Months [ parseInt( d.substring(4, 6) ) - 1 ];
      var d1 = d.substring(6, 7), day;
      if ( d1 == "0") {day = ""} else { day = d1 }
      day += d.substring(7, 8);
      var date = day + " " + mon + " " + yer;
      if (d.length < 9) {
        return date
      }  
      var h = d.substring (8, 10), m = d.substring (10, 12), s = d.substring (12, 14), time = " ";
      var tz = " "; 
      if (h.length > 1) { 
        time += h;
        if (m.length > 1) {
          time += ":" + m;
          if ( (s.length > 1) && (s.indexOf("-") < 0) && (s.indexOf("+") < 0) ) {
            time += ":" + s;
          } 
        }
      }
      var tz1 = d.indexOf("-"), tz2 = d.indexOf("+"), z = 0;
      if (tz1 > 0) { z = tz1 }
      if (tz2 > 0) { z = tz2 }
      if (z > 0) {
        tz += "UTC" + d.substring(z, z+5)
      } else {
        tz = "";
      }    
      return date + time + tz;
    }  

    (function (){
      var d = $('#dofBirth').text();
      $('#dofBirth').text( parceDate (d) );
      d = $('#docDD').text();
      $('#docDD').text( parceDate (d) );
    })();


  }); // end of document ready
})(jQuery); // end of jQuery name space

          