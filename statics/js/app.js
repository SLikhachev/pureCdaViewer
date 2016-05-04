(function($){
  $(function(){
    
    $('.button-collapse').sideNav({
      //menuWidth: 300, // Default is 240
      edge: 'left', // Choose the horizontal origin
      closeOnClick: true // Closes side-nav on <a> clicks, useful for Angular/Meteor
    }
  );
  $(".dropdown-button").dropdown({ 
            hover: true,
            constrain_width: false, // Constrains width of dropdown to the activator
            gutter: 0, // Spacing from edge
            belowOrigin: true,
            alignment: 'left'
            } );
    $('.modal-trigger').leanModal();
  }); // end of document ready
})(jQuery); // end of jQuery name space

          