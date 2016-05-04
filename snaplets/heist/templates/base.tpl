<!DOCTYPE html>
<html lang="en">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
  <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1.0"/>
  <title>Simple C-CDA Viewer</title>
  <!-- CSS  -->
  <link href="../../statics/css/icons.css" type="text/css" rel="stylesheet" media="screen,projection">
  <link href="../../statics/css/materialize/materialize.min.css" type="text/css" rel="stylesheet" media="screen,projection"/>
  <link href="../../statics/css/open-sans.css" type="text/css" rel="stylesheet" media="screen,projection"/>
  <link href="../../statics/css/style.css" type="text/css" rel="stylesheet" media="screen,projection"/>
</head>
<body>

  <div class="navbar-fixed">
    <nav class="bg-color-p-1" role="navigation">
      
      <div class="nav-wrapper container">
         <ul id="slide-out" class="side-nav">
          <li><a href="#">Some Link</a></li>
        </ul>
        <a href="#" data-activates="slide-out" class="button-collapse show-on-large">
          <i class="material-icons">menu</i>
        </a>
        <a href="#file-form" id="fft" class="btn bg-color-p-3 z-depth-0 modal-trigger">
          open cda xml
        </a>
      </div>
    
    </nav>
  </div>

  <div class="section no-pad-bot" id="index-main">
    <div class="container">
      
	    <div id="content">

        <apply-content/>

      </div>
      

    </div>
  </div>


  <!--footer class="page-footer">
  </footer-->
  <div id="file-form" class="modal">
    <form enctype="multipart/form-data" class="" id="open-form" method="POST" action="/upload">
      <div class="file-field input-field">
        <div class="btn z-depth-0">
          <span>File</span>
          <input name="filename" type="file">
        </div>
        <div class="file-path-wrapper">
          <input class="file-path validate" type="text">
        </div>
      </div>
      <button class="btn bg-color-p-3" type="submit">open</button>
    </form>
  </div>
  
   <!--  Scripts-->
  <script src="../../statics/js/lib/jquery-2.1.1.min.js"></script>
  <script src="../../statics/js/lib/materialize.min.js"></script>
  <script src="../../statics/js/app.js"></script>

  </body>
</html>
