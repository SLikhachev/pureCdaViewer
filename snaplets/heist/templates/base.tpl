<!DOCTYPE html>
<html lang="en">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
  <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1.0"/>
  <title>Simple C-CDA Viewer</title>
  <!-- CSS  -->
  <link href="../../statics/css/material-icons.css" type="text/css" rel="stylesheet" media="screen,projection">
  <link href="../../statics/css/materialize.min.css" type="text/css" rel="stylesheet" media="screen,projection"/>
  <link href="../../statics/css/open-sans.css" type="text/css" rel="stylesheet" media="screen,projection"/>
  <link href="../../statics/css/style.css" type="text/css" rel="stylesheet" media="screen,projection"/>
</head>
<body>

  <div class="navbar-fixed" id="top-side">
    <nav class="bg-color-p-1" role="navigation">
      
      <div class="nav-wrapper container">
        <ul class="right">
          <li><a href="#top-side"><i class="material-icons">home</i></a></li>
          <li><a href="#top-side" id="folds-all"><i class="material-icons">&#xe8ef;</i></a></li>
          <li><a href="#top-side" id="unfolds-all"><i class="material-icons">view_agenda</i></a></li>
        </ul>
<!--
         <ul id="slide-out" class="side-nav">
          <cdaContent>
          <li>
            <sTitle />
          </li>
          </cdaContent>
        </ul>

        <a href="#" data-activates="slide-out" class="button-collapse show-on-large">
          <i class="material-icons">menu</i>
        </a>
-->        
        <a href="#file-form" id="fft" class="btn bg-color-p-3 z-depth-0 modal-trigger">
          open c-cda xml file
        </a>
      </div>
    
    </nav>
  </div>

  <div class="section" id="index-main">
    <div class="container">
      
	    <div id="content">

        <apply-content/>

      </div>
      

    </div>
  </div>




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
      <div class="input-field">
        <select name="pageby">
          <option value="0">All sections displayed</option>
          <option value="5">By 5 sections</option>
          <option value="10">By 10 sections</option>
          <option value="20">By 20 sections</option>
        </select>
        <label>Choose number sections on page</label>
      </div>

      <button class="btn bg-color-p-3 z-depth-0" type="submit">open</button>
    </form>
  </div>
  
   <!--  Scripts-->
  <script src="../../statics/js/lib/jquery-2.1.1.min.js"></script>
  <script src="../../statics/js/lib/materialize.min.js"></script>
  <script src="../../statics/js/app.js"></script>

  </body>
</html>
