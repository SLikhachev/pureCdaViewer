The simple web viewer for files of C-CDA xml documents.
==============================================
The application written for [C-CDA® Rendering Tool Challenge](http://www.hl7.org/events/toolingchallenge.cfm).

This is small and fast web app based on [The Haskell Snap framework](http://snapframework.com).

To test the app you need to clone the [git repo](https://github.com/SLikhachev/pureCdaViewer) either with 
**git clone** or download zip archive from GitHub server. The repo includes the executable file in 
**dist/build/cda/cda.exe** and **go.bat** file in the root of the repo. 
The **cda.exe** was compiled with **ghc-7.8.3** compiler on Windows XP and tested with MS Windows XP, 7, 8, 8.1, 10. 
Any modern browsers (include MS IE >= 10, MS Edge, Chromium based, Opera, Firefox, Vivaldi) will be work with the app.
 
There is **server.cfg** file in the root of the repo where:
 * bind - the IP address to bind the Snap web server (default 127.0.0.1 or localhost)
 * port - the TCP port for server listening
 * verbose - Bool value if **true** all messages will be put to stderr channel.

 You can set up your own values to the **server.cfg** file before start the server.
 
Compiled version of the app works on MS Windows only. You need recomile the app to binary with the `cabal build` 
tool for Haskell or something else for you own platform.
 
To start the server you need run the Windows **cmd.exe** then change directory to root of the pureCdaViewer repo and to type **go**.

For example (`c:\cdaViewer is the root of the cloned repo`):

    >cd c:\cdaViewer
    >go

The server stops with two **<CTRL+C>**.

On the MS Windows platform the Snap web server can be run as a system service with help the tools such as [NSSM](http://nssm.cc/).
  
After start the server you need open appropriate browser (see above) then type in the address panel something like **http://127.0.0.1:8989/**
and press RETURN. If you will see the top-fixed green panel with `OPEN C_CDA XML FILE` dark-green button it means that everything is working.

To open new file click on the dark-green button, select the file and the number items of the sections on the one page, then press `OPEN`.
You will see the list of sections of the document. Every item is clickable and scrolls down shows the text content of the section. The information about the last opened file is stored in the cookie session of the browser and expire after 5 years,
 i.e. browser will be recall the last opened file every time it started with the given address.  To change the current opened file just open new one with the `OPEN C_CDA XML FILE` dialog form.

There is not yet:

* semantic analisys of the diocument
* search and filter functions
* good UI for rearrange the structure of the document.

**Author:      SLikhachev <polaughing@yahoo.com>**

**Copyright:   SLikhachev <polaughing@yahoo.com>**

 

  