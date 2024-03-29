;;;_.======================================================================
;;;_. webjump package (and webjump-plus)
;; provide a nice keyboard interface to web pages of your choosing
;; web site list is located in the emacs-pkgs/.emacs-webjump.el file
;(require 'webjump-plus)
(global-set-key "\C-cj" 'webjump)

(setq webjump-sites 
  (append 
   '(
     ("ameriprise"        . "http://www.ameriprise.com")
     ("ascii art"         . "http://www.chris.com/ascii/")
     ("astromart"         . "http://www.astromart.com/")
     ("at&t"              . "https://www.wireless.att.com")
     ("cygwin"            . "http://www.cygwin.com")
     ("cygwin list"       . "http://readlist.com/lists/cygwin.com/cygwin/")
     ("drudge"            . "http://www.drudgereport.com")
     ("fifth-third"       . "http://www.fifththird.com")
     ("foxnews"           . "http://www.foxnews.com")
     ("mint financial"    . "https://wwws.mint.com/account.event")
     ("movies"            . "http://www.google.com/search?&q=movie%3A45140")
     ("national review"   . "http://www.nationalreview.com/")
     ("navy CAC help"     . "http://militarycac.com/noactivclientwindows7.htm")
     ("navy federal"      . "http://myaccounts.navyfcu.org")
     ("nko"               . "https://wwwa.nko.navy.mil")
     ("navy navift"       . "http://www.npc.navy.mil/CareerInfo/PerformanceEvaluation/SoftwareForms/")
     ("navy pay"          . "https://myPay.dfas.mil")
     ("navy reserve"      . "https://navyreserve.navy.mil")
     ("navy uniforms"     . "https://www.navy-nex.com/")
     ("patent"            . "http://www.google.com/patents?vid=USPAT6029175&id=esMDAAAAEBAJ&pg=PA3&dq=6,029,175#v=onepage&q&f=false")
     ("scrabble"          . "http://www.scrabble-word-finder.com/scrabble-solver.html")
     ("snopes"            . "http://www.snopes.com")
     ("stevebrown"        . "http://www.stevebrownetc.com")
     ("stevebrownpd"      . "http://feeds.feedburner.com/sbetcpodcast")
     ("trina site"        . "http://thegregoryfamily.blogspot.com/")
     ("turbotax"          . "http://turbotax.intuit.com")
     ("USAA"              . "http://www.usaa.com")
     ("weather radar"     . "http://radar.weather.gov/radar.php?rid=iln&product=N0R&overlay=11101111&loop=no")
     ("weather"           . "http://www.wunderground.com/US/OH/Twenty_Mile_Stand.html")
     ("wguc"              . "http://www.wguc.org/")
     ("wgucstream"        . "http://cpr2.streamguys.net/wguc-nopreroll")
     ("wkrc"              . "http://www.55krc.com/")
     ("wkrcstream"        . "http://www.iheart.com/live/55KRC-1709/?cid=main.html&pname=1196&campid=play_bar&autoplay=true")

     ;; emacs
     ("emacs org-mode"    . "http://orgmode.org/index.html")
     ("org manual"        . "http://orgmode.org/manual/JavaScript-support.html")
     ("worg"              . "http://orgmode.org/worg/")
     ("org slideshow"     . "http://orgmode.org/worg/org-info-js.html")
     ("org life"          . "http://doc.norang.ca/org-mode.html")
     ("org cookbook"      . "http://home.fnal.gov/~neilsen/notebook/orgExamples/org-examples.html")
     ("org manual"        . "http://orgmode.org/manual/index.html")

     ;; Siemens
     ("AHD"               . "https://helpdesk.industrysoftware.automation.siemens.com/CAisd/pdmweb.exe")
     ("JIRA"              . "https://jira.industrysoftware.automation.siemens.com/secure/Dashboard.jspa")

     ;; camping
     ("gear trade"        . "http://www.geartrade.com/browse/hiking-and-camping")
     ("gear swap forum"   . "http://www.backpackinglight.com/cgi-bin/backpackinglight/forums/display_forum.html?forum=19")
     ("clymb"             . "https://www.theclymb.com/")

     ;; Google
     ("gcal"              . "http://calendar.google.com")
     ("ggcheat"           . "http://www.google.com/help/cheatsheet.html")
     ("gmail"             . "http://gmail.google.com")
     ("ggv"               . "https://www.google.com/voice/")
     ("gga"               . [simple-query "http://www.google.com" "http://www.google.com/search?hl=en&lr=&safe=off&c2coff=1&client=firefox-a&rls=org.mozilla%3Aen-US%3Aofficial&q=-inurl%3A%28htm%7Chtml%7Cphp%29+intitle%3A%22index+of%22+%2B%22last+modified%22+%2B%22parent+directory%22+%2Bdescription+%2Bsize+%2B%28mp3%7Cwma%7Cwav%7C%29%20" ""])
     ("ggd"               . [simple-query "http://www.google.com" "http://www.google.com/search?&q=define%3a" ""])
     ("ggg"               . [simple-query "http://groups.google.com" "http://groups.google.com/groups?q=" ""])
     ("ggi"               . [simple-query "http://images.google.com" "http://images.google.com/images?q=" ""])
     ("ggm"               . [simple-query "http://maps.google.com" "http://maps.google.com/maps?q=" ""])
     ("ggn"               . [simple-query "http://news.google.com" "http://news.google.com/news?q=" ""])
     ("gg"                . [simple-query "http://www.google.com" "http://www.google.com/search?&q=" ""])
     ("google"            . [simple-query "http://www.google.com" "http://www.google.com/search?&q=" ""])

     ;; other search engines here
     ("cygwin search"     . [simple-query "http://cygwin.com" "http://cygwin.com/cgi-bin2/package-grep.cgi?grep=" ""])
     ("dogpile audio"     . [simple-query "http://www.dogpile.com" "http://www.dogpile.com/info.dogpl/search/audio/" ""])
     ("dogpile images"    . [simple-query "http://www.dogpile.com" "http://www.dogpile.com/info.dogpl/search/images/" ""])
     ("dogpile multimedia". [simple-query "http://www.dogpile.com" "http://www.dogpile.com/info.dogpl/search/multimedia/" ""])
     ("dogpile news"      . [simple-query "http://www.dogpile.com" "http://www.dogpile.com/info.dogpl/search/multimedia/" ""])
     ("dogpile"           . [simple-query "http://www.dogpile.com" "http://www.dogpile.com/info.dogpl/search/web/" ""])
     ("emacs wiki"        . [simple-query "http://www.emacswiki.org" "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&sa=Search&q=" ""])
     ("findsounds"        . [simple-query "http://www.findsounds.com" "http://www.findsounds.com/ISAPI/search.dll?keywords=" ""])
     ("freedb"            . [simple-query "http://www.freedb.org" "http://www.freedb.org/freedb_search.php?words=" ""])
     ("imdb"              . [simple-query "http://www.imdb.com/" "http://www.imdb.com/find?q=" ""])
     ("bing maps"         . [simple-query "http://www.bing.com/maps/" "http://www.bing.com/maps/?q=" ""])
     ("liveleak"          . [simple-query "http://www.liveleak.com" "http://www.liveleak.com/browse?s=Search+All&q=" ""])
     ("process library"   . [simple-query "http://www.processlibrary.com" "http://www.processlibrary.com/directory?files=" ""])
     ("wikipedia"         . [simple-query "http://en.wikipedia.org" "http://en.wikipedia.org/wiki/Special:Search/" ""])
     ("wtf"               . [simple-query "http://www.acronymfinder.com" "http://www.acronymfinder.com/af-query.asp?String=exact&Find=Find&Acronym=" ""])
     ("yahoo video"       . [simple-query "http://video.search.yahoo.com" "http://video.search.yahoo.com/search/video?_adv_prop=video&x=op&ei=UTF-8&fmt=avi&fmt=mpeg&fmt=qt&fmt=msmedia&sz=all&dur=long&vst=0&vm=p&va=" ""])
     ("youtube video"     . [simple-query "http://www.youtube.com" "http://www.youtube.com/results?search_query=" ""])
     )))
;   webjump-plus-sites
;   webjump-sample-sites))

(provide 'emacs-webjump)
