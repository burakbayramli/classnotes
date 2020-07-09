# Basit Rasgele Muzik Calici


Basit Rasgele Muzik Calici



Sabit diskinizde olan mp3'lerin rasgele calinmaya baslamasi icin bir Python script:import glob, os, random, sysimport threadingimport selectlist = glob.glob("[MUZIK DIZINI]/*.mp3")while True:   idx = int(random.random() * len(list))   print list[idx]   os.system("mplayer '%s'" % list[idx] )   print "Delete? (Press d for delete)..."   k=""   def input():       global k       i = 0       while i < 1:           i = i + 1           r,w,x = select.select([sys.stdin.fileno()],[],[],2)           if len(r) != 0:               k  =sys.stdin.readline()   T = threading.Thread(target=input)   T.setDaemon(1)   T.start()   T.join(2) # wait for 2 seconds   print ">>>>>>>>>" + k   if k == 'd':       print "deleting " +  list[idx]       cmd = "rm '%s'" % list[idx]       os.system(cmd)Ubuntu uzerinde bu script'e bir kisayol koyduk, kisayolun komutu su sekildexterm -e "python /dizin/rasgele_cal.py"Tiklayinca script icin ayri bir xterm acilacak. Muzik calarken Ctrl-C ile bir sonraki sarkiya gecilebilir (cunku os.system'den disari cikmis oluruz, dongu devam eder). Ses acmak *, azaltmak / (bunlar mplayer'in komutlari). Begenmediginiz sarkiyi diskten sildirmek icin "Delete?" diye sordugunda 'd' ve Enter'e basin.




