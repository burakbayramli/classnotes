# PDF Icinde Kelime Aramak


PDF Icinde Kelime Aramak



Linux uzerindeki PDF dosyalari icinde bir anahtar kelimeyi aramak icin soyle bir Python script kullanilabilirimport oslist = os.popen("find . -name '*.pdf'").read().split("\n")for f in list:   f = f.replace(" ", "\\ ")   if "bu cumleyi ara" in os.popen("pdftotext " + f + " -").read():       print fBu script pdftotext adli bir yardimci programi cagiriyor, program yoksa kurulmasi lazim. Script, bir ust dizinden onun altindaki her alt dizine (recursive) olarak girerek aramayi yapiyor. Kelime bulunursa dosya ismi ekrana basiliyor.




