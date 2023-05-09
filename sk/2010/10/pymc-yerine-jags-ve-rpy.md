# PyMC Yerine JAGS ve rpy

Not: MCMC icin en iyi secenek su anda A. Gelman'in Stan ve PyStan paketleri. 

Python ile MCMC kodlamasi icin PyMC paketini gozden gecirdik, ne yazik ki performanstan memnun kalmadik, diger yandan WinBUGS'in arayuzu son derece kullanissiz (Windows / Wine gerektiriyor), bu sebeple surekli kullanim icin tek alternatif kaldi: Python icinden rpy2 kullanarak JAGS cagirmak.

JAGS kurulusunu isledik, bu program WinBUGS'in tamamen yerine gecmeye
talip bir paket.

Tamamen Unix seviyesinde, komut satirindan calisir, iletisime
gecilmesi (interfacing) temiz.

rpy2 kullanimi icin su yazimizdaki ornek koda bakilabilir, JAGS
kullanimi icin su yazidaki kod faydali. JAGS WinBUGS ile ayni dosya
formatini kullanmakta, yeni bir dil ogrenmeye gerek yok. Andrew
Gelman'in tum ornekleri zaten bugs dilini kullaniyor, bu acidan da
farklilik olmayacak. Tum rpy2, JAGS, Python orneklerinin bir arada
oldugu en iyi kod parcasi surada bulunabilir.





