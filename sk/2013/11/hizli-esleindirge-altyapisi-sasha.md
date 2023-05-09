# SASHA: Esle/Indirge Altyapisi


SASHA: Esle/Indirge Altyapisi




Github'da yeni projemiz:

https://github.com/burakbayramli/sasha 

Bu altyapi ile mrjob usulu tanimlanan class job dosyalari ile esle/indirge hesaplamalari yapilabilir. Arka planda mesajlarin iletimi icin zeromq kullaniliyor. Esle/indirge mimarileri (mesela Hadoop) bilindigi gibi indirgeme islemi sirasinda ayni anahtarlari ayni makinaya, kod parcasina gonderir, biz bu dagitim icin mod(hash(anahtar), kume_buyuklugu) gibi bir mantik kullaniyoruz. Matematiksel mod bolum sonrasi kalan. artik degeri verir, hash ise bir string icin sayisal deger cikarir. Ikisini biraraya koyarsak, sayisal degeri kume buyuklugune bolunce artik deger hicbir zaman bolenden (kumedeki servis sayisi) buyuk olamaz, o zaman bu deger yuk dagitimi icin hizli bir sekilde kullanilabilir. 






