# Closure


Closure



Closure ozelligi Python, Scheme, Javascript gibi dillerde dinamik olarak fonksiyon yaratip o fonksiyonu parametre olarak gonderebilme/dondurebilme ozelligidir. Fonksiyon dinamik olarak bir "paket" halinde olusturulur ve olusturuldugu anda dis kapsamda (outer scope) olan degiskenlere erisebilir, ve bu paket gonderildigi yerde icindeki o degiskenleri hala kullanabilme ozelligini muhafaza eder.def make_number_printer(n):def number_printer():    print nreturn number_printerprinter = make_number_printer(5)printer()Ustteki ornekte '5' sayisi ekrana basilacaktir.Kaynak




