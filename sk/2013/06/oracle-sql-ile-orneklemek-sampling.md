# Oracle SQL ile Orneklemek (Sampling)

SELECT ifadesi ile tum satirlar (where ile filtrenenleri tabii) almak
yerine, o kume icinden 10000 satirlik bir orneklem (sample) almak icin
alttaki ifade kullanilir,

```
exec dbms_random.initialize(17809465);
select kolon1,kolon2,...FROM [tablo]where 1=1
and ...
and rownum<10000
..order by dbms_random.random;
```

Cagri initialize ne ise yarar? Bu fonksiyona ayni tohum (seed) degeri
verilirse (rasgele sayi uretme literaturunu bilenlere bu kavram
tanidik gelecektir), o deger ile yapilan bir rasgele islem arka arkaya
isletilse bile hep ayni "rasgele degerleri" dondurur (ya da ustteki
ornekte hep ayni rasgele satirlari geri getirir). Eger tohum
verilmeseydi ustteki cagri her seferinde farkli degerleri geri
getirecekti.

Bunun faydasi, kodumuzda hata ararken hep ayni veriyle islem
yaptigimizdan emin olabilmemiz (hep ayni tohumu kullanarak), boylece
veriye donuk farkliliklarla ugrasmaya ihtiyac kalmamasi. Sayi 17809465
yerine herhangi, yeterince buyuk baska bir sayi kullanilabilir.

Eger farkli cagrilarda farkli veri istersek, bir sonraki cagrida tohum
degerini degistiririz, ve farkli rasgele sonuclar aliriz.






