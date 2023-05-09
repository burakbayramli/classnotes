# Python ve Alternatif Path


Python paketleri kaynaklardan kurarken ana dizinine gidip "python
setup.cfg build install" ile derleyip kurulusunu yapariz. Fakat bazen
alternatif bir dizin icinde, ayni paketin ama degisik kaynak kodlari
(mesela bir gelistirme branch'i) uzerinde calismamiz gerekirse, ana
pakete degil, Python'un bu gelistirme yaptigimiz ikinci dizini
isletmesi icin test script icinde en basta su satirlar yeterli:import
syssys.path.insert(0, '/benim/gelistirme/dizinim/paket')Boylece
alternatif dizin tum sys.path icindeki dizinlerden "once" gelecek, ve
kurulumu yapilan degil belirttigimiz ikinci dizin isleme konacak.Bu
yontemlerden sadece biri, en cabuk ve gecici olani. Degisik
yaklasimlar olabilir.





