# Derleyiciler

Derleyen programların yegane görevi kaynak kodu "işler kod" haline
çevirmektir.  Kaynak kodun ne olduğunu anlatmadan once, 'işler koda'
değinelim birazcık.  Her bilgisayarın temelinde bir mikroişlemci
bulunur. Mikroişlemci bilgisayarın beynidir, bütün işleri koordine
eder ve işlemleri o yapar. Toplama, çıkarma, çarpma haricinde veri
bulma ve transferi işini de mikroişlemci gerçekleştirir.  Mikroişlemci
bu işleri bir "komut" verildiği zaman yapar. Bu komutların kendisi
aslında bir takım veriden ibarettir. 1 ile 255 arasındaki her sayı
belli bir komuta tekabül edebilir. Mesela toplama için '1', çarpma
icin '2'.  Bu iş programcıların işini zorlaştırır tabii ki: Görüldüğü
üzere 1, 2 gibi kodları hatırlamaya uğraşmaktansa, '+' harfini basmak
daha kolaydır. İşte burada derleyici programlar devreye
girer. Programcılar kodu istedikleri 'güzel' dilde yazabilirler,
işleri bitince derleyici programı kaynak üzerinde calıştırmak
gerekir. Kaynak kodda '+' işareti görülür görülmez '1' sayısı yerine
koyulur, ve derleyici program her şeyi yeni bir dosyaya yazar.  Bu
dosya windows systeminde .exe, .com gibi programlar halinde
sisteminizde bulunabilir.  Kaynak kod ise Java, C++ gibi dillerden
komutlar içeren bir dosyadan ibarettir.


