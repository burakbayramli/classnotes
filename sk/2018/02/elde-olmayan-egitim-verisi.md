# Elde Olmayan Eğitim Verisi

Kaggle sitesinde pek çok veri bilimcinin bildiği üzere veri bilim
yarışmaları düzenleniyor. Mesela geçende bir tanesi basit ses
komutlarını tanımak (Tensorflow kullanarak) hakkındaydı.

https://www.kaggle.com/c/tensorflow-speech-recognition-challenge

Eğitim verisi sağlanmış, yani etiketleriyle beraber ses verisi (mesela
bir wav ses dosyası ve onun hangi komut olduğu, "yes", "no" gibi) bir
de test verisi var, bu veri için etiket verilmemiş, sadece ses
kaydı. Yarışmacı eğitim verisi üzerinde eğitim yapacak test verisi
üzerinde bu modeli kullanıp etiket üretecek, ve Kaggle sitesine
kontrol için verecek - sistem hemen başarıyı ölçüyor, kısa sürede
cevap sağlıyor.

Yarışmacılar eğitimi yaparken tabii ki eğitim verisini alıp onun
içinden kendileri bir test verisi yaratıyorlar, ki skorun gerçekten ne
olabileceğini görebilsinler, model "ezberliyorsa" onu hemen
anlayabilsinler, bu standart bir teknik. Fakat bazı yarışmacılar
farkediyor ki eğitim / kendi test verilerinde elde edilen başarı
Kaggle'dan gelen cevabın %10-15 gerisinde! Bu demektir ki Kaggle'ın
sağladığı test verisi eğitim verisinden çok farklı. Yarışmacıdan
beklenen ses verisini değiştirip, gürültü ekleyip, kaydırıp vs yeni
eğitim verisi "üretmesi" ve böylece başarısını arttırması.

Bu durumun gerçek dünya şartlarını pek yansıtmadığını söylemek
gerekir. Genelde modeller eldeki veri neyse onun üzerinde eğitilir,
yani "gelecekte ne olabilir" türündeki spekülasyona girilmez. Eğer
girilebiliyorsa o veri de elde vardır demektir ve eğitim verisi
o'dur. Kaggle'ın yaptığı resimden kedi tanıyan bir yarışmada test
verisinde üç, dört kulaklı kedilere hazır olunmasını istemek gibi.

Fakat gerçek dünyada şu olur: kullanılmış dış dünyadan gelen / gelmis
veri değişmeye başlayabilir, bu duruma hazır olunması gerekir ki
böylece bu yeni veriyle tekrar eğitim yapılsın, modeller
güncellensin. O değişimi yakalamak için rutinler yazılabilir,
vs. Fakat bu durum spekülatif şekilde elde olmayan veriye göre
eğitmekten farklı.






