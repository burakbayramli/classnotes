# Kaynak Kod İdaresi - CVS

Bir yazılım projesi geliştirme süreci başlamadan once, etrafınıza
şöyle bir bakın. Programcılarınız hazır, bilgisayarları alınmış ve
kurulmuş, derleyicileri hazır (Java, vs). Ne eksik?  Kaynak Kod İdare
programını unutmayın. KKİ sistemleri sayesinde aynı yazılımda bir çok
programcının çalışması mümkün oluyor. Yoksa, eğer iki programcı aynı
anda BeniDuzelt.java kayıdını değiştiriyorsa, hangisinin sürümü en
önde nasıl bileceksiniz? Bildiniz, KKI sistemi sayesinde.  KKİ sistemi
için önce bir depo yaratmak gerekir. Bu depo, bütün kaynak kodun
saklandığı yer olur. Programcılar, bu depodan dosyaları kendi
ortamlarına indirirler. Dosya üzerinde ekleme, çıkarma işini kendi
ortamlarında yaparlar. İşleri bitince KKI sistemine "geri" verirler.

KKİ sistemi, eğer "aynı anda" iki kişinin değistirdiği bir dosya
varolduğunu bulursa, değişim "carpışması" olduğunu haber verir. Bu
haber aynı dosyayı ikinci geri veren programcıya gösterilir. Böyle bir
durumda programcının, değişmiş dosyayı depodan çıkartıp, kendi sürümü
ile 'birleştirmesi(merge)' gerekir. CVS programı otomatik bir
birleştirici sunuyor, işimizi rahatlatmak için.

Mesela, diyelim ki, BeniDegistir.c dosyasinı iki kişi aynı anda
değiştirmeye başladı. İşleri bittikten sonra, belli aralar ile şunu
yaptılar.  cvs ci BeniDegistir.c # ya da cvs commit BeniDegistir.c
İkinci programcı bir hata mesajı görecek, ve depo sizin sürümü kabul
etmeyecek. Çünkü sizden önce baskaşı kayıdı değiştirmiş, ve siz
eklemenizi ondan bir önceki sürüme göre yapmışsınız. CVS size diyor ki
"Bir de kodun son haline bir bak, eğer değişikliğin hala geçerliyse,
bana tekrar geri ver".

Bu durumda, en yeni sürümü depodan alıp, kendi dosyan ile
"birleştirmek" gerekir. Dikkat edin, bu birleştirme hala depoda değil,
sizin şahsi dosyanızda oluyor.  $ cvs -q co -P BeniDegistir.c Bu
komuttan sonra, elinizde şöyle bir kayıt gececek..

```
#include 

int main(int argc, char **argv){
   init_arayici();
   ara();
   if (argc != 1) {
      fprintf(stderr, "tc: Hic Oge Gondermeye Gerek Yok.\n");
      exit(1);
   }
   if (nerr == 0)
     KodYarat();
   else
      fprintf(stderr, "Kod Yaratilmadi.\n");<<<<<<< nerr ="=">>>>>>> 1.6} <<<<<<< ve ="=">
======= ve >>>>>>>
```

1.6 arasına gelenler depoda sizden önce yapılan değişiklikler.  Bu
birlesmiş dosyaya bakarak, sizin eklemenizin geçerli olup olmadığına
bakın, ve dosyayı son haline getirin. `====` işaretlerini çıkartarak
tabii. Bunu yaparken öteki programcıya soru sormanız
gerekebilir. İletişim çok önemli.. Bundan sonra, en son formu bulup,
şu komutu tekrar işletmeniz gerekir.  cvs commit BeniDegistir.c Fakat
artık cvs komutu başarıyla tamamlanacak. Depoda artık güncel dosyanız
bulunuyor!



