# Tekil Nesne Kalıbı (Singleton Pattern)

Java programlarımızda bâzen bellekteki herhangi bir sınıfın sadece bir
nesnesi olmasını isteyebiliriz. Meselâ: Bazı değerleri merkezi bir
yerde tutmak istiyoruz, ve bu nesneden sadece bir tâne olmasını
istiyoruz. Ayrıca bu kayıt nesnesine erişim için programda oradan
buraya aynı nesne referansını geçirmek istemiyoruz. Bu gibi durumlar
için Tekil Nesne kalıbı biçilmiş kaftandır. Nerede olursak olalım,
SINIF.nesneyiVer().herhangiBirIslem() gibi bir kullanımla bu tek
nesneye erişebiliriz.  Tekil nesne kalıbının temeli çok basittir. Java
dilinde static kelimesinin nesneler bazında (instance) değil, sınıf
bazında geçerli, yani hep aynı olduğunu biliyoruz. Bu özelliği
kullanarak bir Tekil Nesne yaratmak mümkündür.  Öyle bir sınıf yazarız
ki, kurucu metodu (constructor) dışarıya kapalı olur. Bu
bir.

İkincisi, sınıf kendi tipinden bir nesneye işaret eden bir göstergeçi
static olarak kendi içinde taşır. Bundan sonra yapmamız gereken,
static bir nesneyiVer() diye bir işlem yazmaktır. Bu işlem şunları
yapar. Önce sınıf içinde tutulan static göstergecin bir şeyi gösterip
göstermediğine bakar. Eğer gösteriyorsa, bu değeri geri döndürür. Eğer
göstergeç boş (null) ise (ilk başta böyle olacaktır), o zaman yeni bir
tekil TekilNesne yaratılır, ve static göstergece yazılır, ve bu nesne
döndürülür.  Yâni, sonuç olarak TekilNesne yaratılması sadece ve
sadece tek bir static metod üzerinden olacak. Bu metod da "tek bir" ve
"hep aynı" nesneyi yaratıp geri döndürdüğü için, TekilNesne sınıfından
sadece bir tâne nesne olmuş olur.

```
public class TekilNesne {
   private static TekilNesne nesne;
   private TekilNesne() {
     nesne = new TekilNesne();
   }
   public static TekilNesne nesneyiVer() {
      if (nesne == null) {
         nesne = new TekilNesne();
      }
      return nesne;
   } ...
   public void birseylerYap() { .. }
   public void baskaseylerYap() { .. }
}
```

Kullanmak için

```
TekilNesne.nesneyiVer().birseylerYap();
..
TekilNesne.nesneyiVer().baskaseylerYap();
..
```

Önemli bir nokta olarak, birseylerYap() ve baskaseylerYap()
metotlarının static metot olmadıklarına dikkatinizi çekerim. Bu
metotlar, sınıf bazında değil nesne bazında tanımlanmıştır. Fakat bu
sınıftan tek bir nesne çıkacağı için zaten hep aynı nesnenin
üzerindeki birseylerYap() ve baskaseylerYap() metodunu işletiyor
olacağız. Bu bağlamda metot davranışı static bir metodun davranışına
benzer, fakat konuşulan iç değişkenler nesne bazlı olacaktır, bunun
akılda tutulması gerekir.





