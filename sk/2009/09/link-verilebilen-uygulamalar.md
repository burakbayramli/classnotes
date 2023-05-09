# Link Verilebilen Uygulamalar

Internet siteleri hakkinda verilen bir tavsiye onlari 'linklenebilir'
hale getirmektir, yani site icerigi dinamik bir sekilde (veri
tabanindaki veriye dayanarak) uretiliyor olsa bile, bir baglantinin
belli bir icerige direk erisebilmesinin saglanmasidir. Bir diger tarif
"konum bilgisinin servis tarafinda degil, baglanan tarafinda
tutulmasi" diye gider. Ornek su: Bir forum icerisinde hosunuza giden
bir yorum gordunuz ve bu yorumu direk bir linki kopyala/yapistir
yaparak bir arkadasiniza email uzerinden gondermek
istiyorsunuz. Baglanti site.com/yorum?id=323423 gibi olabilir. Iste bu
uygulama linklenebilir bir uygulamadir. Insanlar yeni yorum, yorum
listele fonksiyonlarini kullaniyor olsalar bile, ozgun (unique) bir
baglanti onu takip edeni bir yoruma direk goturebilir.Bu isi Seam ile
nasil beceririz?

Seam linkleme  kavramina direk destek sagliyor.  Diyelim ki view.xhtml
sayfasi  ile bir  yorum  gosteriyoruz. Bu  sayfayi linklenebilir  hale
getirmek  icin   CommentsHandler  class'imizda  bir   get/set  ikilisi
eklememiz  lazim. ID  selectedCommentId olsun  mesela. Bu  ID ile  bir
yorumun yuklenmesi load() metodunda  olsun. Simdi pages.xml dosyasinda

```
<page    view-id="/view.xhtml"   ...    >
  <param    name="commentId" value="#{commentsHandler.selectedCommentId}"/>
  <action execute="#{commentsHandler.load}"/>
</page>
```

tanimi yapalim.  Boylece bir yoruma, view.xhtml sayfasina nasil
gelirsek gelelim, URL'e baktigimizda `commentId=..` seklinde kimligin
artik URL'in parcasi haline geldigini gorecegiz.  Seam otomatik olarak
selectedCommentId degiskeninin URL uzerindeki commentId degerine
eslenmesini idare edecektir.  Bu idare iki yonludur, iki degerden biri
degisirse bu degisi otekine otomatik olarak yansitilir. Daha sonra
action execute ile belirttigimiz metot cagirilacaktir ve bu metot
biraz once `selectedCommentId` ye eslenmis bilgiyi kullanarak veri
tabanina baglanarak gerekli objeyi oradan alabilir.



