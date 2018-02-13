### İçerik

İçeriğe ek yapmak isteyenler gerekli .tex dosyasını değiştirip,
ekleyip içinde oldukları dizinde `python ../build.py tex` komutunu
işletebilirler, gerekli PDF dosyası otomatik olarak üretilecektir.

LaTeX dosyalarının tamamını HTML'e çevirmek için `python build.py
html` komutu kullanılır, sonuç içerik`TARGET_DIR` altına yazılır. HTML
sonucu biz buradan alıp Github'a bir Web projesine gönderiyoruz,

https://burakbayramli.github.io/dersblog

İçerik ekleri için Github Web arayüzünden o an bakılan dosya üzerinde
Edit tıklamak yeterli, değişim arayüzden yapılır, kaydedilir, değişim
isteği bana "yama sorusu (patch request)" olarak gelir, ben
değişikliği dahil ederim, ve üstteki adrese gönderimi yapılınca
değişiklik sayfalara, PDF dosyalarına yansır. Github usta
kullanıcıları tabii depo klonlaması, vs. ile aynı şeyleri
yapabilirler.

US klavyede Türkçe yazmak isteyenler (ve Emacs kullananlar için)

http://sayilarvekuramlar.blogspot.com/2017/12/emacs.html

HTML, PDF islemler icin gereken programlar

```
pdftk
texlive
```

