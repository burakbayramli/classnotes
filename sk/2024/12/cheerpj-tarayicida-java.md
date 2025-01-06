# Tarayıcıda Java - Cheerpj

Web siteleri çoğunlukla Chrome, Edge, Firefox gibi tarayıcılara kendi
makinalarında işletmek üzere kod gönderirler. Bu kodlar son zamanlarda
Javascript üzerinden oluyor, bir kaynak kodu işletilmek üzere
tarayıcıya aktarılıyor. Fakat Web'in ilk başladığı zamanlarda bazıları
işler (binary) kodların tarayıcıya gönderilmesi gerektiğini
söylüyordu, Java applet teknolojisinin arkasında yatan kavram
buydu. Applet kodlarını Java kaynaktan derleyip işler kod .class, .jar
dosyaları üretip paketliyordunuz, bağlanan tarayıcı bu kodları kendi
tarafında işletiyordu.

Applet teknolojisi yaygınlık kazanmadı, şu anda pek tüm yaygın
tarayıcılar desteği çıkartmış durumda. Fakat bazılarının hala Java
kodlarını tarayıcıda işletme ihtiyacı var - belki mevcut / eski
(legacy) kodlar var, Java olarak duruyorlar, o kodları Javascript'e
tercüme etmek uzun zaman alacak... Applet gittiğine göre, bu kodlar
tarayıcıda nasıl işletilecek?

Cheerpj bunu yapmaya talip, test ettik, teknolojide bazı eksikler var,
basit kodlar çalıştı fakat çetrefil jar dosyalarında problem çıktı.
Yine de paylaşalım, belki başkaları için yeterli olur.

[Dosyalar](cheerpj-listing.html)

Not: Üstteki HTML Unix `tree -af -H . ` komutu ile üretilmiştir, dizin
yapısını göstermek istedik, aynı zamanda dosyalar tıklanıp
görülebilir, indirilebilir.

https://cheerpj.com/docs/getting-started/Java-library

https://www.theserverside.com/blog/Coffee-Talk-Java-News-Stories-and-Opinions/java-in-the-browser-webassembly-tutorial-wasm-teavm-html-javascript

https://labs.leaningtech.com/docs/cheerpj2/reference/Runtime-API