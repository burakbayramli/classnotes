# Debugger İyi mi, Kötü mü

Asağıdaki konuşma, bir İnternet tartışma forum'undan alınmıştır.

TIGRAN

....Bütün bu bilgiler icin teşekkürler. Acaba Linus (Torvalds) bu konu
hakkında ne düşünüyor? Bir süre önceki fikrini hatırlıyorum, ama belki
değişmiştir. Yani, fikir değistirmek normal, ve kdb programı artık
bayağı iyi durumda, acaba Linus bunlara bakınca, kdb programını Linux
ana dağıtımına dahil edemez mi?

LINUS

Debugger yazılımlarını sevmem. Hiç bir zaman hoşuma gitmedi, herhalde
böyle devam edecek. GDB programını kullanırım, debugger olarak değil
ama, disassembler olarak.  Çekirdek (kernel) debugger'larını savunmak
icin söylenenler beni az bile olsun etkilemedi. Emin olun,
savunmaların çoğunu da duydum. Sonuçta söylenenleri şu kelimeye
indirgeyebiliriz.  Debugger varsa, geliştirme yapmak rahatlar, ve yeni
özellikler eklemek kolaylaşır.  Açık olayım, umrumda değil. Çekirdek
kod yazmak "kolay" bir şey olmamalı. Satırları teker teker takip edip,
program çalışmasını anında izleyerek hata bulma işlemini
desteklemiyorum. Çekirdek içini en ince detayda görebilmemiz (debugger
sayesinde) bence o kadar da iyi bir şey değil.  Güya, karşı
destekçilere göre, eğer debugger yazılımı yoksa başımıza şunlar
gelecekmiş.

* Bir hata olunca her sey çöküyor, fsck programını kullanıyorsunuz ve
cok uzun zaman alıyor, bu da programcının moralini bozuyor.

* Debugger olmayınca, programcılar Linux çekirdek projesinden
vageçiyorlar, çünkü işleri zorlaşıyor ve zaman alıyor.

* Yeni özellikler eklemek zaman alıyor.  Fakat bütün bunların nesi
kötü?  Bana göre, durum hata değil, istenen özellik. Hem belgelenmiş,
hem de boyle olması iyi, o zaman duruma hata diyemeyiz.

Özellikle, "yeni özellik eklemek zaman alıyor" düşüncesi, debugger
kullanmak için hiç geçerli değil. Sanki özellik azalsa, bu Linux icin
(ya da bütün yazılım sektörü için) bir problem olacak. Tam
tersi. Benim en büyük işlerimden biri Linux icin yazılan yeni
özelliklere 'hayır' demek, arayıpda bulmak değil.  Oh tabii, "ama her
şey çöktü ve fsck komutunu kullandım, ve sonuç olarak hic yardım
edecek bir ipucu vermedi, moralim bozuldu". Ne yapayım? Bu olana iki
turlu karşılık verirsin. Ya daha dikkatli olmaya başlarsın, ya da
çekirdek debugger programı istiyorum diye ağlarsın.  Açıkcası,
dikkatli olmayanları bu şekilde önceden elemek bence iyi. Bu kulağa
hissiz gelebilir, ne yazık ki öyle. Ve hani, 'Eğer sıcağa gelemiyorsan
mutfaktan çıkarsın' seviyesinde bile değil. Daha da derin. Yazılım
dünyasinda Darwin Kanunu'ndan bahsediyorum.  "Dünyada iki türlü
programcı var" demek cok soğuk ve hissiz. Ama öyle ve ben ikinci tür
ile çalışmayı tercih ederim.

Alışın.  Ben kötü bir adamım. Linux dünyasi niye bazen tersini
düşünüyor anlamıyorum. İnsanlar beni iyi zannediyor, fakat ben, sinsi
ve plancı bir adamım; Eğer bana göre daha güzel bir Linux ortaya
çıkacaksa, kaybedilen ve kırılmış kalpler beni zerre kadar
ilgilendirmiyor.  Bunu öylesine de soylemiyorum. Hakikaten iyi bir
adam değilim. İçimden gelerek ve yüzümü bozmadan "umrumda degil"
diyebilirim.  Benim düşünceme göre, çekirdek debugger olmaması,
programcıları otomatik olarak sorunlar hakkında değişik "seviyede"
görmeye sevkedecek. Debugger yoksa, "nasıl çalıştığını anlayım, sonra
tamir etmeye baslarım" düşüncesine girmezsiniz. Daha değisik şekilde
düşünmelisiniz. Herşeyi daha başka seviyede görmek isteyeceksiniz.
Bir bakıma "kaynak kod, işler kod" ayırımı bu, daha bile
fazlası... Kaynak koda bakmak değil, (tabii ki arada bakacaksınız, bütün en basit
debugger bunu sağlardı zaten), ama kaynak kodun üst seviyesine
bakacaksınız. Kavramların "anlamına" geleceksiniz.

Yani, debugger olmazsa, üst seviyeye atlayıp programin genel olarak ne
yapmaya uğraştığını anlamak isteyeceksiniz, tek bir satırın ne
yaptığını değil.  Zaten 'önemli' hataların coğunda debugger
programlarının faydası olması çok zor. Önemsiz aptalca hatalar da var
tabii, geçende olan truncate() hatası gibi mesela... Beni ilgilendiren
'önemli' hatalar. Gerisi detay. Eninde sonunda tamir olurlar nasıl
olsa.  Başkalarının aynı fikirde olmadığının farkındayım. Ben kimsenin
annesi değilim, eğer debugger kullanmak isterseniz kullanırsınız,
"lekelendiniz" diye düşünüp size arkamı dönecek falan değilim. Sadece,
bu programları kullanmak konusunda benden yardım beklemeyin. Tercihim,
programcıların debugger kullanmayı azaltması. O yüzden, bu yazılımlar
Linux ana dağıtımın parcası olmayacak. Ayrıca revaçta olan programlar
ismen bilinmiyorsa, ya da yokolurlar ise arkasından ağlayacağımı
zannetmiyorum.

Linus Torvalds





