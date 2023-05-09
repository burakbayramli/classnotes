# Çok Çekirdekli İşlemciler

Mahout sitesinde bir makalenin işlemciler ile alakalı bölümünde güzel
bir saptama:

"Mikroişlemcilerde frekans bazlı ölçekleme yapmak, yani işlemcinin
saat hızını daha yükselterek daha yüksek performans elde etmek güç
kaynağı kullanımı sınırlarına çarpmaya başladı. İşlemcinin boyutu
küçüldükçe güç sızması (power leakage) yaşanıyor. Diğer taraftan Moore
kanununa göre işlemcilerin yoğunluğu her nesilde ikiye
katlanacaktır. O zaman frekansı sabit tutarak ama her cipteki işlemci
çekirdeği ikiye katlayarak hala düşük güç kullanımında devam
edebiliriz ve işlemci gücünü ikiye katlamış oluruz. Bu durum, mikroçip
endüstrisini çok çekirdekli mimarilere itmiştir".

Yani söylenmek istenen tek işlemcinin transistör yoğunluğu ile
oynayarak artık daha fazla hız elde edemiyoruz, bazı sınırlara
toslamaya başladık. O zaman çok çekirdekli mimarı, aynı hızda ama
paralel çalışan ek çekirdekler koyarak hızlanmayı sağlayacaktır.Bunun
programcılar için getirdikleri faydalar / sorunlar nelerdir?
Programcılar daha paralel kodlama bağlamında düşünmeye alışmalı,
programlama dilleri bu servisleri daha rahat sağlayabilmeli. Referans
verilen makale paralelizasyon fikrini yapay öğrenim konularına
uygulamaya çalışıyor mesela. Yelpazenin diğer ucunda ise, daha fazla
paralel çalışan "makinaların" sayesinde devasa boyutlarda veri
işleyebilme konuları var. Aslında bu iki trend birbiriyle yakında
alakalı. Hem tekil makinaların fiyatı ucuzlarken, hem de tekil
makinaların içindeki paralel çekirdeklerin sayısı artıyor. Yelpazenin
her noktasında paralelizasyona doğru bir gidişat yaşanıyor.Bu konu
hakkında hemen bir şey yapılması gerekli demiyoruz; sadece akılda
tutulması gereken bir konu.





