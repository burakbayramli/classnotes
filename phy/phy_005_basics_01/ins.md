Ataletsel Yöngüdüm Sistemleri (Inertial Navigation Systems -INS-)

Cep telefonlarında hareket ya da dönüş ölçümleri veren algılayıcılar
var. Peki niye bu ölçümler hep ``ivmesel''? Niye hız ölçülmüyor da ivme
ölçülüyor?  Arabalarda hız ölçen ibre var, aynısı cep telefonlarında olamaz
mı?

Fakat yöngüdüm sistemlerinin tarihini düşünürsek aslında burada istisna
olan arabalardaki hız ölçümü, çünkü araba durumunda yere direk bağlı bir
obje var (tekerlek), ve bu objenin alınan mesafe ile direk bir bağlantısı
var, bu durumda hız hesabı daha kolay. Fakat yere direk bağlı olmayan
taşıtlarda, mesela gemiler, füzeler, uçaklarda, elde hareketi tek temsil
eden ve ölçülebilen şey o taşıta etki eden ivmedir. Taşıta etki yapan
kuvvet ölçülebilir, ve bu kuvvet direk ivme ile oranlıdır, hız ile
değil. Bir arabada bile gaza basılınca koltukta geriye doğru itiliriz, ve
ivmelenmeyi hissederiz. Ama aynı hızda giderken pek bir şey
hissetmiyoruz. Gözlerimizi kapatsak 50 km/saat ya da 80 km/saat arasındaki
farkı anlayamayabiliriz. 

Bu sebeple INS'ler ivmesel ölçüm bazlıdır. İvmesel ölçüm nasıl toplanır?
Kuvvet ölçüm aletine etki ettiğinde alet içindeki bir yay gerilebilir
mesela ve bu gerilme ölçülür, ya da bir sıvı hareket eder, ya da ufak
rotorlar dönmeye başlar, dönme ölçülür. 

INS'in uzun bir tarihi var. Aslında bizim kafamızın içinde de ataletsel
algılayıcı var, dönüş yönümüzü ve kendi ivmelenmemizi bu algılayıcılar ile
anlayabiliyoruz. Bu organik ölçüm aletleri evrimsel tarihimizde balığa
benzeyen bir organizmadan bu yana gelişen bir şey. Bu sistem sayesinde
dengemizi sağlıyoruz, görsel olarak anlayamadığımız zaman duruşumuzu
hissediyoruz [3, sf. 13].

Fakat INS'in esas gelişme zamanı 1930-1945 arasında Almanya'da ve
Amerika'da oldu; 1. Dünya Savaşı sonrası Versailles Anlaşması sebebiyle o
savaşı kaybetmiş Almanya'nın bazı silahları geliştirmesi men edilmişti, bu
sebeple Almanya anlaşmaya dahil edilmemiş araçların, mesela füzeler gibi,
geliştirilmesine öncelik verdi. V-1 ve V-2 roketleri bu sırada gelişti ve
bu roketlerin INS'e ihtiyacı vardı. Aynı zaman diliminde C. Draper adlı bir
mühendis ABD ordusu için INS uygulamarı geliştirdi. Draper 1946'da bir
AR-GE projesi çerçevesinde gemiler, denizaltılar, uçaklarda
kullanılabilecek modern INS üzerinde denemeler yaptılar. Draper INS'in
babası sayılıyor.

MİT Üniversitesi de NASA için INS araştırmaları yaptı, Apollo Komuta
Modülü'ndeki sistem onlar tarafından geliştirildi.

İvmeden hız, oradan mesafe için daha önce gördüğümüz gibi iki entegrasyon
gerekiyor,

$$ v(t) = v(t_0) + \int _{t_0}^{t_1} a(s) \ud s $$

$$ x(t) = x(t_0) + \int _{t_0}^{t_1} v(s) \ud s $$

Bu hesapların sayısal olarak yapılmasının detayları [5]'te bulunabilir.

İvme, hızın vektörel kullanımı hakkında detaylar için [6].

[6] Bayramlı, {\em Çok Boyutlu Calculus - Ders 6}

[3] Grewal, {\em Global navigation Satellite Systems, Inertial Navigation, and Integration, 3rd Edition}

[5] Bayramlı, Hesapsal Bilim, {\em Sayısal Fonksiyonları Sayısal Entegre Etmek}



