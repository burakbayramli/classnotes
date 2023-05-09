# R ve BUGS

R dili istatistik hesaplamalari icin yaygin bir sekilde
kullaniliyor. WinBUGS (ve genelde BUGS kutuphanesi) ise bir simulasyon
teknigi olan Monte Carlo simulasyonlari icin kullanilmakta.R kurmak
icin Ubuntu uzerindesudo apt-get install r-base
r-base-devyeterli. BUGS biraz daha cetrefil. WinBUGS bir Windows
programi. Once wine kurariz:sudo apt-get install wineSonra su sayfadan
(cok dandik bir sayfa evet, bu gibi sebeplerden zaten bu ortamda
gelecek gormuyoruz -asiri Windows baglantisi bir diger faktor-)
WinBUGS exe dosyasi indirilir. Exe uzerinde sag click Windows kurulus
secenegini gosterir. Bunlar olduktan sonra R komut satirina girin ve R
ile Winbugs baglantisi kuracak install komutunu isletin:$ R>
install.packages("R2WinBUGS")Bu kurulum, daha sinirli isler yapabilen
egitim versiyonunu kurdu. Daha sofistike isler icin tam versiyona
gecmek lazim; sorun degil, herkese acik bir anahtar kurunca o da
oluyor.

Su adrese girin

http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/WinBUGS14_immortality_key.txt

Bu dosyanin tamamini bir yere kaydedin. Sonra WINE ile Bugs'i
baslatin. Open | File ile biraz once kaydettiginiz txt dosyasini
yukleyin. Tools | Decode menusunu secin, sonra cikan dialog kutusunda
"Decode All" dugmesine tiklayin. Kurulus tamam.Artik hem R hem BUGS
isletmeye haziriz. Normal R programlari isletmek icin normal "R -f
[dosya]" kullanilir. Eger script icinde plot, vs. komutlari varsa,
bunun ciktilari Rplots.pdf adli bir dosyaya yaziliyor.BUGS isletmek
icin ise su ornek kullanilabilir.

test-schools.Rlibrary(R2WinBUGS)model.file <-
system.file(package="R2WinBUGS","model","schools.txt")data(schools)J
<- nrow(schools)y <- schools$estimatesigma.y <- schools$sddata <- list
("J", "y", "sigma.y")inits <- function(){list(theta=rnorm(J, 0, 100),
mu.theta=rnorm(1, 0, 100),sigma.theta=runif(1, 0, 100))}parameters <-
c("theta", "mu.theta", "sigma.theta")schools.sim <- bugs(data, inits,
parameters, model.file, n.chains=3,
n.iter=5000)print(schools.sim)plot(schools.sim)

Ornegi isletmek icin "R -f test-schools.R". Arka planda WinBUGS'in
baslatip kapatildigini goreceksiniz, R oraya bilgi gecip sonucu geri
aldi, vs. WinBUGS bazen kapatilmayabiliyor, o durumda manuel olarak
siz kapatin, farketmez.Kaynak





