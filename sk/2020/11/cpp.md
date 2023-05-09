# C++ Dili, Derleme, Kodlama

C++ dilini kullanmak için ne bilmek gerekir? Ben kariyerime bu dili
kullanarak başladım, ve yüksek performanslı işlemesi gereken
programları bu dilde yazdım. Sonra başka dillere geçtik, ama geçende
uzun zaman sonrası bu dille işleyen bir programı derlemem gerektiğinde
farkettim ki aklıma kalıcı tuttuğum belli bazı şeyler var. Dilin
sözdizim vs detayları her zaman bir yerden bakılabilir. Ama genel
hatları, hep akılda olanlar şunlar;

Unix, Linux, Ubuntu ortaminda `gcc` ya da `g++` ile derleme
yapilir. Kurmak icin en basit yol,

```
sudo apt install build-essential
```

C++, aynen ondan önceki C gibi, tanım (header) .h dosyası ve .cpp
dosyaları üzerinden kodlanır. Tanımlara erişmek isteyenler .h
dosyalarını `#include` ile "dahil eder". Dahil etmek için dahil edilen
programların arandığı dizinleri `g++`a bildirmek gerekir, bunun için
`-I` kullanılır, `-I /vs/vs/dizin1` gibi.

Derlerken bir cpp dosyasını derleriz, eğer içinde bir `main()` ifadesi
varsa bu dosya direk bir işler kod (executable) üretebilir. Yoksa, bir
.o dosyası alınır, onu varsa, diğer .o dosyalarıyla bağlantılarız
(linking) ve işler kodu ortaya çıkartırız (bir .o içinde hala `main`
olması gerekir). Çok sık kullanılan .o dosyalarını paketlenip bir
kütüphane (library) haline getirmek te mümkün, bu dosyalar mesela XX
kütüphanesi için libXX.a ya da libXX.so dosyalarında olablir, onlar
`-lXX` ile bağlantılanır.

Kütüphanelerden .a kalıcı (statik) .so dinamik kütüphaneler
içindir. Aradaki fark kalıcı kütüphane kodu işler kodun parçası haline
gelir, işler kod dosyasının bu durumda daha büyük olduğunu
farkedersiniz zaten, dinamik ise işleme anında bu dosya otomatik
olarak "bulunur" ve hafızaya getirilir, işletilir. Bağlantılama için
de aranılan dizinler vardir, bu dizinleri `-L` ile `g++` a söyleriz.

Örnek görelim. En basit program (o tek satırlık `print` komutu python,
bu doküman içinden cpp dosyasını göstermek için kullanıldı sadece)

```python
print (open("ex1.cpp").read())
```

```text
#include <iostream>
using namespace std;

int main() 
{
    cout << "Hello, World!" << endl << endl;
    return 0;
}

```

Şimdi derleyip işletelim (o ünlem işaretini de yok sayabiliriz, yine
bu doküman için kullanıldı, Unix komut satırında ünlemsiz işletilir)

```python
! g++ -o /tmp/ex1.exe ex1.cpp
! /tmp/ex1.exe
print ('')
```

```text
Hello, World!
```

`-o` ile işler kod ismini ve yerini tanımlamış olduk.

Header dosyaları farklı modülleri derleme, bağlantılama örneği
görelim.

Bir kedi (cat) modülü olsun, onun .h dosyası

```python
print (open("cat.h").read().strip())
```

```text
#include <string>

class Cat
{
    std::string _name;
public:
    Cat(const std::string & name);
    void speak();
};
```

Bu bir C++ sınıfı (class). Sınıfta `speak` metotu var, o metotun
tanımı .h dosyasında. Gerçekleştirimi (implementatıon) .cpp dosyasında
olacak,


```python
print (open("cat.cpp").read().strip())
```

```text
#include <iostream>
#include <string>

#include "cat.h"

using namespace std;

Cat::Cat(const string & name):_name(name){}
void Cat::speak()
{
    cout << "Miyav " << _name << endl;
}
```

Dikkat edilirse .cpp dosyası da kendi .h dosyasını dahil ediyor, ona
bir kod sağlıyor. Derleyelim,

```python
! g++ -c cat.cpp  -o /tmp/cat.o  -Wall -O2
! ls -al /tmp/cat*
print ('')
```

```text
-rw-r--r-- 1 burak burak 4392 Nov  4 11:58 /tmp/cat.o
```

Bir .o dosyasının yaratıldığını görüyoruz, `-c` sayesinde.

Bazı ek seçenekler de kullandık, bunlar

`-Wall` tüm uyarıları (hatalardan daha zayıf, uyarı gelse bile
derlemek, işletmek mümkün) göster.

`-O2`, ikinci derece optimizasyon yap (oldukca kuuvetli), işler kod seviyesinde
bazı hızlandırma adımları böyle atılıyor.

Kedi kodunu kullanmak için bir `main()` yazalım,

```python
print (open("ex2.cpp").read().strip())
```

```text
#include <iostream>
#include <string>
#include "cat.h"

using std::cout;using std::endl;using std::string;
int main()
{
    string name = "Ali";
    cout<< "Kedim burada, " << name << "!" <<endl;
    Cat kitty(name);
    kitty.speak();
    return 0;
}
```

Şimdi `main` içeren kodu derleyelim ve bağlantılama ile işler kodu üretelim,


```python
! g++ ex2.cpp -Wall -O2 -o /tmp/ex2.exe /tmp/cat.o
! /tmp/ex2.exe
print ('')
```

```text
Kedim burada, Ali!
Miyav Ali
```

Hiç `-I` seçeneği gerekmedi, çünkü tüm dosyalar aynı dizinde, bu
durumda `#include` çift tırnak içinde aynı dosya içinden dahil
edebilir. Ama farklı dosyalar varsa `#include <..>` komutunun işlemesi
için `-I` gerekli olacaktı.

Bir diğer bilgi, çoğu yaygın kullanılan kütüphane Ubuntu'da `apt-get`
ile kurulunca header dosyalarını ve kütüphane dosyalarını yaygın /
bilinen ana dizinler altına koyduğudur. Bu durumda o yerleri ayrıca
derleyiciye belirtmeye gerek yoktur çünkü `g++` bu iyi bilinen yerler
altında arama yapmayı bilir. Fakat kütüphane ismini hala belirtmek
gerekir, mesela OpenGL kullanıyorsak, [şu yazıda](../08/pyopengl.html)
gördük, `apt-get install libgl1-mesa-dev ..` vs ardından `-lGL -lGLU
-lglut` gibi o kurulmuş kütüphaneleri bağlantılamak istediğimizi
özellikle belirtmek lazım.

Dosyalar [ex1.cpp](ex1.cpp), [ex2.cpp](ex2.cpp), [cat.h](cat.h), [cat.cpp](cat.cpp)

Niye C++

C++'in hızlı işlediği herkes tarafından bilinir. Ama mesela Java'ya
yapılan optimize edici ekler ile Java C++ hızına yaklaşmadı mı?

Çoğu bakımdan bu doğru fakat C++ hala bazı şeyleri yapmamıza izin
veriyor; mesela paketten çıktığı haliyle Java'da çöp toplayıcı
(garbage collector) açık, C++ da hiç yok. Çöp toplayıcı belli
aralıklarla bir arka plan kodunun işleyip hafızayı temizlemesi
anlamına geliyor, ve programcının direk tanımlamadığı bu işlem kontrol
bağlamında rahatsız edici olabiliyor, ne zaman işliyor, ne hızda,
vs. bu "bilinmezlik" performansta düşüş, en azından sürpriz yaratabiliyor.

Diğer cevap kültürle alakalı; C'nin devamı C++ uzun yıllardır ortada,
ve C ile beraber "performans için gidilen dil" olarak ün yaptı, hızlı
hesap isteyen bilimciler yıllardır ona geçiş yaptı, ve bir sürü
kütüphane, yardımcı kod bu dil etrafında şekillendi. 

Make

Derleme yapmak için teker teker her dosya üzerinde `g++` işletmek
külfetli olabilir. Make programı ile dosya sonekleri arasında "gidiş
kuralları" tanılanabiliyor, ve birçok derleme ile alakali komutlar tek
bir dosyada toplanabiliyor. Mesela bir `.cpp` den `.o`'ya gitmenin
yolu "vsvs komutudur" denebilir, böylece iki dosya tipi arasında bir
bağ yaratmış oluruz, hatta bu bağ dosya değişimlerini, zamanları bile
kontrol edebilir, mesela bir kere A.o ürettiysek, sadece ve sadece
onun temel aldığı A.cpp değişmiş ise tekrar derlemek.

Make programını işletmek için komut satırında `make` yazmak yeterli,
olağan durumda program aynı dizinde olan `Makefile` adlı bir dosya
arar. Bu dosyanın içeriği (üstteki örnek için) şuna benzeyebilir,

```
CC=g++
CFLAGS=
LIBS=-lm
OBJ = cat.o ex2.o
     
%.o: %.cpp
	$(CC)-c -o $@ $< $(CFLAGS)

kedi.exe: $(OBJ)
	$(CC) -o $@ $^ $(LIBS) 

clean:
	rm -f *.o 
	rm -f *.exe
```

Makefile içinde hedefler vardır, `make hedef` ile bu hedefler ayrı
ayrı da işletilebilir, ama tanımlanmamışsa olağan hedef ilk hedeftir,
üstteki örnekte bu `kedi.exe`. Hedefin sağında onun bağlı olduğu başka
hedefler / dosyalar olabilir, bizde `kedi.exe` için `$(OBJ)`
gerekiyor, bu bir değişken sadece, tekrarlamamak için böyle yaptık,
içinde `cat.o ex2.o` var, yani nihai isler kod icin bu iki `.o`
dosyasi gerekiyor. Bu dosyaları da ayrı birer hedef yapabilirdik, ama
daha hızlı kodlamak için bu hedefleri bir genel sonek kuralı üzerinden
tanımladık, `%.o: %.cpp` tanımı bu işte. Bu kural herhangi bir `o`
dosyası için hangi komutu işleteceğini biliyor, `$(CC) -c -o ...` diye
giden komut ile..  Ve `make` böyle geriye geriye gide gide bize
gereken tüm dosyaları ortaya çıkartacak ve en sonunda `kedi.exe` ile
nihai `g++` komutunu işletip işler kodu ortaya çıkartacak.

`$(CFLAGS)` su anda bos ama `-I` kullanımı gerekseydi onu `$(CFLAGS)`
içine koyabilirdik, böylece otomatik olarak derleme işleminin parçası
haline gelirdi.


İlk işlettiğimizde

```
g++ -c -o cat.o cat.cpp 
g++ -c -o ex2.o ex2.cpp 
g++ -o kedi.exe cat.o ex2.o -lm 
```

görürüz, ve `kedi.exe` yaratılmış olur. Eğer tekrar işletsek,

```
make: 'kedi.exe' is up to date.
```

mesajını görürdük. Temizlik yapmak istersek, `.o`, `.exe` dosyalarını
silmek için, `make clean` işletebiliriz.

Bağlantılar

Faydalı bazı Türkçe kaynaklar altta bulunabilir

[C++ ile Programlama Ders Notları](https://drive.google.com/uc?export=view&id=1QXYMbZkgVj6xM0ePCqL4wYKi-O3yE2l-)

[C++ Programlama Youtube Dersleri](https://www.youtube.com/watch?v=BOoPd7SJvuk&list=PLoPK1OnMH5178ZBJNAVBA2FXcBMOYmv_n)


Kaynaklar


[2] https://linuxconfig.org/how-to-install-g-the-c-compiler-on-ubuntu-18-04-bionic-beaver-linux

[3] https://stackoverflow.com/questions/58058/using-c-classes-in-so-libraries

