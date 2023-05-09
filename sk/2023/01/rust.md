# Rust Programlama Dili

Rust diline ilk kez Rust'in Linux 6.1'e dahil edileceğini duyduktan
sonra [2] dikkat etmeye başladık; şimdiye kadar Linux çekirdek
seviyesinde sadece tek bir dili destekledi: C. Bir süre önce C++
dilini dahil etme çabaları hüsrana uğraşmıştı. Eğer çekirdek
programcıları, ki en temel seviye "makinaya yakın" ve çetrefil türden
kodlama ile uğraşıyorlar, bir dili sevdilerse bu dilde faydalı
özellikler olmalıdır.

Hakikaten de Rust dilinin pek çok farklı kitleye hitap edebildiğini
görüyoruz. Evet C, C++ dilleri kendilerini ispat ettiler fakat bunun
bedeli hafıza erişimindeki güvenlik oldu.  Java, C# gibi üst seviye
diller hafıza erişiminde güvenlik sağladılar, fakat doğal olarak
hafızaya direk erişime engel getirdiler, programlama esnekliğine
kısıtlama getirdiler ve ne zaman devreye gireceği belli olmayan çöp
toplayıcı (garbage collector) programda istenmeyen anlarda
duraksamalara, geciktirmelere sebep olduğu için istenmeyen bir
performans kaybını yanlarında getirmiş oldular [3]. Rust modern, açık
yazılım bir sistem programlama dilidir, ve üç dünyanın en iyi
özelliklerini taşır; Java'nın değişken tipleme güvenliği, C++'in
yapısal açıklığı, hızı, verimliliği, ve çöp toplayıcı olmadan hafıza
erişim güvenliği.

Linux programcılarının niye bu dili sevdiği açık; Bildiğimiz gibi C
ile hafızaya direk erişim yapabiliriz, bir dizine hafızada yer ayırıp
(allocate) erişiriz, ama gösterge (pointer) ile ayrılan yerden ötesine
geçmek mümkündür, ve bu bazen `segfault` denen istenmeyen hafıza
erişim hatalarına, hatta güvenlik açıklarına sebep olmakdadır. Rust bu
tür kullanımlarına hafıza erişimine performans kısıtlaması getirmeden,
ya da erişimi yasaklamadan bir kontrol mekanizması getirmiştir.

Rust ile hızlı işler kod (executable) üretmek mümkündür. Bazı diller
sözdizimlerinde az tanım gerektirirler, ve yükü yorumlayıcıya
verirler, bazı diller güçlü tipleme üzerinden derleyiciye daha çok iş
yaptırırlar (potansiyel hataları önceden bulma, sonuç işler kod için
optimizasyonlar yapma) ve bu şekilde daha hızlı kod üretmeye
uğraşırlar. Rust bunlardan ikinci kategoriye daha yakındır. Mesela
derleyici metot çağrılarını statik olarak çözümlemeye uğraşır, bazı
diller bu çözümlemeyi işlem anında "dinamik" olarak yapmaya
uğraşabilir, statik yöntem daha hızlıdır.

### İlk Kullanım

Kurmak için Ubuntu Linux uzerinde,

```
curl https://sh.rustup.rs -sSf | sh

rustup update
```

yeterli. Tüm konsolları kapatıp yeni bir tane başlatalım, artık
`rustc` derleyicisini direk kullanabiliriz. Alttaki kod Python, bu
yazıda sunum amaçlı yazıldı,


```python
def rcode(infile): print (open(infile).read())
```

İlk kodu `rust1.rs` içinde görelim,

```python
rcode("rust1.rs")
```

```text
fn main() {
    println!("Merhaba Dunya!");
}

```

Derleyelip işletelim,

```python
! rustc -o /tmp/rust1.exe rust1.rs
! /tmp/rust1.exe
```

```text
Merhaba Dunya!
```

Görülen program C, C++, ve Java yapısını andırıyor, `main` tanımı var
mesela, süslü parantez kullanımı mevcut.

Rust kodlama, işleyiş açısından C/C++ dillerine benziyor, bir derleme,
bağlama (linking) ve işler kod (executable) yaratma süreci var, ve
dilin sözdizimindeki özellikler sayesinde işler kodun optimize
edilmesi kolaylaşıyor, ve C hızına erişir bir sonuç elde ediyoruz.

Şimdi yeni bir fonksiyon yazalım, bir sayı alsın ve ona bir sayı eklesin,

```python
rcode("rust2.rs")
```

```text
fn add_one(a: u8) -> u8 {
    let g = a + 1;
    g
}

fn main() {
    let x = 1;
    let z = add_one(x);
    println!("{}", z);
}

```

```python
! rustc -o /tmp/rust2.exe rust2.rs
! /tmp/rust2.exe
```

```text
2
```

Fonksiyonun dönüş değeri en son satırda ismi üzerinden belirtiliyor,
bir döndürme komutu, mesela `return` gibi, kullanılmamış. Bu durumda
fonksiyon ortasından değer döndürmek için içinde olunan bloktan bir
şekilde çıkılmalı, son satıra erişilmeli, oradan dönüş yapılmalı
herhalde.

Güçlü tipleme (strong typing) kullanımına dikkat; bazen Rust tanım
yoksa bile bir tipi tahmin edebilir, varsa onu kullanır ama her zaman
Rust statik tipleme, derleme seviyesinde bir tip bilgisine sahiptir.
Bu derleme sırasında yapılabilecek optimizasyonlara yardım eder.

Not: İlerlemeden önce bu yazıda Rust kodu gösteren, derleyen ve işleten
bir yardımcı kod yazalım,

```python
import subprocess, os, sys

def rshow_comp_run(infile):
   print (open(infile).read())
   file = os.getcwd() + "/" + infile
   # derle
   cmd = "rustc -o /tmp/%s %s" % (infile.replace(".rs",".exe"),file)
   print (cmd)
   process = subprocess.Popen(cmd.split(" "), stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.STDOUT, shell=False)
   output, err = process.communicate()
   res = str(output).split("\\n")
   for x in res: print (x)
   if "error: aborting" in str(output): return
   # islet
   cmd = "/tmp/%s" % infile.replace(".rs",".exe")
   process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.STDOUT, shell=False)
   output, err = process.communicate()
   res = str(output).split("\\n")
   for x in res: print (x)
```

Artık tek çağrı ile üç işi birarada yapacağız.

### Değişkenler, Kapsamlar (Scope)

Rust'in hafıza hatalarının önüne geçmek için getirdiği bir değişiklik
sahiplenme özelliği. İlginç bir özellik bu, mesela eğer obje üzerinde
kopyalama desteği yok ise eşittir işareti bir objeyi bir değişkenden
diğerine taşır, kopyalamaz, referans arttırmaz. Birinden alıp diğerine
verir. Bu durum aynı kapsam içinde bile gerçekleşebilir. Mesela [6, sf. 113],

```python
rshow_comp_run("rust3.rs")
```

```text

fn main() {
    let num1 = 1;
    let num2 = num1;
    let s1 = String::from("meep");
    let s2 = s1;
    println!("Number num1 is {}", num1);
    println!("Number num2 is {}", num2);
    println!("String s1 is {}", s1);
    println!("String s2 is {}", s2);
}

rustc -o /tmp/rust3.exe /home/burak/Documents/classnotes/sk/2023/01/rust3.rs
b'error[E0382]: borrow of moved value: `s1`
 --> /home/burak/Documents/classnotes/sk/2023/01/rust3.rs:9:33
  |
5 |     let s1 = String::from("meep");
  |         -- move occurs because `s1` has type `String`, which does not implement the `Copy` trait
6 |     let s2 = s1;
  |              -- value moved here
...
9 |     println!("String s1 is {}", s1);
  |                                 ^^ value borrowed here after move
  |
  = note: this error originates in the macro `$crate::format_args_nl` which comes from the expansion of the macro `println` (in Nightly builds, run with -Z macro-backtrace for more info)

error: aborting due to previous error

For more information about this error, try `rustc --explain E0382`.
'
```

Burada hata ortaya çıktı çünkü String tipi kopyalama özelliğini kodlamamış,
ama 1 değerinin tipi tam sayıların bu desteği var, bu sebeple String s1'den
s2'ye taşındı, taşınınca ilk referans geçersiz hale geldi.

Fonksiyonlara geçilen değişkenler için de aynı durum geçerli; eğer değişkeni
bir fonksiyona veriyorsam sahiplenme değişiyor, artık kullanamıyorum. 

```python
rshow_comp_run("rust4.rs")
```

```text
fn take_the_s(s: String) {
}
fn main() {
    let s = String::from("string");
    take_the_s(s);
    println!("s is {}", s);
}

rustc -o /tmp/rust4.exe /home/burak/Documents/classnotes/sk/2023/01/rust4.rs
b'warning: unused variable: `s`
 --> /home/burak/Documents/classnotes/sk/2023/01/rust4.rs:1:15
  |
1 | fn take_the_s(s: String) {
  |               ^ help: if this is intentional, prefix it with an underscore: `_s`
  |
  = note: `#[warn(unused_variables)]` on by default

error[E0382]: borrow of moved value: `s`
 --> /home/burak/Documents/classnotes/sk/2023/01/rust4.rs:6:25
  |
4 |     let s = String::from("string");
  |         - move occurs because `s` has type `String`, which does not implement the `Copy` trait
5 |     take_the_s(s);
  |                - value moved here
6 |     println!("s is {}", s);
  |                         ^ value borrowed here after move
  |
  = note: this error originates in the macro `$crate::format_args_nl` which comes from the expansion of the macro `println` (in Nightly builds, run with -Z macro-backtrace for more info)

error: aborting due to previous error; 1 warning emitted

For more information about this error, try `rustc --explain E0382`.
'
```

Eğer hatadan kurtulmak istiyorsam eldeki String objesini değil onun
kopyasını verebilirim,

```python
rshow_comp_run("rust5.rs")
```

```text
fn take_the_s(s: String) {
    println!("Fonksiyonda s degeri {}", s);
}
fn main() {
    let s = String::from("string");
    take_the_s(s.clone());
    println!("s is {}", s);
}

rustc -o /tmp/rust5.exe /home/burak/Documents/classnotes/sk/2023/01/rust5.rs
b''
b'Fonksiyonda s degeri string
s is string
'
```

Görüldüğü gibi hata kalmadı.

Alternatif olarak Rust dilinin tabiriyle bir referans yaratıp onu
fonksiyona "borç verebilirim", referans yaratma C++ dilindeki
kullanıma benziyor, bir `&` kullanıyoruz,

```python
rshow_comp_run("rust6.rs")
```

```text
fn take_the_s(s: &String) {
    println!("Fonksiyonda s degeri {}", s);
}
fn main() {
    let s = String::from("string");
    take_the_s(&s);
    println!("s is {}", s);
}

rustc -o /tmp/rust6.exe /home/burak/Documents/classnotes/sk/2023/01/rust6.rs
b''
b'Fonksiyonda s degeri string
s is string
'
```

Sahiplenme hataların nasıl önüne geçer? Burada yapılan bir muhasebe
kontrolü, çift kayıtlı defter tutma yöntemi gibi; eğer bir değişken
diğerine eşitleniyor ve önceki değişken isminin kullanımı kodların
yüzde doksanında bir tür hataya yol açıyorsa o zaman kendi kendimizi
kontrol bağlamında derleyici seviyesinde dile böyle bir kısıtlama
getirebiliriz. İnsanlar unutabilir, gözden kaçırabilir, dil bu
eksikleri yakalamak için yardım etmelidir.

Gözden kaçabilecek şeyleri belirtilmiş (explicit) hale getirmek,
tanımını zorlamak yine benzer bir muhasebe tekniği olabilir. Mesela
alttaki Python programı çoğunlukla işler, ama bazı durumlarda metot
bir `None` döndürecektir, bu beklenmeyen durum istemci tarafında pek
çok hataya yol açabilir.

```python
from random import random
class Obje1:
    def goster(self):
        print("Merhaba!")
        
def rasgeleIsleBozul():
    if random() > 0.1: # yuzde 90 sansla alttaki isler
        return Obje1()

while True:
    o = rasgeleIsleBozul()
    o.goster()
```

```text
Merhaba!
Merhaba!
Merhaba!
Merhaba!

AttributeErrorTraceback (most recent call last)
<ipython-input-1-94690a78715c> in <module>
     12 while True:
     13     o = rasgeleIsleBozul()
---> 14     o.goster()

AttributeError: 'NoneType' object has no attribute 'goster'
```

Burada problem Python'un bir dönüş tipi, komutu zorlamaması, bu
sebeple eğer fonksiyon planlamayan şekilde sona erişip döndüyse
hesapta olmayan `None` dönüş tipi elde etmemizdir. Her seferde bir
obje bekleyen istemci tarafı da boş bir referans elde edince tabii ki
patlayacaktır. Rust sözdizimi içine dönüş değerini fonksiyonun son
satırında olmaya zorlayarak bizi bu tür hatalardan koruyabilir.
Gözümüz alışır, kodlarken hep fonksiyonun son satırına bakarız, Rust
orada bir tanım yoksa bize derleyici seviyesinde hatırlatabilir,
muhasebemiz böylece daha çetrefil, sağlam hale gelir.

### Kaynaklar

[1] https://doc.rust-lang.org/reference/destructors.html

[2] https://www.zdnet.com/article/linus-torvalds-rust-will-go-into-linux-6-1/

[3] Eshwarla, Practical System Programming for Rust Developers

[4] [Stackoverflow](https://stackoverflow.com/questions/36136201/how-does-rust-guarantee-memory-safety-and-prevent-segfaults)

[5] McNamara, Rust In Action

[6] Kaihlavirta, Mastering Rust


