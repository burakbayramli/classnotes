# Açık Anahtar Şifrelemesi (RSA, Public Key Cryptography)

RSA şifrelemesi iki anahtar sistemiyle işler. Kullanıcı kendisine anahtar
üretmek istediğinde programı ona iki tane anahtar verir, biri açık diğeri gizli
tutulacaktır, kullanıcı gizli anahtarı Web'e koymaz, güvenli yerde saklar, kendi
diskinde tutar. Ve birazdan anlatacağımız matematiksel özellikler sayesinde ve
anahtarlar doğru seçildiğinde bir anahtar ile şifrelenen mesaj diğeri ile
çözülebilir. Bu her iki yönde de olabilir, gizli ile şifrele açık olanla çöz, ya
da açık olanla şifrele gizli olanla çöz.

Bu matematiksel özelliğin pek çok kullanım alanı olabilir. En bariz olanı bana
mesaj göndermek isteyen açık anahtarımı kullanarak şifreler, ve sadece ben
okuyabilirim, çünkü gizli anahtarım ile şifreyi sadece ben çözebilirim. Diğer
bir kullanım mesajın benden geldiğinin doğrulanabilmesi; eğer bir mesajı gizli
anahtarım ile şifreler ve yayınlarsam (broadcast) ve mesajın benden olduğunu
söylersem, insanlar benim açık anahtarımı alıp bu mesajın şifresini
çözebilirler, mesaj tabii ki okunabilir bir metin olacaktır, böylece mesajın
benden geldiğine inanılır.

RSA'yı zor (hatta çok büyük anahtarlar ile neredeyse imkansız) kırılabilen bir
sistem yapan özellik asal sayılar ve çarpanlara ayırmakla alakalı. Özet şudur:
İki asal sayıyı alıp çarparsam sonucu hem gizli hem açık anahtarımda
kullanabilirim (nasıl, birazdan göreceğiz), ve herkes bu çarpımı görebilir, ama
o çarpımı alıp hangi asal sayılardan olduğunu bulmaya uğraşmak, yani geri yönde
gitmek çok zordur. Burada "kolay'' ve "zor'' kelimeleri hesapsal çetrefillik
(computational complexity) bağlamında, zor demek tüm seçeneklerin denenmesi
gerektiği türden bir zorluk. Kısayol yok. "Kolay'' ise polinom hızda işleyen
algoritma demektir, toplama, çıkartma, sıralamak bu tür algoritmalardır.

```python
ls = 'abcdefghijklmnopqrstuvyz'
print ls[7],ls[8]
m = 78
```

```
h i
```

```python
p1 = 53; p2 = 59
n = p1*p2
print 'n =',n
phi_n = (p1-1)*(p2-1)
print 'phi(n) =', phi_n
```

```
n = 3127
phi(n) = 3016
```

```python
e = 3
d = int( (2 * (phi_n) + 1 ) / 3. )
print d
```

```
2011
```

$n,e$ haricinde her seyi sakla, onlar acik anahtar.

```python
import math
c = int( math.pow(m,3) % n )
print 'sifrelenmis mesaj', c
```

```
sifrelenmis mesaj 2375
```

```python
print pow(c,d,n)
```

```
78
```

Üstteki `pow` çağrısının üstel bir hesabın mod hesabı olduğuna dikkat,
bu fonksiyon üstel hesabı yapmadan mod hesabı yapmayı sağlıyor, çünkü eğer
önce üstel hesabı yapsak sonuç çok büyük olurdu, fakat mod dahil olunca bu
hesabı yapmanın daha hızlı bir yolu var. Hesabın temeli üstel almanın ardı
ardına çarpım yapmakla eşdeğer olduğuyla alakalı ve mod çarpım kuralı
alttaki gibidir,

$$ 
(a \cdot b) \mod m = [(a \mod m) \cdot (b \mod m)] \mod m
$$

Yani bir döngü içinde üstel kadar dönülür, ve o sırada üstteki formülün
uygun bir hali uygulanır. Detaylar için [5]. 

Kaynaklar

[3] Khanacademy, *RSA encryption: Step 4*, [https://youtu.be/UjIPMJd6Xks](https://youtu.be/UjIPMJd6Xks)

[4] Samid, *RSA -- The Math*, [https://www.youtube.com/watch?v=EOhLZRwxaVo](https://www.youtube.com/watch?v=EOhLZRwxaVo)

[5] Wikipedia, *Modular exponentiation*, [http://en.wikipedia.org/wiki/Modular_exponentiation](http://en.wikipedia.org/wiki/Modular_exponentiation)


