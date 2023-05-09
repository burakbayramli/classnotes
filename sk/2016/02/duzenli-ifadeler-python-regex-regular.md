# Düzenli İfadeler, Python, Regex (Regular Expressions)

Düzenli İfadeler, Python, Regex (Regular Expressions)

Bir metin içinde bir kelime arıyorsak,

```
s1 = "aaabbabbabbbbee 44 xxxxx xxxxxxx"
print "aaa" in s1

True
```

teknigini kullanabiliriz. Fakat ya "bir ya da daha fazla a ardından
bir ya da daha fazla b gelen tüm bölümleri bana göster" diye bir kalıp
arıyorsak? Bu gibi durumları basit string araması ile yapamayız, daha
kuvvetli bir dil lazım.

Düzenli ifadeler bir metin bloğunu temsil eden tariflerdir; onları bir
algoritma olarak değil bir beyan şeklinde görmek lazim; birden fazla
metin çeşidini temsil edebilecek bir kalıp tanımlıyoruz, ve bu kalıbın
bir metin içinde olup olmadığını bilgisayara soruyoruz. Mesela onceki
ornek icin

```
a+b+
```

ifadesi kullanılır. + işareti bir veya daha fazla demek, ondan önceki
harflerin tekrar edebileceğini söylüyor. Metin üzerindeki uyumu nedir?

```
import re
print re.match('a+b+', s1) != None

True
```

Bir uyum var. Fakat daha iyisini yapabiliriz, uyan bölümleri gösterelim,

```
import re
res = re.findall('a+b+',s1)
print res

['aaabb', 'abb', 'abbbb']
```

+ yerine * kullanabilirdik, * işareti "0 ya da daha fazla" anlamına geliyor.

Hem + hem de * açgözlü operatörlerdir, yani uydurabildikleri büyük
veriyi almak isterler. Bazen bunu istemeyiz, açgözlülüğü durdurmak
için ? kullanılır.

```
print re.findall('a+?b+?',s1)

['aaab', 'ab', 'ab']
```

Bir örnek daha

```
s2 = "jjjjddddddddd<H1>title</H1>xxxxxxxxxxxxx"
print 'acgozlu', re.findall("<.*>", s2)
print 'degil', re.findall("<.*?>", s2) # acgozlu degil

acgozlu ['<H1>title</H1>']
degil ['<H1>', '</H1>']
```

Karakterlerin kendisi yerine onları temsil eden "joker" semboller de
kullanılabilir. Nokta karakteri "ne olursa olsun" anlamına gelir,
mesela 1. metin üzerinde 'a' ile başlasın '4' ile bitsin, arada ne
olursa olsun diyorsak,

```
print re.findall('a.*?4',s1)
['aaabbabbabbbbee 4']
```

[] işareti içine konan her karakter aynı bir seçenek olarak kabul
edilir, yani "bu karakterlerden herhangi biri olabilir" ifadesi böyle
kodlanır. Mesela [abc] diyorsak, 'a','b','c' harflerinden herhangi
biri mümkündür. Aralık tanımlamak ta mümkün, mesela [0-4] demek 0 ile
4 arasındaki tüm sayılar demektir, ya da [a-f] "a ile f arası tüm
harfler". Bunların bir karışımı olabilir, [0-9A-Fa-f] gibi.

```
s3 = "xxxx4534yyyyy 444a44444444"
print re.findall("[3-5a]+",s3)
print re.findall("[a-z]+",s3)

['4534', '444a44444444']
['xxxx', 'yyyyy', 'a']
```

Bir Kalıp İçindeki Daha Ufak Kalıbı Çıkartmak

Bazen aradığımız bir kalıp daha büyük bir kalıp içindedir,  HTML
kazıma (scraping) yaparken bu tür ihtiyaç çok ortaya çıkar, diyelim ki
döküman içinde 222.444.222.111'ye benzeyen yani noktalarla ayrılmış
sayılar grubu pek çok yerde var, ama biz bunlardan sadece <tr>
.. </tr> içinde olanlarını istiyoruz. Örnek veri

```
s4 = "ddddddhhhhhhf jjjjjj 111.222.33.444 dddddd ddddddddddd" + \
"xxxxxxxxxx xxxxxxxxxj kkkkkk kkkkkk <tr>xxxxx111.222.888.444yyyy</tr> " + \
"xxxxxxxxxx xxxxxxxxxj kkkkkk kkkkkk <tr>xxxxx111.555.888.444yyyy</tr> " + \
"dddddd jjjjjjj 333.222.33.444 111.222.33.444 jjjjjjjjjjjj"
Bu çağrıyı yapmanın iki yolu var. Birincisi,

regex = "<tr>x+(\d+\.\d+\.\d+\.\d+)y+</tr>"
for x in re.finditer(regex, s4, re.DOTALL):
   print x.group(0)
   print x.group(1)

<tr>xxxxx111.222.888.444yyyy</tr>
111.222.888.444
<tr>xxxxx111.555.888.444yyyy</tr>
111.555.888.444
```

Bu çağrı hem dış, hem iç (parantez arasında olan) düzenli ifadenin
sonucunu verebilir, group() ile hangisini istediğimize karar
veririz. Eğer hep parantez içindekini istiyorsak, ki çoğunlukla böyle
olur, direk group(0)'u veren findall'u kullanabiliriz,

```
print re.findall(regex, s4)
['111.222.888.444', '111.555.888.444']
```

Tüm Veriye Bakmak

Şimdiye kadar gördüğümüz tüm teknikler satır bazında işler; eğer bir
dosyayi açıp satır satır işliyorsak bu problem değil. Fakat bazen bir
satırda başlayıp diğer bir satırda biten kalıpları bulmamız
gerekebilir. Mesela 444 ile başlayıp 5555 ile biten bir kalıp farklı
satırlarda ise ne yapacağız? Alttaki veride yeni satır (newline) `\n`
işaretine dikkat, bu karakter bir dosya içinde bir satırın sonunda
mevcuttur.

```
s5 = "xxxxxx xxxxxxxxxxx 44444 xxxxx\n" + \
     "555555 xxxxx"
print re.findall("4+.*?5+", s5)

[]
```

Hiç sonuç gelmedi. Fakat eğer re.DOTALL seçeneğini geçersek, bu nokta
ifadesinin yeni satır karakterine de uyum yapabileceğini söyler,
böylece

```
print re.findall("4+.*?5+", s5, re.DOTALL)

['44444 xxxxx\n555555']
```

sonucu bulunabilir.

Eğer belli bir kelimeden sonra tekrarlanan kalıpları bulmak
istiyorsak, mesela sadece ccccccccccc kelimesinden sonra gelen ip
adreslerini almak,

```
s6 = """
ddddddhhhhhhf jjjjjj 111.222.33.444 dddddd ddddddddddd
ccccccccccc
xxxxxxxxxx xxxxxxxxxj kkkkkk kkkkkk xxxxx111.222.888.444yyyy
xxxxxxxxxx xxxxxxxxxj kkkkkk kkkkkk xxxxx111.555.888.444yyyy
dddddd jjjjjjj 333.222.33.444 111.222.33.444 jjjjjjjjjjjj
"""

regex = ".*?ccccccccccc|(\d+\.\d+\.\d+\.\d+)"
res = re.findall(regex, s6, re.DOTALL)
```

Burada ilginç bir numara kullanıldı; | komutu ile metni iki bölüme
ayırmış olduk, bu komut "ya onu ya bunu" şeklinde bir seçim yaptığı
için önce sol tarafa uyacaktır, fakat belli bir noktadan sonra sağ
kısmı uyduracaktır ve o kısım içinde gruplama vardır, ve o gruplamanın
içi doldurulur. Bu komut

```
['', '111.222.888.444', '111.555.888.444', '333.222.33.444', '111.222.33.444']
```

sonucunu verir, baştaki boş kelime kısmini filter ile çıkartabiliriz,
mesela filter(None, res) gibi.

Metin Değiştirmek

`re.sub` ile duzenli ifadeler kullanip degistirme islemi de
yapabiliriz. Mesela

```
line = '{\\em Bir kelime}'
print (line)
s = re.sub(r'{\\em (.*?)}', r'*\1*', line)
print (s) 
```

Yani Dİ ile bir cümle içindeki bir kalıbı yakaladık, sonra uyan kalıba
`\1` ile referans yaptık (Dİ'da kaç parantez varsa onlar numaralanır,
ikinci, üçüncü parantezler `\2`,`\3` diye giderdi), ve uyan bölümü
daha genel başka bir çıktı içinde kullandık.


Tek Karakter Uyumu

`\d` Tek haneli bir sayı
`\D` Tek haneli sayı haricinde her şey
`\w` Bir alfabe harfi (a ile z, A ile Z arası)
`\w` Alfabe haricinde herhangi bir karakter
`\s` Beyaz boşluk (SPACE, TAB) karakteri
`\S` Beyaz boşluk haricinde herhangi bir karakter
`^` Satır başlangıç karakteri
`.` Herhangi bir karakter
`$` Satır sonu karakteri

Tekrarlar

`*`  0 ya da daha fazla
`+`  1 ya da daha fazla
`?`  1 ya da hiç
`{n}` n kere
`{n,}` en az ne kere
`{n,m}` en az ne kere, ama m'den fazla değil
`str1 | str2` Ya ifade1 ya da ifade2 bulunacak 

"abbbccdaabccdde" üzerinde bazı düzenli ifade örnekleri

Abc Uymaz. `\$d` içinde hiç "A" yoktur yâni "Abc" bulunamaz.

abc Uyar.

^abc Uymaz. Çünkü ^ meta-karakteri dizinin başında anlamındadır ve
"abc" alt dizisi PVerb!\$d!'nin başında değildir.

abc> Uymaz. Çünkü > meta-karakteri dizinin sonunda anlamındadır ve
"abc" alt dizisi $d 'nin sonunda değildir.

^ab*c Uyar, çünkü dizinin basinda "a" ve ardından ``sıfır veya daha
fazla b'' ve ardından "c" var! Bu düzenli ifadede olan * karakteri
hemen solundaki karakter için ``sıfır veya daha fazla kez tekrar
eden'' anlamında bir meta karakterdir.

aH*bc Uyar. Çünkü dizide "a" ve ardından sıfır veya daha fazla "H" ve
ardinda "bc" var!

aH+bc Uymaz. Çünkü dizide "a" ve ardından en az bir tane "H" ve onun
ardında "bc" seklinde bir düzen yok! Bu Duzenli ifadede olan "+"
karakteri hemen solundaki karakter için "bir veya daha fazla kez
tekrar eden" anlamında bir meta-karakterdir.

a*bc Uymaz. Çünkü dizinin içinde hiç "a" ve ardından gelen "*" yok! Bu
örnekte "*" bir meta-karakter olarak değil; basit anlamıyla bir
asterisk karakteri olarak kullanıldığı için önündeki "" ile
işaretlenmiştir.

a+bc Uymaz. Çünkü dizinin içinde hiç "a" ve ardindan gelen "+" yok! Bu
örnekte "+" bir meta-karakter olarak değil; basit anlamıyla bir artı
işareti olarak kullanıldığı için önündeki "" ile işaretlenmiştir.

a c Uymaz. Çünkü dizinin içinde hiç "a" ve ardindan gelen "" yok.

a.*b Uyar Çünkü dizide ``a ve aralarında birşeyler ve sonra b'' var!
``.''  (nokta) meta-karakteri herhangi bir karaktere uyar; ardından
gelen ``*'' ile birlikte (yâni .*!) birşeyler olarak okunabilir.

d.*a Uyar Çünkü "birşey" anlamındaki noktanın ardından gelen "*"
meta-karakteri sıfır veya daha fazla herhangi birşey anlamındadır.

d.+a Uyar Çünkü "birşey" anlamındaki noktanın ardından gelen
meta-karakter bir +! ; Yâni bir veya daha fazla "herhangi birşey"

da? Uyar Çünkü dizide "d" ve ardından bir veya sıfır tane "a" gelen
alt dizi var.

Eski Anlatım

Düzenli İfadeler, Perl, Kurumsal Java kitabından, [PDF](https://github.com/burakbayramli/classnotes/raw/master/sk/2016/02/regex-kj.pdf)


