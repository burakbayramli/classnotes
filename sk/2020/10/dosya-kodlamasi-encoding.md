# Dosya Kodlamasi (File Encoding)

Bir Word dosyasını Excel ile açamıyoruz, ya da PDF'i Word ile.. Bunun
sebebi dosya formatının değişik olması, veri içindeki bayt sıralaması
bir programda diğeri için gibi değil. Bu dosyalar çoğunlukla ikisel
(binary) olarak tanımlanıyor.

Fakat metin dosyalar her ne kadar basit editör, ya da komut satırında
mesela `cat` ile görülebiliyorlarsa da, aslında onların da bir iç
formatı var, ve bazı formatlar diğerleri ile uyumlu değil, özellikle
uluslararası karakterler varsa.

En basit farklılık mesela Windows (DOS) ve Unix metin dosyaları
arasında, Windows ile her satır hem yeni satır karakteri hem de CRLF
denen bir veri ile biter. Linux üzerinde iki format arasında gidip
gelmek için `unix2dos`, `dos2unix` kullandık uzun zaman, şimdi
`tofrodos` var.

Tüm sistemler için diğer bir farklılık kodlama (encoding)
olabilir. Mesela yaygın olan UTF8, ISO8859-9'dan farklıdır. Birinde
yazılan dosyanın enternasyonel karakterleri diğerinde bozuk
çıkabilir. Bu iki format arasında gidip gelmek için `iconv`
var. Örnek,

```
iconv file.tex -f ISO8859-9 -t UTF-8 -o out.tex
```

Not: Eğer bir dosyaya olduğu yerde aynı isimle `iconv` uygulanıyorsa,
bu bazı dosyalarda `Bus error` hatasına yol açabilir. Hatadan
kurtulmak için dosyanın çevrilmiş halini bir geçici dosyaya gönderip, sonra
oradan geri alıp orijinal dosyayı `mv` ezmek daha iyi olur.


Editor

Editörümüzün de bir metni gösterirken hangi kodlama ile iş yaptığını
bilmesi gerekir. Emacs'e bunu sonek çerçevesinde söylemek için,

```
(modify-coding-system-alist 'file "\\.tex\\'" 'utf-8)
```

diyebiliriz. Eğer `ISO8859-9` istersek

```
(modify-coding-system-alist 'file "\\.tex\\'" 'latin-5)
```

olurdu. Eğer LaTeX kullanıyorsak, onun da kodlamayı bilmesi
gerekir. Dosya başında, UTF8 için,


```
\usepackage[utf8]{inputenc}
```

gerekli.

