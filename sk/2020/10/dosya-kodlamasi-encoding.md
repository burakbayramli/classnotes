# Dosya Kodlamasi (File Encoding)

Bir Word dosyasını Excel ile açamıyoruz, ya da PDF'i Word ile.. Bunun
sebebi dosya formatının değişik olması, veri içindeki bayt sıralaması
bir programda diğeri için gibi değil. Bu dosyalar çoğunlukla ikisel
(binary) olarak tanımlanıyor.

Fakat metin dosyalar her ne kadar basit editör, ya da komut satırında
mesela `cat` ile görülebiliyorlarsa da, aslında onların da bir iç
formatı var, ve bazı formatlar diğerleri ile uyumlu değil, özellikle
uluslararası karakterler gösterilmek isteniyorsa.

En basit farklılık mesela Windows (DOS) ve Ünix metin dosyaları
arasındaydı, Windows ile her satır hem yeni satır karakteri hem de
CRLF denen bir veri ile biter. Linux üzerinde iki format arasında
gidip gelmek için `ünix2dos`, `dos2ünix` kullandık uzun zaman, şimdi
`tofrodos` var.

Diğer farklılık kodlama (encoding) olabilir. Mesela yaygın olan UTF8,
İSO8859-9'dan farklıdır. Bu iki format arasında gidip gelmek için
`iconv` kullanılabilir. Örnek,

```
iconv file.tex -f ISO8859-9 -t UTF-8 -o out.tex
```

Editor

Editörümüzün de bir metni gösterirken hangi kodlama ile iş yaptığını
bilmesi gerekir. Emacs e bunu sonek bağlamında söylemek  için

```
(modify-coding-system-alist 'file "\\.tex\\'" 'utf-8)
```

diyebiliriz. Eğer `İSO8859-9` istersek

```
(modify-coding-system-alist 'file "\\.tex\\'" 'latin-5)
```

olurdu.

Eğer LaTeX kullanıyorsak, onun da kodlamayı bilmesi gerekir. Dosya başında, UTF8 için,


```
\usepackage[utf8]{inputenc}
```

gerekli.











