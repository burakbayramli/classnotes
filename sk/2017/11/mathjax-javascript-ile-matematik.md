# MathJax: Web Sayfalarinda Javascript ile Matematik Formülleri

LaTeX ile yazılan matematik formüllerinin HTML içinde gösterilebilmesi
için MathJax Javascript kütüphanesi var. Gerekli JS dosyaları diğer
HTML, CSS dosyaları ile birlikte aynı dizinde olur, gerektikçe servis
makinasından alınır, ve formül grafiğinin üretilmesi için işlem zaten
Javascript ile kullanıcının tarayıcısında yapılır. Eğer MathJax
kodlarını Internet'ten almak yeterliyse, şu şekilde bir HTML yeterli,

```
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({
    tex2jax: {inlineMath: [["$","$"],["\\(","\\)"]]}
  });
</script>
<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML-full">
</script>
</head>
<body>
  ...
  
</body>
</html>
```

Eğer kurmak istiyorsak alttakiler. 

Kurmak icin GH'dan kod alinir, 

https://github.com/mathjax/MathJax

Depo icinde gereken dosya / dizinler

```
config/
extensions/
jax/
MathJax.js
```

Bu dosyalar sayfalarla beraber web servis makinasindan servis
edilecek. Simdi bu dosyalarla ayni seviyede olan bir HTML dosyasi
icinde


```
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<script type="text/x-mathjax-config">
   MathJax.Hub.Config({
    tex2jax: {inlineMath: [["$","$"],["\\(","\\)"]]}
  });
  </script>
  <script type="text/javascript" src="MathJax.js?config=TeX-AMS_HTML-full"></script>
</head>
<body>

<p>
Eğer $a \ne 0$ olursa  \(ax^2 + bx + c = 0\) denkleminin çözümü iki tanedir
ve bunlar $$x = {-b \pm \sqrt{b^2-4ac} \over 2a}.$$
</p>

</body>
</html>
```

Bu dosya tarayıcıda gösterilince

![](mathj01.png)

MathJax yaklaşımı güzel çünkü ek bir "üretme komutu"na gerek duymuyor
- kod HTML içinde, bildiğimiz LaTeX olarak kalıyor, servis tarafında
neredeyse hiç işlem yapılmıyor, sayfayı görmek isteyen kişinin
bilgisayarı gerekli işlemi yaparak formülü görüyor. 


