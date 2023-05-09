# HTML Prezentasyonlari - reveal.js

Basit HTML içerikleriyle, matematik, envai türden medya içeren
prezenasyonlar yaratmak için `reveal.js`. 

Kodu Github'dan alırız,

[https://github.com/hakimel/reveal.js](https://github.com/hakimel/reveal.js)

En basit örnek için gereken dosyalar `css/reveal.css`,
`css/theme/white.css`, ve `js/reveal.js`. Basit prezentasyon,

```
<html>
  <head>
    <link rel="stylesheet" href="css/reveal.css"/>
    <link rel="stylesheet" href="css/theme/white.css"/>
  </head>
  <body>
    <div class="reveal">
      <div class="slides">
        <section>
          <h3>Section 1</h3>
          <ul>
            <li class="fragment" data-fragment-index="0">More</li>
            <li class="fragment" data-fragment-index="0">Stuff</li>
          </ul>
        </section>
        <section><h3>Slide 2</h3></section>
      </div>
    </div>
    <script src="js/reveal.js"></script>
    <script>
      Reveal.initialize();
    </script>        
  </body>
</html>
```

Bu kod iki sayfalık bir prezentasyon yaratır, büyük ekranda slaytlar
gösterilir, birincisinde alt içerik ana başlıktan sonra gösterilir,
kayış, geçiş gayet güzel. MathJax desteğini de HTML içine rahatça
ekleyebiliriz böylece matematik içerikli prezentasyonlar yaratmış oluruz. 

Fikri [şuradaki arkadaştan](https://mec560sbu.github.io/Prersentations/Pres_SystemDynamics.html#/1) aldık. 





