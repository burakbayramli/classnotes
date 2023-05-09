# Julia

Bilimsel hesaplama alanında yeni bir dil: Julia. C, Fortran gibi
diller statik tipleme kullanır, kodlaması zor ve hızlıdır, Python, R,
Matlab gibi diller kullanması kolay, dinamik ama yavaştır. Julia her
iki tarafın güçlü taraflarını birleştirmeye uğraşıyor. Şahsen
Python'da gördüğümüz bazı eksikleri cevaplandırması açısından bizim
ilgimizi çekti. Julia LLVM bazlı bir JİT kullanır, LLVM'i bir süredir
takip ediyoruz, sağlam bir teknoloji (ve Java VM bazlı çözümlere
alternatif sağlaması açısından faydalı oldu, Scala gibi).

Julia yazılan herhangi türden bir kodu "C performansına yakın" hızda
işletilmesini amaçlıyor. Python'da aynı performansı elde etmek için
çoğunlukla Numba ya da C çağrısı kullanmak lazım, bu çetrefil kodlama
demek, Julia bu işi paketten çıktığı haliyle halleder. Ayrıca
paralelizasyon, hem aynı makinadaki, hem diğer makinalardaki birden
fazla işlemciyi (ssh üzerinden) başağrısı yaratmadan hesap sürecinin
parçası haline getirebiliyor. Python'da programcılar rahatsız eden bir
konuyu adreslemişler yani, Python derleyicisinde GİL adlı bir kilit
vardır ve bu sebeple gerçek thread kullanımı mümkün olmaz,
multiprocessing gibi paketlerle arka planda süreç yaratan çözümler
kullanılabilir, fakat bu çözümler dilin doğal bir uzantısı gibi
durmazlar. Julia bu işi temelden çözüyor.

Bir başka iyi haber: Jupyter notebook ortamında Julia çekirdeği de mevcut.

Ubuntu'da kurmanin en kolay yolu

```
sudo snap install julia --classic
```

Ek Julia paketleri, Python `pip` gibi, kurmak icin `julia` komut
satirina girilir, ve orada

```
julia> using Pkg

julia> Pkg.add("[paket ismi[")
```

işletilir.






