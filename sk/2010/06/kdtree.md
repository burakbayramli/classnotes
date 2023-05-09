# kdtree

KDTree arama amaçlı olarak herhangi boyutlu bir geometrik alanı (ve o
alandaki noktaları) bölümlere ayırabilen bir algoritmadir. Çok boyutlu
noktaları bir alana dağıtıp / koyup, sonra elimizdeki başka bir
noktanın "o noktaların hangisine daha yakın olduğunu" sorgulamamızı
sağlar.

KDTree'leri bir tür "çok boyutlu noktalar için kullanılan bir veri
tabanı" olarak kabul edebiliriz. Bu taban içinde noktalar öyle
dağıtılıyor ki, "yakın nokta" arama işlevi daha kolay hale
geliyor.Python olarak iyi işleyen bir kod python-kdtree
projesinde. Kullanım örneği altta, örnekte kdtree'ye noktaları verip,
sonra bu noktaların içinden en yakın 2 tanesini bulmasını söylüyoruz.

```
data = [(2,3), (5,4), (9,6), (4,7), (8,1), (7,2)]
point = (8,5)
tree = KDTree.construct_from_data(data)
nearest = tree.query(point, t=2)
print nearest
```





