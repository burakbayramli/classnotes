# Python Liste Kavrama (List Comprehension)

Python'un en geç anlaşılan, öğrenilen özelliklerinden biri belki de
budur. Kitap yazarlarının bile hala eski usul liste oluşturmayı
kullanması bunun işareti. Eski usul nasıldır? Mesela 1 ila 10
arasındaki sayıların karesini alacağım, ve bununla yeni bir liste
oluşturacağım. Ana liste,

```python
data = list(range(1,11))
print (data)
```

```text
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

```python
kareler = []
for x in data:
   kareler.append(x**2)
print (kareler)
```

```text
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

Bir boş liste tanımlandı, ona ekler yapıldı, bir `for` döngüsü var,
vs. Fakat liste oluşturması liste kavrama ile tek bir satırda
yapılabilirdi,

```python
kareler = [x**2 for x in data]
print (kareler)
```

```text
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

Yani liste içeriğinin nasıl oluşturulacağını bir nevi liste tanımının
parçası haline getirmiş oluyoruz. 

Kavrama operasyonları çok daha kapsamlı olabiliyor, `ıf`, `else`,
kullanmak mümkün mesela, hatta içiçe (neşted) döngüler bile
kullanılabiliyor.


