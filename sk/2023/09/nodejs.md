# NodeJs

Javascript kodlarini servis tarafinda isletebilmek icin NodeJs. Gorsel
sayfa kodlarinin gorsel olmayan kismi var ise, bu kodlari ayri bir
module ayirip onlari Node ile test etmek faydali olabilir, web servisi
baslat, sayfa yukle, kod degistir, tekrar yukle dinamigine girmeye
gerek kalmaz.

Ubuntu uzerinde

```
sudo apt install nodejs
```

Bir .js dosyasi yaratin [1], mesela tst.js, ve icine

```
const add = (a,b) => {
    return a + b
}

console.log(add(4,6))
```

Simdi `node tst.js` komutunu isletin, sonuc `10` olarak ekrana basilacaktir.









[devam edecek]

Kaynaklar

[1] https://www.boardinfinity.com/blog/how-to-run-a-js-file-in-terminal

