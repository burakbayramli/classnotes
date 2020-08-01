# Network Ölçümü

Network'umuzu izlemek icin araclar

```
sudo apt install vnstat

vnstat --live
```

ile mevcut network donanim arayuzleri gosterilir,

```
vnstat --live -i [donanim]
```

ile o donanimi izleyebiliriz.

Veri akis hizi bit / saniye olarak verilir, gigabayt / gun olarak almak icin
1 GB / gun = 92592 bit / saniye oldugunu bilelim.

