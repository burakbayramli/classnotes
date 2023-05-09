# Network Ölçümü, Izlemek

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

Hangi IP'ye sahip oldugumuzu anlamak icin `ifconfig -a`.

Eger DHCP ile bir network'e dahil olduysak bize dinamik olarak bir IP
adresi verilecektir. Bazen admin statik olarak bir IP vermeyi de
secebilir. 


