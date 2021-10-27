# SU2

ŞU2 aerodinamik simülasyonları için açık kaynak bir programdır.

[[Kaynak]](https://github.com/su2code/SU2/releases/tag/v7.0.2)

Kaynak kodu indirelim, açalım. İşler program `/usr/local/bin` altına
gidece şekilde kurmak için, dizine girip

```
./meson.py build --prefix=/usr/local

./ninja -C build install
```

Meson ve ninja yeni bir derleme sistemi (daha önce `make` ile yapılan işleri
yapıyor), hızlı işliyor. Derleme sonunda `.başhrç` içine koymanız tavsiye
edilen bazı ayarlar var,

```
export SU2_RUN=/usr/local/bin
export SU2_HOME=/home/user1/vs/vs/SU2-7.0.2
export PATH=$PATH:$SU2_RUN
export PYTHONPATH=$PYTHONPATH:$SU2_RUN
```

gibi olabilir. Bunları `.bachrç` içine koyalım, komut satırı kapatıp
tekrar açalım.

Örnek olarak alttaki ufak dersi takip edebiliriz. Derste gösterilen
ayar dosyası (configüration file) ve hesapsal izgarayı tanımlayan
dosya (mesh file) indirilir, 

https://su2code.github.io/tutorials/Inc_Turbulent_NACA0012/

Artik

```
SU2_CFD turb_naca0012.cfg
```

ile hesabi isletebiliriz. Dongu sayisi biraz fazla tanimlanmis olabilir,
onu mesela `ITER= 100` daha az hale getirebiliriz.

İşlem bitince `surface_flow.vtk`, `flow.vtk`, dosyaları üretilmiş olacak.
Bu dosyalar sonuç verilerini içeriyor, onlara görsel şekilde bakmak mümkün,
`paraview` programı bunun için.

```
apt-get update

apt-get install paraview

sudo apt install libcanberra-gtk-module libcanberra-gtk3-module
```

Simdi

`paraview` ile isletiriz, once `surface_flow.vtk` acariz, `Apply` dugmesine tiklariz.
Sonra `flow.vtk` acariz, tekrar `Apply`. Bu alttaki goruntuyu cikartmali,

![](su2_01.png)







