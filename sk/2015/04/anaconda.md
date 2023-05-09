# Python 3, Ubuntu, Anaconda

Basit Python 3

```
sudo pip install python3-pip python3-numpy python3-scipy
```

gibi komutlarla pek cok Python 3 icin olan paket kurulabilir. Komut
satirinda "python script.py" yerine "python3 script.py" kullanilinca
sadece Python 3 icin kurulan paketler isleme konur. Altta Anaconda
usulu Python 3 kullanmayi da tarif ediyoruz, ama bu yontem herhalde en
kolay olani. Diger paketler icin (her paket icin Ubuntu apt-get
pyton3-vsvs hazir edilmemis olabilir)

```
sudo pip3 install [paket ismi]
```

isletilir.

Anaconda

Continum sirketinin one surdugu yeni python paket sistemi bu. Mevcut
Python paket sisteminin ciddi kullanicilari bu sistemlerin
bagimliliklari takip etmekte basarili olamadigindan yakinip dururlardi
(cok uc nokta ihtiyaclardan bahsediyoruz tabii, bizim simdiye kadar
sikayetimiz olmadi); Anaconda bu problemlere cevap olarak
kurgulanmis. Zaten Continuum Enthought sirketinden cikti, Enthought
sirketi de sayisal Python baglaminda liderlerden biriydi (Numpy'i
yazan kisi Travis Oliphant Enthought ve simdi Contiuum kurucusu
mesela).

Anaconda su anda Windows uzerinde dogru durust veri analiz Python
paketlerini kurabilmek icin neredeyse tek cozum.

Anaconda'nin degisik bazi ozellikleri var, mesela bir python isler
programi kendi `$HOME` dizininiz altinda kurulacak
(`$HOME/anaconda/bin/python`) ve artik olagan Python yorumcunuz bu
olacak (cunku `$PATH`'inizde bu dizin en one eklenecek) ve tum Python
paketleri de sizin kendi ev dizininize gidecek,
`$HOME/anaconda/lib/python2.7` altinda..

Bu sebeple artik Python paket kuruluslari icin sudo kullanimina gerek
kalmiyor, kisisel kullanim icin conda install yeterli.

Ayrica pip ile paket kurmak isterseniz, Conda'nin pip'i ile bu isi
yapmaniz lazim, yani `$HOME/anaconda/bin/pip` ile. Ayni sekilde, sudo
pip degil pip.

Ubuntu

Kurmak icin

http://continuum.io/downloads

linux 64 bit installer diyen sh dosyasini indir

```
bash Anaconda-2.2.0-Linux-x86.sh
```

Kurulus sirasinda kurulum dizini sorulacak, islem bittiginde o dizinin
altindaki bin dizinini PATH'e eklemek lazim, mesela .bashrc icinde

```
export PATH="[DIZIN]/anaconda3/bin":$PATH
```

Komut satirini kapatip yenisini acariz. Bir de bu bin dizinine girip
chmod u+x conda ile programi isletilir hale getirmek gerekebilir.

Artik Python 3 icin

```
conda create -n py3k python=3
```

Artik `source activate py3k` ile Python 3 ortamina girilebilir ve python
script.py ile Python 3 script'leri isletilebilir. Bir diger secenek,
 ki bunun icin source yapmaya gerek yok,

[HOME]/anaconda/envs/py3k/bin/python

ile direk Python 3 yorumlayicisini cagirmak.

PYMACS ICIN ONEMLI NOT

Eger Emacs icinden Pymacs kullaniyorsaniz dikkat,  Pymacs
/usr/bin/python kullaniyor, bunu pymacs.el icinde degistirebilirsiniz,
623. satirda

```
pymacs-python-command
```

yerine

```
[HOME DIZIN]/anaconda/bin/python
```

kullanin ve python setup.py build; python setup.py install. 

Ama en iyisi belki de Emacs'ı komut satırından, bir conda çevresine
girdikten sonra isletmek.
