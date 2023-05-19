# Ubuntu 22

Daha önceki bazı versiyonların yazıları [şurada](ubuntu.html)

Versiyon 22 kurulumu için

http://releases.ubuntu.com/22.04/

Üstteki adresten 64-bit iso ya da iso için torrent indirilir, iso'yu
USB flash disk'e "yakmak" için [3]. Bios'a bilgisayar başlarken F2'yi
basılı tutarak girebiliriz, girdikten sonra başlangıç şeklini "Legacy
Mode" haline getirmek gerekli, ve USB diski yükleme sırasında en üste
getirmek lazım. F10 ile kaydedilir, tekrar başlatılır ve Ubuntu
kurulur.

Kurulum ardından hemen

```
sudo apt install emacs git build-essential python3 openssh-server python3-virtualenv
```

Alt+Space Problemi

Emacs üzerinde Alt-Space çok kullananlar için, bu kombinasyon Ubuntu
tarafından "kapılmış". Kombinasyonu Ubuntu seviyesinde iptal etmek
için `System Tools` | `System Settings` | `Keyboard ve Shortcuts` |
Windows bölümunde `Activate the window menü` için Alt+Space seçilmiş, bu
satıra gidip çift tıklama ve Silme (backspace) düğmesine basılır.

Diğer Tuşlar

Biz CAPS tuşunu CTRL, varsa Windows tuşunu Alt olarak kullanıyoruz.
Package Manager'da "gnome tweaks" kurulur. `Additional Layout Options`
içinde `Alt and Win behaviour` altında `Alt is mapped to Win and the
usual Alt` seçilir. `Caps Lock behavior` altında `Make CAPS an
additional Ctrl`.

Python ile çok iş yapılacak, hemen sistem ve neredeyse tüm programcılık
işlerinin kullanacağı kütüphaneler için bir sanal ortam yaratmak iyidir.
Benimki `$HOME/Documents` altında,

```
virtualenv -p /usr/bin/python3 env3
```

`source env3/bin/activate` ile ortama girdikten sonra

```
pip install ipython matplotlib 
```

Şimdi `.bashrc` içinde bazı iyi kısayol komutlar,

```
alias env3='source /home/burak/Documents/env3/bin/activate'
alias em='source /home/burak/Documents/env3/bin/activate; emacs & disown'
alias emnw='source /home/burak/Documents/env3/bin/activate; emacs -nw '
```

### Pymacs

Emacs eklentilerini Python ile yazmayı sağlayan paket Pymacs. Kodlar

```
git clone git@github.com:/pinard/Pymacs.git
```

ile alınır. Dizinde `make` ve `python setup.py build` ve `python
setup.py install` işletilir. Fakat Ubuntu 22 olağan kurulum Python
versiyon 3.10.6 ile yapıyor, Pymacs'in bu versiyon ile ufak bir
problemi var, ya geriye 3.6 versiyonuna gidilir, ya da hemen Pymacs
kodu üzerinde ufak bir degisiklik,

Satır 43

```python
def callable(value):
    return isinstance(value, collections.Callable)
```

alttakine değiştirilir [2],

```python
def callable(value):
    return isinstance(value, collections.abc.Callable)
```

Kaynaklar

[1] https://fostips.com/remap-keys-ubuntu-22-04/

[2] https://stackoverflow.com/questions/69515086/error-attributeerror-collections-has-no-attribute-callable-using-beautifu

[3] http://www.pendrivelinux.com/universal-usb-installer-easy-as-1-2-3/