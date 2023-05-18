# Ubuntu 22

```
sudo apt emacs git build-essential python3 openssh-server python3-virtualenv
```

Alt+Space

Emacs üzerinde Alt-Space çok kullanıyoruz, fakat bu kombinasyon Ubuntu
tarafından "kapılmış". Bu kombinasyonu Ubuntu seviyesinde iptal etmek
için System Tools | System Settings | Keyboard ve Shortçüts | Windows
bölümunde "Activate the window menü" için Alt+Space seçilmiş, bu
satıra gidip çift tıklama yapın ve Silme (backspace) düğmesine basın.

Diger Tuslarvirtualenv -p /usr/bin/python3 env3

Package Manager'da "gnome tweaks" kurulur. `Additional Layout Options`
icinde `Alt and Win behaviour` altinda `Alt is mapped to Win and the
usual Alt` secilir.  `Caps Lock behavior` altinda `Make CAPS an
additional Ctrl`.

```
virtualenv -p /usr/bin/python3 env3
```

`.bashrc` icinde

alias env3='source /home/burak/Documents/env3/bin/activate'
alias em='source /home/burak/Documents/env3/bin/activate; emacs & disown'
alias emnw='source /home/burak/Documents/env3/bin/activate; emacs -nw '

### Pymacs

Ubuntu 22 olagan kurulum Python versiyon 3.10.6 

git clone git@github.com:/pinard/Pymacs.git

Satir 43

```python
def callable(value):
    return isinstance(value, collections.Callable)
```

```python
def callable(value):
    return isinstance(value, collections.abc.Callable)
```



Kaynaklar

https://fostips.com/remap-keys-ubuntu-22-04/

https://stackoverflow.com/questions/69515086/error-attributeerror-collections-has-no-attribute-callable-using-beautifu

