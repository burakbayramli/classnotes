# Komut Satırı Seçeneklerini İşlemek (Command Line Option Processing), Python

Python script'lerine verilecek seçenekleri nasıl isleriz? İlk akla
gelen sys üzerinden işlem yapmak,

```python
import sys

if __name__ == "__main__":
    print sys.argv[0]
    print sys.argv[1]
```

gibi.. Burada script ismi sys.argv[0] içinde, ilk argüman sys.argv[1],
vs. Eğer len(sys.argv) dersek kaç tane seçenek verildiğini raporlar. 

Fakat bu seçeneklerden hangileri zorunlu, hangileri değil, hiç seçenek
verilmeyince güzel bir hata mesajı verilse iyi olmaz mı, gibi
ihtiyaçlar için argparse paketi kullanılabilir. Bir test.py icinde,

```python
import argparse

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Bir program')
    parser.add_argument('arg1', type=str, help='arg1 tanimi.')
    parser.add_argument('arg2', type=str, help='arg2 tanimi.')
    parser.add_argument('--arg3', type=bool, nargs="?", help='arg3.')
    parser.add_argument('--arg4', type=str, nargs="?", help='arg4.')

    args = parser.parse_args()
    print args.arg1
    print args.arg2
    print args.arg3
    print args.arg4

```

Eğer hiç seçenek vermeden çağırırsak 

```python
usage: test.py [-h] [--arg3 [ARG3]] [--arg4 [ARG4]] arg1 arg2
```

```
test.py: error: too few arguments
```

mesaji gelecek. Istesek `-h` ile tanımları raporlatabilirdik.

Önünde `--` olan seçenekler isim verilmeden geçilebilir.

```
python test.py secenek1 secenek2
```

gibi. Önünde -- olan ve nargs="?" diyen seçenekler şart olmayan
seçeneklerdir.

```
python -u test.py secenek1 secenek2 --arg3 secenek3 --arg4 secenek4
```

Sonuç

```
secenek1

secenek2

True

secenek4
```

3. seçeneğin True gelmesi doğru, çünkü tip tanımı da yapılabiliyor, ve
o tipi bool olarak tanımlamıştık.
