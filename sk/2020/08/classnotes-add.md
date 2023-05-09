# Ek Bilgiler

Emacs Bağlantısı

Github projelerimizden biriyle Emacs'te LaTeX doküman *içinde* iken
Python kodlarını [emacs-ipython](https://github.com/burakbayramli/emacs-ipython) adlı
bir teknoloji üzerinden direk belge içinde işletmek mümkün (arka
planda ipython'a bağlanıyor, yani aynı temel yapı kullanılıyor). Bu
durumda, emacs-ipython gereken tüm ipython ayarlarını kendisi yapıyor.

R

Bazen Python içinden R kütüphanelerini çağırmak gerekebiliyor. R ayrı
bir dildir, ama Python içinden onun kütüphanelerini çağırabiliyoruz.

```
sudo apt-get install r-base-dev r-base python-rpy2
```

R kütüphanelerini R içinden kurmak lazım. Komut satırında R yazın, ve

> install.packages("[kutuphane ismi"])

Bir servis / makina listesi gösteren menü çıkacak, bu menüden bir
ülkeyi seçin, ve paket kurulacaktır. Bizim notlar için gereken paketler,

```
lme4
tseries
urca
```

Latex Format

The format of these documents, fonts, the pseudocode look-and-feel was
taken from Andrew Cotter's thesis called *Stochastic Optimization for
Machine Learning*.


