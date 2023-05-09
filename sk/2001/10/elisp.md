# Elisp

Tüm buffer'ı bir değişken içine almak için

```
(setq cmd (buffer-substring 1 (buffer-size)))
```

Bir buffer'a olan referansı o buffer'ın ismini kullanarak alabilmek
için;

```
(get-buffer "*Isim Filan Falan*")
```

Bu buffer'ı öldürmek için

```
(kill-buffer (get-buffer "*Isim Filan Falan*"))
```

Buffer'ın canlı olup olmadığını kontrol için (ve eğer canlı değilse
birşeyler yapmak için)

```
(if (not (buffer-live-p (get-buffer "Isim Filan Falan"))) (progn;; bir
seyler yap))
```

Buffer içinde olan dosyanın soneksiz hâlini almak için (meselâ
c:/temp/filan/TT.java üzerinden TT kelimesini çıkartabilmek için)

```
(setq soneksizDosyaIsmi (file-name-sans-extension
(file-name-nondirectory buffer-file-name)))
```

Komut satırına elisp içinden bir program işlettirebilmek için

```
(shell-command "perl c:/dizin/ismi/script.pl")
```





