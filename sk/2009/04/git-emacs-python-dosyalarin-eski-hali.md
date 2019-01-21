# Git, Emacs, Python: Dosyalarin Eski Hali


Git, Emacs, Python: Dosyalarin Eski Hali




Python ile Emacs eklentisi yazilmasini saglayan Pymacs uzerinde yazilan bir Git yardimci programi alttadir. Herhangi bir dosya uzerindeyken M-x githist-do-show-version yazip (menuye baglanabilir) sorulan soruya bir rakam ile cevap verilince o rakam kadar Git icindeki tarihte geriye gidilip, dosyanin o versiyondaki hali cikartilarak ayri bir Emacs buffer olarak gosterilmekte. Isletilen komut
git show master~[rakam]:noktagit/seviyesinden/baslayan/dizin/ismi/Dosya.java
Kurmak icin Python kodu surada, site-lisp altina kopyalanmali, ve .emacs degisikligi:
(pymacs-load "/usr/share/emacs/22.2/site-lisp/githist")

(defun githist-do-show-version(num)
(interactive "nKac commit oncesine gidelim: ")
(githist-show-version num)
)
Ingilizce blog'daki haber:




