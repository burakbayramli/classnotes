# Ubuntu ve Emacs


Ubuntu ve Emacs




Ubuntu uzerinde Emacs kullanmak icin once en son paketi kurdurmak gerekiyor;

sudo apt-get install emacs22

Sonra bizim Java JDE ortaminda calismasi icin hazir ettigimiz ve pek cok diger ek araci iceren paketi indirip /usr/share/emacs/22.2/site-lisp/ altinda aciyoruz. Artik tek bir .emacs satiri ile bu dizin altindaki emacs-ubuntu.el dosyasini yuklettirebiliriz. Bu satir soyle olacak;

(load-file "/usr/share/emacs/22.2/site-lisp/emacs-ubuntu.el")




