# Emacs Uzerinden Twitter


Emacs Uzerinden Twitter



Emacs icinde Twitter kullanabilmek icin, once TwitteringMode kodlari indirilir. http://github.com/hayamiz/twittering-mode/tree/master  Acildiktan sonra kodlarin [DIZIN] icinde oldugunu farzedelim, .emacs icinde (add-to-list 'load-path "[DIZIN]")(require 'twittering-mode)(setq twittering-use-master-password t) Emacs icinde M-x twit komutunu kullandiktan sonra Twitter mod'u acilacak. Ilk kullanimda eger Web uzerinden Twitter kullaniyorsaniz, ve sifreniz hep hatirlaniyorsa, OAuth uzerinden Emacs'ten erisim icin izin istenecek. Bu y ve n sorusuna y ile cevap verin, bundan sonra tarayici bir sayfayi acip o sayfada bir PIN kodu verecek. Bu kod Emacs'e girilirse,  Emacs bir sifre soracak. Bu sifre artik Emacs uzerinden Twitter kullanma sifresidir. Her M-x twit sonrasi bu sifre sorulur. Programa girince 'i' ile kisi ikonlari gosterilebilir. Yeni tweet icin 'u' kullanilir, mesaj girildikten sonra eklemek icin  C-c C-c. Kaynak 




