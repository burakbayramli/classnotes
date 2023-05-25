# Emacs

Emacs hiç kuşkusuz, bol özellikli, ve esnek bir editör.

Ubuntu üzerinde kurmak için

```
sudo apt-get install emacs25
```

Başlatmak için ben bir alias kullanıyorum çünkü o script içinden bir
Python sanal ortam başlatıyorum, böylece Emacs icindeyken istediğim
Python paketlerine de erişebiliyoruz. Çünkü Python script'leri
çoğunlukla Emacs içinden işletirim, eğer Emacs süreci Python
paketlerinden habersiz ise, script işini yapamaz. Mesela `env3` adlı
bir ortam için

```
alias em='source /home/burak/Documents/env3/bin/activate; emacs & disown '
```

Emacs'ın tüm başlangıç ayarlarını bir el dosyasında
tutabiliriz. Olağan haliyle Emacs ev dizini `$HOME` altındaki `.emacs`
dosyasını arar. Fakat bu dosya ev dizininde silinir, kaybolur endisesi
ile biz orada

```
(load-file "/dizin/dizin/emacs-ayar.el")
```

gibi başka bir dosyayı yükleriz, yani ayarlar `emacs-ayar.el` içinde
oluyor mesela, ki bu dosya kod deposu içinde güvenli.

Emacs,  

* LISP'i andıran bir dil ile her şeyini değiştirmenize izin veriyor.  

* Gene LISP kullanarak özellik eklemenize izin veriyor (ne isterseniz).  

* Her programlama dili icin bir 'kip' (mode) desteği var. Mesela
  'sadece' Java yazarken "if kelimesinden sonra otomatik aşağı satıra
  git" gibi bir tanım Emacs için cok kolay. Bu gibi ayarlar, .emacs
  config dosyasından rahatça yapılıyor.

* Emacs ve dış dünya bağlantısı rahat. Unix komutlarını Emacs içinden
  işletmek mümkün. (Esc-x shell)

* Emacs içinden dış kaynak kodları derleme, ve sonuçları tarayıp
  hatalı olan satırı göstermek çok rahat.
  
Ekte göstereceğimiz kodlar, benim .emacs config dosyamdan alındı. İlk
önce açıklamayı, sonra kodun kendisini sunacağım.
  
Örnek .emacs
  
Emacs içinden derleme yapmak, istenilen komut buraya girilebilir,

```
(setq compile-command "make ")
```
  
Metin incelerken, Emacs göstergesinden, kaçıncı satırda olduğumuzu
görmek:

```
(setq column-number-mode t)
```
  
Sevdiğim renkler..  

```
(set-face-background 'modeline "darkred")
(set-face-foreground 'modeline "white")
(set-face-foreground 'region "lightgrey")
(set-face-background 'region "white")
(set-background-color  "navyblue")
(set-foreground-color "white")
(set-cursor-color "turquoise")
```

Sevdiğim boyutlar..  

```
(set-frame-width (selected-frame) 83)
(set-frame-height (selected-frame) 49)
```

Tuşların ne yaptığını tamamen değiştirebilirsiniz. Şahsen ben ok
tusları yerine (uzak oldukları için) Control tuşu ile alfabe
harflerine beraber basarak ileri-geri gitmeyi tercih ediyorum. 4 yön
için, sol = control-J, sağ = control-L, yukarı = control-P ve aşağı =
control-N kullandık.


```
(define-key global-map "\C-m" 'newline-and-indent)
(global-unset-key "\C-f")
(global-unset-key "\C-w")
(global-unset-key "\C-d")
(global-unset-key "\C-l")
(global-unset-key "\C-p")
(global-unset-key "\C-j")
(global-unset-key "\C-k")
(global-unset-key "\C-o")
(global-set-key "\C-o" 'other-window)
(global-set-key "\C-p" 'previous-line)
(global-set-key "\C-k" 'backward-delete-char-untabify)
(global-set-key "\C-n" 'next-line)
(global-set-key "\C-j" 'backward-char)
(global-set-key "\C-f" 'backward-kill-word) ;; geriye git, kelime sil
(global-set-key "\C-w" 'backward-word) ;; geri git, kelime atla
(global-set-key "\C-d" 'forward-word) ;; ileri kelime atla
(global-set-key "\C-l" 'forward-char) ;; ileri git
(global-set-key "\C-p" 'previous-line) ;; onceki satir
(global-set-key "\C-t" 'kill-line) ;; satir sil
(global-set-key "\C-x\c" 'compile) ;; derle
(global-set-key [f9] 'kill-buffer) ;; hafizadaki metni hafizadan cikar
(global-set-key "\C-c\i" 'indent-region) ;; dile gore, secilmis alani duzenle
(global-set-key "\C-c\k" 'kill-region) ;; secilmis alani sil
(global-set-key "\C-c\c" 'copy-region-as-kill) ;; secilmis alani kopyala
(global-set-key "\C-c\u" 'scroll-cursor-to-top)
(global-set-key "\C-x\q" 'query-replace) ;; ara ve degistir
(global-set-key "\C-x\g" 'goto-line) ;; satir no'ya atla
```

Emacs programlama dili kiplerinden bahsetmiştim. Bu kiplere geçmek
için "Esc-x java-mode (enter)" gibi komut işletirseniz Emacs kipe
geçecektir. Fakat eğer bu geçişin otomatik olmasını istiyorsanız,
aşağıdakini yapabilirsiniz.

```
(setq auto-mode-alist
 (append '(("\\.C$"   . c++-mode)
("\\.cc$"  . c++-mode)
("\\.cpp$" . c++-mode)
("\\.log$" . hscroll-mode)
("\\.cxx$" . c++-mode)
("\\.hxx$" . c++-mode)
("\\.h$"   . c++-mode)
("\\.hh$"  . c++-mode)
("\\.idl$" . c++-mode)
("\\.c$"   . c-mode)
           ("\\.pl$" . perl-mode)
           ("\\.java$" . java-mode)
           ("\\.jsp$" . java-mode)
           ("\\.inc$" . html-mode)
           ("\\.rb$" . ruby-mode)
           ("\\.?[Ff][Aa][Qq]$" . faq-mode)
           ("\\.txt$" . text-mode))
   auto-mode-alist))
```

Ayrıca, Emacs kullanırken Control tusunu çok kullanmak gerekiyor. Yeni
bilgisayarların çogunda Control tuşu oldukça alttadır. Sürekli Ctrl
tuşuna basacağım derken Emacs kullanıcıları el felci geçirmesin diye,
çözüm olarak CAPS tuşunu Control tuşuna çevirmek iyi olabilir. CAPS
düğmesi yukarıda olduğu için, sol serçe parmağı ile basmak çok
rahhattır. Bu degisimler isletim sistemi seviyesinde yapilir. Ubuntu
yazilarinda isliyoruz. Windows'da yapmak için, [su dosyayi](capsctrl.reg)
indirip, üzerine tıklayın. Sorulan soruya evet
diye cevap verin. Bilgisayarı kapatıp açtıktan sonra, CAPS düğmeniz
Control gibi işliyor olacak.

Referans

[Pymacs](../../2009/04/pymacs.html)

[Ayarlarım (My Config File)](https://github.com/burakbayramli/kod/tree/master/site-lisp)

[Unicode, Ascii, Deascii, Python, Emacs](../../2018/07/unicode-ascii-deascii-python-emacs.html)

