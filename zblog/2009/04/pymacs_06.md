# Pymacs


Pymacs




Lisp surekli kullandigim dillerden biri degil; sadece uzun aralarla Emacs editorumu kisisellestirmek icin kullaniyorum ve uzun sure gectikten bazi Lisp numaralarini tekrar ogrenmem gerekiyor. Bir araci birden yerde fazla kullanabilme meraklisi oldugumuz icin Lisp'ten mumkun oldugu kadar uzak durmaya karar verdik. Bu kararda onemli bir faktor, bir editor uzantisi icin okkali kodlama yapmamiz gerektigi idi... Peki bu is hangi dilde yapilacakti?

Bilimsel hesaplama icin zaten Python kullanmaya karar vermistik. Acaba Python'u Emacs'i uzatmak/kisisellestirmek icin kullanamaz miyiz?

Editor uzantisi kodlamanin agir siklet kismi icin de Perl kullanmayi dusunuyorduk, cunku bir suru dosya/dizin gezme/isleme gerekecekti, fakat bu kullanimda Perl yerine de Python kullanabilirdik. Bir tasla iki kus.

Biraz arama sonucunda Emacs'i Python ile uzatmami saglayacak bir araci buldum: Pymacs. Pymacs, tum Emacs lisp fonksiyonlarina bir Python script'i icinden ve tum Python modullerine de Emacs icinden erisebilmenize izin veriyor. Kurmak icin ya apt-get ile ya da kaynagi indirip python setup.py install ile kurabilirsiniz. Tarifleri takip edin, oldukca kolay.

Hemen bir Emacs uzantisi yazalim, mesela pymacstest.py adinda bir program yazalim; bu programi /usr/share/emacs/22.2/site-lisp/ altinda diger EL dosyalari ile birlikte tuttugunuzu varsayalim. O zaman .emacs icinden

(pymacs-load "/usr/share/emacs/22.2/site-lisp/pymacstest") 

cagrisi yuklemek icin yeterli. Bu kod icinde neler olsun?

from Pymacs import lispdef test():    lisp.message("i am here")

Gordugunuz uzere lisp objesi uzerinde Emacs metotlari var; lisp.message cagrisi elisp icindeki (message "..") cagrisini yapiyor. pymacstest.test cagrisini yapmak artik cocuk oyuncagi, Pymacs isimlendirme kurallarina gore, dosya ismi + "-" + metot ismi kullanarak bu cagri Lisp tarafindan yapilabiliyor. Metot ismi demek ki pymacstest-test olacak.

Hizli gelistirme icin bazi numaralar; Python kodu degistigi zaman modulu tekrar yuklettirmek icin *Pymacs* isimli buffer'i oldurmek, sonra modulu tekrar yuklemek. Bizim .emacs dosyamiz su anda soyle (baska modul ve metot isimleriyle):

(defun reload-pyjde()(interactive)(if (buffer-live-p (get-buffer "*Pymacs*" ))   (kill-buffer (get-buffer         "*Pymacs*")))(pymacs-load "/usr/share/emacs/22.2/site-lisp/pyjde"))(defun test-me()(interactive)(pyjde-test))(global-set-key [f5] 'test-me)(global-set-key [f11] 'reload-pyjde)(reload-pyjde) 

Boylece gelistirme sirasinda pyjde.py dosyasi degistikce F11 tusu ile kodu tekrar yukletip F5 ile cagriyi yapabiliyoruz. Metot test-me() daha sonra baska yerlere map edecek, bu tusa gerek kalmayacak fakat durumu anladiniz saniyorum.

Boylece alet cantamizdaki Lisp, Perl'den ayni anda kurtulduk, ayrica hesapsal isler icin Python kullanacagiz - iki tool gitti, yerine uc is yapan tek tool geldi.

Bazi komutlar:

Uzerinde oldugumuz nokta bir blok icinde, bu bloga tekabul eden tum metni istiyoruz,

b = lisp.search_forward("[BITIS]")e = lisp.search_backward("[BASLA]")content = lisp.buffer_substring(b, e)

Burada Emacs LISP fonksiyonlarindan ikisini kullanmis olduk.

Herhangi bir noktada *Messages* icinde ve Emacs alt boslugunda gozukebilecek bir metin basmak icin 

lisp.message("mesaj")

Icinde oldugumuz Emacs buffer'in ismi

lisp.buffer_name()

Icinde oldugumuz dosyanin ismi

lisp.buffer_file_name()

Diyelim ki oldugumuz noktaya bir suru islemden sonra donmek istiyoruz

remember_where = lisp.point()...
lisp.goto_char(remember_where)





