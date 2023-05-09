# Emacs Dış Programları Nasıl İşletir?

Emacs, değişken bir şekilde eklenti yapılabilmesi, kullanıcıya göre
değiştirilebilmesi bakımından çok esnek, ve güçlü bir metinyaz
programıdır. Emacs'in tamamı LISP dili ile kodlanmıştır, ve bu yüzden
ek uzatmaların kodlaması gene LISP dilinde yapılır.  Örnek olarak,
Emacs'i şöyle uzatmayı düşünelim.  Bir yazılım projesi dahilinde PVCS
adında bir Kaynak Kod İdare sistemi kullanıyoruz. Bu KKİ programı ile
dosya kitleme, dosya değiştirildikten sonra yapılan değişiklikleri
KKI'ya geri ekleme, kod değişikliklerini idare etmek gibi işlemler
gerçekleştirebiliyoruz.  Fakat PVCS, ayrı görsel bir program
kullanmayı gerektiriyor. Ne kadar zor bir iş! Tam en sevdiğimiz
metinyaz ile dosyayı açmış iken, dosya kitlemek için bu başka öteki
programa geçmemiz gerektiğini farkediyoruz, çünkü dosya salt okunur
halde!  Bu dosyayı düzenlerken, Emacs içinden tek bir tuşa basarak
kitlemeyi otomatik yapsak ne kadar harika olurdu!  Bu yazıda LISP
dilini kullanarak Emacs'i uzatacağız, ve istediğimiz bu özelliği
kodlayacağız.

Üzerinde Olduğumuz Dosyanın İsmine Erişmek Emacs'i başlattığınızda
yapılmasını istediğiniz kodları Unix'te .emacs, Windows'da _emacs
dosyayısına koymanız lazım. Emacs her başladığında bu dosya içindeki
LISP kodlarını işletir.  Özel konumuza dönelim: Üzerinde olduğumuz
dosyayı kitlemek için, bu dosyanın ismini bilmemiz lazım.
(buffer-file-name) .. işlevini kullanarak bu dosya ismini Emacs'den
alabilirsiniz. Bahsedilen işlevi test etmek için, herhangi bir dosya
içine (buffer-file-name) yazın, ve imleci işlevin en sonuna getirip
`\C-x\C-e` (Control X sonra Control e) tuşlarına basın. Bu tuşlar
eval-last-sexp işlemini çağırır, yani son-işlevi-çalıştır komutunu.
Sonucu Emacs'in alt-boşluk kısmında göreceksiniz.  Evet, artık içinde
olduğumuz dosyayı biliyoruz. Peki Emacs, işletim sistemine erişip
nasıl bir dış komutu işletebilir?  Bunun için

```
(shell-command "isletim sistem komutu")
```

..komutu gerekecek.

Unix örneği olarak, `ls -1` komutunu, `(shell-command "ls -1")` olarak
işletebilirsiniz. `\C-x\C-e` kullanarak bunu da kontrol edebilirsiniz.

Parçaları Biraraya Koyalım

```
(defun kki-dosya-kitle()
"kitleme islevi"
(interactive)
(setq komut "c:/Progra~1/PVCS/vm/win32/bin/get -l ")
(setq komut (concat komut (file-name-nondirectory
   (buffer-file-name))))(shell-command komut)
(vc-toggle-read-only))
(global-set-key "\C-c\C-o" kki-dosya-kitle)
```

Yukarıda yapılanları tarif edelim: Komut değişkeni ile, komut satırına
geçilecek komutu 'toparlıyoruz'. Ekleye ekleye tamamlıyoruz
yani. (buffer-file-name) bu bağlamda
kullanılıyor. (vc-toggle-read-only) ile yaptığımız, salt okunur
dosyayı, yazılabilir dosyaya çevirmek. Emacs ile bunu yapmak çok
basittir. Ve en sonda yapılan, yeni yazdığımız kki-dosya-kitle
işlevini, `\C-c\C-o` tuşlarına bağlamak! Bu da Emacs'in önemli
özelliklerinden biridir, istediğiniz işlevi istediğiniz klavye tuşuna
bağlıyabilirsiniz. Artık `\C-c\C-o` ne zaman bassanız, üzerinde
olduğunuz dosya KKİ sisteminden kitlenip, sizin değiştirmenize hazır
olacak.

KKİ'ya Dosya Geri Vermek

```
(defun kki-dosya-geri-ver()
"dosya geri vermek"
(interactive)
(setq comments (read-string "Yorum Ekleyin: " ))
(save-buffer)
(setq komut "c:/Progra~1/PVCS/vm/win32/bin/put -q -f ")
(setq komut (concat komut "-m")) ;; pvcs uzerinde yorumlar -m ile ekleniyor
(setq komut (concat komut " \""))
(setq komut (concat komut comments))
(setq komut (concat komut " \""))
(setq komut (concat komut (file-name-nondirectory
    (buffer-file-name))))
    (print komut)(shell-command komut)
    (vc-toggle-read-only))
(global-set-key "\C-c\C-i" 'kki-dosya-geri-ver)
```

Not Burada verilen örnekler, PVCS KKİ sistemine göre yazıldı. PVCS,
piyasadaki 'ortalama' KKİ sistemlerinden biridir. Burada önemli olan,
fikirleri öğrenmek, yani "Emacs ile dış programlar nasıl işletilir"
gibi teknikleri anlamaktır; PVCS için kullandığımiz get ve put
komutlarının karışıklık yaratmaması için bu notu düşmeyi uygun
buluyoruz.  Örnek config dosyalarını buradan indirebilirsiniz.





