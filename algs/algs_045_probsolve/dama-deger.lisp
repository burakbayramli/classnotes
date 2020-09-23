(load "dama-alg.lisp")

;; Bu islev, atak oyun halinde mi, yoksa defans oyun halinde mi
;; oldugumuzu hesaplar. Eger defans agirlikli oynuyorsak, ki program
;; bunu oyunun basinda yapmak uzere yazildi, her tahtanin degeri
;; hesaplanirken agirliklar defansif olculere gore secilir.  Atak
;; oyununda bu agirliklar degisiyor.  Defans/Atak karari su ana kadar
;; kac hamle yapildigina gore veriliyor; Belli bir tetik degerinden
;; sonra, (7 hamle) bilgisayar atak oynamaya basliyor.
(defun atak-oyun? ()
  (cond ((> bilgisayar-hamle-sayisi 7) t)
	(t nil)))

;; Alttaki degerlendirme islevini goruyoruz. Bu islev oyunun
;; akillica secim yapmasina yardim edecek.
(defun tahta-degeri(tahta)   
  (let (
	(y-sayac 0) (x-sayac 0)
	;; sayac degiskenler
	(benim-tas-sayim (caadr tahta))(rakibin-tas-sayisi (cadadr tahta))
	(benim-kral-sayim 0)(rakibin-kral-sayisi 0)
	(mevcut-hamlelerim 0)(rakibin-mevcut-hamleleri 0)
	;; agirliklar
	(kralin-agirligi nil)(mevcut-hamle-agirligi nil)
	(benim-kalan-tas-agirligim nil)(rakibin-kalan-tas-agirligi nil)
	;; sonuc
	(degerlendirme-toplami 0)
	)

    ;; Atak/defans halimize gore agirlik degerlerini
    ;; degistir
    (cond ((equal (atak-oyun?) nil)
	   (progn
	     (setq kralin-agirligi 50)
	     (setq mevcut-hamle-agirligi 60)
	     (setq benim-kalan-tas-agirligim 70)
	     (setq rakibin-kalan-tas-agirligi 30)
	     ))
	  (t
	   (progn
	     (setq kralin-agirligi 100)
	     (setq mevcut-hamle-agirligi 60)
	     (setq benim-kalan-tas-agirligim 20)
	     (setq rakibin-kalan-tas-agirligi 70)
	     )
	   ))

    ;; butun tahtayi tararken..
    (dotimes (x-sayac 8)
      (dotimes (y-sayac 8) 
	(setq konum-degeri (tas-degeri-ver (list x-sayac y-sayac) tahta))
	(if (not (equal konum-degeri nil))
	    (progn
	      ;; tas degeri ve kral gosterge degerine eris
	      (setq renk (car konum-degeri))
	      (setq kral-mi (cadr konum-degeri))

	      ;; saymaya basla
	      	      
	      ;; krallari say
	      (if (and (equal *enust-renk* renk)
		       (equal kral-mi t))
		  (setq benim-kral-sayim (+ 1 benim-kral-sayim)))
		
	      (if (and (equal *enalt-renk* renk)
		       (equal kral-mi t))
		       (setq rakibin-kral-sayisi (+ rakibin-kral-sayisi 1)))

	      ;; mevcut hareketleri say (hareket sahamiz ne kadar genis?)
	      (setq mevcut-hamlelerim
		    (length (hareket-listesi-hesapla *enust-renk* tahta)))
	      (setq rakibin-mevcut-hamleleri
		    (length (hareket-listesi-hesapla *enalt-renk* tahta)))
	      ))
	))

    ;; hepsini topla
    (setf degerlendirme-toplami
	  (+ degerlendirme-toplami
	     (* benim-tas-sayim benim-kalan-tas-agirligim)))
    (setq degerlendirme-toplami
	  (+ degerlendirme-toplami
	     (* rakibin-tas-sayisi rakibin-kalan-tas-agirligi)))
    (setq degerlendirme-toplami
	  (+ degerlendirme-toplami
	     (* benim-kral-sayim kralin-agirligi)))
    (setq degerlendirme-toplami
	  (+ degerlendirme-toplami
	     (* mevcut-hamlelerim mevcut-hamle-agirligi)))

    ;; sonuc degeri geri bildir
    degerlendirme-toplami)
  )

;;
;; testler
(setq bilgisayar-hamle-sayisi 0)
(test "atak mi defans mi? 1" (atak-oyun?) nil)

(setq bilgisayar-hamle-sayisi 10)
(test "atak mi defans mi? 2" (atak-oyun?) t)

(oyuna-basla *beyaz*)
(test "atak mi defans mi? 3" (atak-oyun?) nil)

(oyuna-basla *beyaz*)
(test "tahta degerlendir" (tahta-degeri *tahta*) 1620)

(print "Tamam. Degerlendirme Birim Testleri Gecti")
