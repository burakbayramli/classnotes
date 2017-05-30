(load "dama-temel.lisp")

;;
;; Cok basit bir degerlendirme fonksiyonu.  Degerlendirme islemleri
;; tahtaya bakarak bu tahtanin bilgisayar icin ne kadar iyi durdugunu
;; (oldugunu) bir rakam ile rapor ederler. *enust-renk* bilgisayarin
;; hangi oyuncuyu oynadigini gosterir. Burada kullanilan degerlendirme
;; cok basit, "daha zoru" degerlendirme icin ozel olan dosyada olacak.
;; Bu islevin raporladigi rakam, zaten tahtanin en sonunda tutulmakta.
(defun tahta-degeri(tahta)
  (cond ((equal *enust-renk* *beyaz*)
	 (caadr tahta))
	(t (cadadr tahta))
  ))

;;
;; disardan cagirilan islev
(defun altust-arama(dugum)
  (setf benim-eniyi -999999)
  (setf onun-eniyi +999999)
  (eniyi-hareket (copy-tree dugum) 10 benim-eniyi onun-eniyi)
  )

;; Simdi, altust (minimax) algoritmasinin alfa-beta seklini gosteriyoruz. 
(defun eniyi-hareket(konumn derinlik benim-eniyi onun-eniyi)

  ;; Dikkat ederseniz normal altust algoritmasina iki yeni
  ;; bildirgec ekledik. Baslarken Benim-Eniyi -sonsuz'a (eksi sonsuz)
  ;; esitlenmeli. Onun-eniyi ise +sonsuz'a esitlenmeli.
  (let ((hareket-listesi nil)(enyuksek-deger nil)
	(eniyim nil)(dene nil)(denenen-deger nil))

    ;; Bu kontrol, ozyineli cagirimi karar agacin sonuna geldigimizde
    ;; bitirebilmek icindir. Iki elemanli bir liste geriye
    ;; getiriyoruz. 
    (if (eql derinlik 0)
	(return-from eniyi-hareket (list (tahta-degeri konumn) nil)))

    ;; Oyun kurallarina gore uygun olan, tahta'nin o anki konumuna
    ;; gore olan butun muhtemel hareketleri buluyoruz. 
    (setq hareket-listesi (hareket-listesi-hesapla *enust-renk* konumn))
    
    (setq enyuksek-deger benim-eniyi)
    (setq eniyim nil)

    ;; Artik hareket-listesi'ni taramaya haziriz. En iyi
    ;; hareketi bu listeden sececegiz. Enyuksek-deger ve eniyim
    ;; degerlerini nasil yukledigimize dikkat edin.

    (do () ((equal hareket-listesi nil))
      
      ;; Burasi ana ozyineli cagirimin yapildigi yer. Bu cagiri ile
      ;; siradaki hareketin arama alanini 'aciyoruz'. Iki yeni
      ;; bildirgeci gectigimize dikkat edin. 
      (setf dene (eniyi-hareket (tas-oynat (first hareket-listesi) konumn)
			  (- derinlik 1)
			  (* onun-eniyi -1)
			  (* enyuksek-deger -1)))
      
      ;; eniyi-hareket iki deger geri getirir, unutmayin..
      (setq denenen-deger (first dene))

      ;; Simdi, elimizdeki hareketin otekilere daha iyi olup olmadigina
      ;; karar verelim. Elimizdeki hareketi 'su ana kadar en iyi' olarak
      ;; secip secmeyecegimiz buna bagli.

      (if (> denenen-deger enyuksek-deger)
	  (progn
	    (setq enyuksek-deger denenen-deger)
	    (setq eniyim (first hareket-listesi))
	    ))      

      ;; Iste 'kesit' islemi alfa-beta algoritmasi icin burada
      ;; yapilir. Eger iyi bir hareket bulamadiysak, arayisi burada
      ;; durduruyoruz. Return fonksiyonu, donguyu yarida kesiyor
      ;; ve bir onceki ozyine seviyesine donuyor. 

      (if (> enyuksek-deger onun-eniyi)
	  (return-from eniyi-hareket (list enyuksek-deger eniyim)))

      ;; Listede taramaya devam ediyoruz, bakalim elimizdekinden daha
      ;; iyi bir hareket bulabilecekmiyiz
      (setq hareket-listesi (cdr hareket-listesi))
      
      ) 

    ;; Butun hareket-liste'sini aradiktan sonra, en iyi
    ;; hareketi bulmus bulunuyoruz. Geriye cevap olarak
    ;; bu hareketi bildirecegiz. 
    (return-from eniyi-hareket (list enyuksek-deger eniyim))
    
    ) ;; let sonu
  
  )  ;; defun sonu


;;
;; testler

(oyuna-basla *beyaz*)
(test "degerlendir testi" (tahta-degeri *tahta*) 12)

(oyuna-basla *beyaz*)
(test "altust basit test" (altust-arama *tahta*) '(12 ((1 5) (0 4))))

(setq *enust-renk* "b")
(setf hic-siyah-kalmadi
  '(((nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil) )
     (("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil)
     (nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil) )
     (nil nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil nil)
     (nil nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil nil )
     (nil nil nil nil nil nil nil nil)) (12 0)))
(test "basit degerlendir 1" (tahta-degeri hic-siyah-kalmadi) 12)

(setq *enust-renk* "s")
(setf hic-siyah-kalmadi
  '(((nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil) )
     (("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil)
     (nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil) )
     (nil nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil nil)
     (nil nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil nil )
     (nil nil nil nil nil nil nil nil)) (12 0)))
(test "basit degerlendir 2" (tahta-degeri hic-siyah-kalmadi) 0)

(oyuna-basla *beyaz*)
(setq tahta-kopyasi (copy-tree *tahta*))
(altust-arama *tahta*)
(test "tahta degismemesi gerekir testi" (equal tahta-kopyasi *tahta*) t)

(print "Tamam. Algoritma Birim Testleri Gecti")

