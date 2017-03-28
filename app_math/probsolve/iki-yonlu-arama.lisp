(load "ortak.lisp")

(defun iki-yonlu-ara (di ds ileriden-cocuklar geriden-cocuklar)
  (let ( ( acik-i   (list  (list di nil nil) ) ) ;;dikkat ic liste bir
						 ;;dugum
	 ( acik-g   (list  (list ds nil nil) ) ) 
	 ;;tamamlanan listesine ihtiyac yok
	 ( nf      nil )
	 ( nb      nil )
	 ( kizlar-i nil )
	 ( kizlar-g nil )
	 ( ortak-dugum nil  ))

    (setq kaydir-sayisi 0)
    
    (loop
     (cond
      ((or (null acik-i) (null acik-g)) ;;arama uzayini bitirdik mi?
       (print "Sorry, the problem posed is insoluable")
       (return nil)))

     ;;Evet. Ilk dugumu cikart ve Acik listeyi guncellestir
     (setf nf (pop acik-i)) 

     ;;agaclar icin tamamlanan listesine ihtiyac yok
     (setf ortak-dugum (member nf acik-g :test #'durum-karsilastirici))
     (when ortak-dugum
       ;; bu dugumu, geri gelen tarafta bulduk mu?
       (print "Harika. Sonuc Bulundu:")
       (sonuc-izini-bul nf)
       (sonuc-izini-bul-geri (first ortak-dugum))
       (sonuc-raporu-ver acik-i acik-g nil nil)
       (return ortak-dugum))
     
     ;; burada yeni dugumler cikartiyoruz
     (setf kizlar-i (apply ileriden-cocuklar (list nf)))

     ;; hata bulmak icin rapor
     (kac-dugum-yarattik)
     
     ;; acik liste kuyruk olarak kullaniliyor, o yuzden algoritma
     ;; esasen kat-kat arama.
     (setf acik-i (append acik-i kizlar-i)) 

     ;;Tamam, ilk dugumu cikar, acik listeyi guncellestir
     (setf nb (pop acik-g)) 

     (setf ortak-dugum (member nb acik-i  :test #'durum-karsilastirici))
     (when ortak-dugum
       ;; durum, ileri giden kisimda bulundu mu?
       (print "Harika. Sonuc Bulundu:")
       (sonuc-izini-bul nb)
       (sonuc-izini-bul-geri (first ortak-dugum))
       (sonuc-raporu-ver acik-i acik-g nil nil)
       (return ortak-dugum))

     ;;burada yeni cocuklar yaratiyoruz
     (setf kizlar-g (apply geriden-cocuklar (list nb))) 

     ;; hata bulmak icin rapor
     (kac-dugum-yarattik)
     
     ;;acik kuyruk olarak kullaniliyor, o yuzden kat-kat arama yapmis
     ;;oluyoruz
     (setf acik-g (append acik-g kizlar-g)) 
     
     ) ;;closes loop
    ) ;;closes let
  ) ;; closes defun

;;
;; geri giden algorithmanin izini buluyor
;;
(defun sonuc-izini-bul-geri (dugum)
  (cond ((null dugum) (print "Geri giden sonucu izliyoruz") nil) 
	(t 
	 (if (equal 'north (Kaydir dugum)) (print 'asagi))
	 (if (equal 'east (Kaydir dugum)) (print 'sola))
	 (if (equal 'west (Kaydir dugum)) (print 'saga))
	 (if (equal 'south (Kaydir dugum)) (print 'yukari))
	 (sonuc-izini-bul-geri (Ust dugum)) 
	 )))


;;
;; iki dugumun 'icine bakarak' durumlarini karsilastiriyor
;;
(defun durum-karsilastirici (a b)
  (if (equal (durum a)(durum b)) b))

;;
;; iki yonlu arama icin bu islemi tekrar tanimlamak gerekti..
;;
(defun sonuc-raporu-ver (acik-i acik-g start end)
  (print "# of items in OPEN FORWARD")
  (print (length acik-i))
  (print "# of items in OPEN BACKWARD")
  (print (length acik-g))
  (print "# of items in TOTAL nodes")  
  (print kaydir-sayisi)  
  )

;;
;; testler
;;
(setq d0 '((1 2 3 4) (5 6 0 8) (9 10 7 11) (13 14 15 12) (1 2)))
(setq ds '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 0) (3 3)))
(test "bi-directional" 
      (not (eql (iki-yonlu-ara d0
			       ds
			       'cocuklari-getiren-islem
			       'cocuklari-getiren-islem) 'hata)) t )

(print "Oldu. Iki Yonlu Arama Testleri Gecti")
