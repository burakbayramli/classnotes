(load "ortak.lisp")

(Defun Durum (dugum) (first dugum))
(Defun Kaydir (dugum) (second dugum))
(Defun G-guncel (dugum) (third dugum))
;; tguncel 'tahmin guncel' den
;; geliyor.
(Defun t-guncel (dugum) (fourth dugum))	
(Defun Ust (dugum) (fifth dugum))

(defun FIYAT (ust cocuk) 1) 

;;
;; sinifta tanimlanan tahmin islevi (function). baslangic 
;; tahtasindaki her tas icin, ayni tasin sonuc tahtasindaki 
;; pozisyonuna olan uzakliklari toplayan bir islev.
;;
(defun tguncel-hesapla (su-anki-durum sonuc-durumu)
  (let ((i 0)(j 0)(toplam 0)(su-anki nil)(uzaklik 0))
    (loop
     (setq i 0)
     (loop
      (setf su-anki (nth i (nth j su-anki-durum)))
      
      (cond ((member su-anki (first sonuc-durumu))
	     (setf 
	      uzaklik
	      (+ (abs (- 0 j))
		 (abs (- i (position su-anki 
				     (first sonuc-durumu)))))))
	
	    ((member su-anki (second sonuc-durumu))
	     (setf 
	      uzaklik
	      (+ (abs (- 1 j))
		 (abs (- i (position su-anki 
				     (second sonuc-durumu)))))))

	    ((member su-anki (third sonuc-durumu))
	     (setf 
	      uzaklik
	      (+ (abs (- 2 j))
		 (abs (- i (position su-anki 
				     (third sonuc-durumu)))))))
	     
	    ((member su-anki (fourth sonuc-durumu))
	     (setf 
	      uzaklik
	      (+ (abs (- 3 j))
		 (abs (- i (position su-anki 
				     (fourth sonuc-durumu)))))))
	    )
     
      (setf toplam (+ toplam uzaklik))
      
      (setq i (incf i))	;; increment
      (if (eql i 4) (return)))
     (setq j (incf j))
     (if (eql j 4) (return))
     ) toplam)
  )

;; bu algoritma, fiyati-sabit-arama islevi temel alinarak yazilmistir.
;; yani, algoritma asagi yukari aynidir. Tek fark, FIYAT islevinin
;; hesaplanmasi.
(defun a-yildiz-arama (s0 sg sons)
  (let ( ( acik   (list  (list s0 nil 0 0 nil) ) ) 
	 ;;g-guncel set to 0
	 ( tamamlanan nil )
	 ( n      nil )
	 ( kizlar nil )) 
    (setq kaydir-count 0)
    (loop
     (if (null acik) (return 'hata))
     
     (setf n (pop acik)) 

     (push n tamamlanan)

     (if (equal (durum n) sg) 

	 (let () 
	   (print "Great. I found a solution. Here it is:")
	   (sonuc-raporu-ver acik tamamlanan s0)
	   (return (sonuc-izini-bul n)))) 

     (setf kizlar (cocuklari-getiren-islem n))

     (kac-dugum-yarattik)

     (setf kizlar
	   (DIFF kizlar tamamlanan)) 


     (setf acik (UPDATE kizlar acik)) 
     (setf acik	
	   (sort acik #'(lambda(dugum1 dugum2)
			  (< (+ (g-guncel dugum1) (t-guncel dugum1))
			     (+ (g-guncel dugum2) (t-guncel dugum2)) 
			     ))))
				

     ) 
    ) 
  ) 

(defun UPDATE (kizlar acik)
 
  (let  ( ( m  nil ) (bulunan-eski-d nil) )

    (loop 

     (if (null kizlar) (return acik))

     (setf m (pop kizlar)) 
  
     (setf bulunan-eski-d (MEMBER-DURUM (durum m) acik)) 

     (if bulunan-eski-d 
	 (let ((eski-d (first bulunan-eski-d)))

	   (if (< (g-guncel m) (g-guncel eski-d)) 
	       (setf (first bulunan-eski-d) m) ))

       (push m acik)     
       )
     

     ) 
    )
  ) 

(Defun MEMBER-DURUM (durum list-of-nodes)
  (if (null list-of-nodes) 
      nil 
    (if (equal durum (first list-of-nodes))
	list-of-nodes 
      (MEMBER-DURUM durum (rest list-of-nodes))))) 

(defun cocuklari-getiren-islem (dugum)
  (let ( (butun-cocuklar nil)
	 (cocuk-dugumler nil)
	 (ogul-dugumler nil)
	 (durum (first dugum)) )

    (setf ogul-dugumler (kaydir-yukari durum)) 
    (setf cocuk-dugumler (list (list
				ogul-dugumler
				'yukari
				(+ (g-guncel dugum)
				   (FIYAT (durum dugum) ogul-dugumler))
				(tguncel-hesapla (durum dugum) sg )
				dugum)))        

    (setf butun-cocuklar (append butun-cocuklar cocuk-dugumler))

    (setf ogul-dugumler (kaydir-asagi durum)) 

    (setf cocuk-dugumler (list (list
				ogul-dugumler
				'asagi
				(+ (g-guncel dugum)
				   (FIYAT (durum dugum) ogul-dugumler))
				(tguncel-hesapla (durum dugum) sg)
				dugum)))        

    (setf butun-cocuklar (append butun-cocuklar cocuk-dugumler))

    (setf ogul-dugumler (kaydir-saga durum)) 

    (setf cocuk-dugumler (list (list
				ogul-dugumler
				'saga
				(+ (g-guncel dugum)
				   (FIYAT (durum dugum) ogul-dugumler))
				(tguncel-hesapla (durum dugum) sg)
				dugum)))        

    (setf butun-cocuklar (append butun-cocuklar cocuk-dugumler))

    (setf ogul-dugumler (kaydir-sola durum)) 

    (setf cocuk-dugumler (list (list
				ogul-dugumler
				'sola
				(+ (g-guncel dugum)
				   (FIYAT (durum dugum) ogul-dugumler))
				(tguncel-hesapla (durum dugum) sg)
				dugum)))        

    (setf butun-cocuklar (append butun-cocuklar cocuk-dugumler))

    (setq kaydir-count (+ kaydir-count (length butun-cocuklar)))
    
    butun-cocuklar))



(setq s0 '((1 2 3 4) (5 6 0 8) (9 10 7 11) (13 14 15 12) (1 2)))
(setq sg '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 15 14 0) (3 3)))
(test "calc hhat test A*" (tguncel-hesapla s0 sg) 8)

(setq s0 '((1 2 3 4) (5 6 0 8) (9 10 7 11) (13 14 15 12) (1 2)))
(setq sg '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 0) (3 3)))
(test "A* Search test"
      (not (eql (a-yildiz-arama s0 sg nil) 'hata)) t )

(print "OK. A*  Tests Passed")
