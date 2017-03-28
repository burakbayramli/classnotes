;; Oyun tahtasi liste icinde liste olarak temsil edildi. Her tahta
;; konumu, eger uzerinde bir tas var ise, bir liste nesnesi
;; tasir. Eger bu tas normal bir tas ise, bu liste ("b" nil) gibi
;; olur. Eger bu tas kral tasi ise, ("b" T) olarak temsil
;; edilecektir. Yani, ikinci T ya da NIL degeri krallik
;; gostergesidir. Eger konum uzerinde hic tas yok ise, liste nesnesi
;; yerine NIL bulacaksiniz, dikkat edin (nil nil) degil. Listenin en
;; sonundaki iki rakam, tahta uzerinde o anda her renkten kac tas
;; kaldigini gosterir.  Birinci rakam beyaz, ikincisi siyah icin. Bunu
;; yapmamizin sebebi algoritmayi hizlandirmak icin, boylece ikidebir
;; tahtayi bastan sona taramak gerekmiyor.

(setq *beyaz*                  "b")
(setq *siyah*                  "s")
(setq *enust-renk*              nil)
(setq *enalt-renk*              nil)
(setq *tahta*                  nil)

(setq bilgisayar-hamle-sayisi 0)

;;
;; test isleten islev
(defun test (isim deyim sonuc)
  (cond
   ((equal deyim sonuc) t)
   (t (print isim) (error "HATA! Birim test bir yanlis buldu! "))  
   ))

;;
;; Tahta silbastan baslayinca, enust-renk bilgisayara aittir.
(defun oyuna-basla (renk)
  (setq bilgisayar-hamle-sayisi 0)
  (setq *tahta* (copy-tree (tahtayi-hazirla)))
  (cond
   ((equal renk *beyaz*)
    (progn (setq *enust-renk* "b")
	   (setq *enalt-renk* "s")))
   (t (progn  (setq *enust-renk* "s")
	      (setq *enalt-renk* "b")
	      )
      )
   )
  )

;;
;; tahtayi sifirla
(defun tahtayi-hazirla ()
  '(((nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil) )
     (("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil)
     (nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil) )
     (nil nil nil nil nil nil nil nil)
     (nil nil nil nil nil nil nil nil)
     (("s" nil)  nil ("s" nil)  nil ("s" nil)  nil ("s" nil)  nil)
     (nil ("s" nil)  nil ("s" nil)  nil ("s" nil)  nil ("s" nil) )
     (("s" nil)  nil ("s" nil)  nil ("s" nil)  nil ("s" nil)  nil)
     ) (12 12))
  )

;;
;; tahtanin o anki durumunu ekrana metin olarak bas (hata bulmak
;; icin cok yararli oldu)
(defun goster (tahta)
  (terpri)
  (write-line "+=0===1===2===3===4===5===6===7-+")
  (let ((y-sayici 8))
    (dotimes (y-ozyine 8)
      (princ (- y-sayici 1))
      (let ((x-sayici 7))
	(dolist (kon (nth (- 7 (- y-sayici 1)) (car tahta)))
	  (princ " ")
	  (cond
	   ((equal kon nil) (princ " "))
	   ((and (equal (car kon) "b") (null (cadr kon))) (princ "b"))
	   ((and (equal (car kon) "b") (cadr kon)) (princ "B"))
	   ((and (equal (car kon) "s") (null (cadr kon))) (princ "s"))
	   ((and (equal (car kon) "s") (cadr kon)) (princ "S"))
	   )
	  (princ " ")
	  (if (null (equal x-sayici 0)) (princ "|"))
	  (setq x-sayici (- x-sayici 1))
	  )
	)
      (write-line "|")
      (if (null (equal y-sayici 1))
	  (write-line "|===============================|"))
      (setq y-sayici (- y-sayici 1))
      )
    )
  (write-line "+=0===1===2===3===4===5===6===7=+")
  nil
  )

;;
;; her hamle su sekilde temsil edilir, mesela '((0 2)(1 2))
;; Yani, hamle  X=1, Y=2 konumundan X=1, Y=2 konumuna yapilacak.
(defun tas-oynat (hamle tahta)

  (setq gecici-tas1 (tas-degeri-ver (car hamle) tahta))
  (setq gecici-tas2 (tas-degeri-ver (cadr hamle) tahta))

  ;; hamle
  (setq tahta 
	(tas-degeri-degis (cadr hamle) 
			  (tas-cikart (car hamle) tahta) gecici-tas1))
  (setq atlanan-konum 
	(atlayarak-gidilen-konumlar (car hamle) (cadr hamle))) 

  ;; eger gerekiyorsa krala cevir
  (cond
   ((and (equal (car gecici-tas1) "s") (equal 7 (cadadr hamle))) 
    (setf (cadr gecici-tas1) t))
   ((and (equal (car gecici-tas1) "b") (equal 0 (cadadr hamle))) 
    (setf (cadr gecici-tas1) t))
   )
    
  (cond
   (atlanan-konum ;; hareket atlama ise
    (progn 
      (if (equal (car (tas-degeri-ver atlanan-konum tahta)) "b")
	  (setf (caadr tahta) (- (caadr tahta) 1))
	(setf (cadadr tahta) (- (cadadr tahta) 1)))
      (setq tahta (tas-cikart atlanan-konum tahta))
      (cond
       ((caddr hamle) ;; birkac atlama var ise, ozyineli olarak cagir
	(progn
	  (tas-oynat (cdr hamle) tahta)
	  )
	)
       (t
	tahta ))
      )
    )
   (t ;; atlama degil
    tahta
    )  
   )
  ) 

;;
;; tas degerini geri getir
(defun tas-degeri-ver (kon tahta)
  (nth (car kon) (nth (- 7 (cadr kon)) (car tahta)))
  )

;;
;; tasin degerini tahtaya koy
(defun tas-degeri-degis (kon tahta eleman)
  (setf (nth (car kon) (nth (- 7 (cadr kon)) (car tahta))) eleman)
  tahta
  )

;;
;; tasi tahtadan al
(defun tas-cikart (kon tahta)
  (setf (nth (car kon) (nth (- 7 (cadr kon)) (car tahta))) nil)
  tahta
  )

;;
;; 
(defun atlayarak-gidilen-konumlar (kon_a kon_b)
  (setq x (car kon_a))
  (setq y (cadr kon_a))
  (cond
   ((equal kon_b (list (+ x 1) (+ y 1))) nil)
   ((equal kon_b (list (+ x 1) (- y 1))) nil)
   ((equal kon_b (list (- x 1) (+ y 1))) nil)
   ((equal kon_b (list (- x 1) (- y 1))) nil)
   ;; ya da ortada kalan taslarin kordinatlarini getir
   (t 
    (cond
     ((< x (car kon_b)) (setq gecici_x (+ 1 x)))
     (t (setq gecici_x (- x 1)))
     )
    (cond
     ((< y (cadr kon_b)) (setq gecici_y (+ 1 y)))
     (t (setq gecici_y (- y 1)))
     )
    (list gecici_x gecici_y)
    )
   )
  )

(defun hamle-kurallara-uygunmu (hareket tahta renk)
  ;; hareket menzilin disinda ise, bu kural disi bir hareket
  (cond
   ((null (menzildemi (cadr hareket)))
    nil
    )
   ;; gitmeye ugrastigimiz yerde zaten bir tas var ise,
   ;; bu da kural disi bir harekettir
   ((tas-degeri-ver (cadr hareket) tahta) 
    nil
    )
   (t 
    (let (
	  (eleman (tas-degeri-ver (car hareket) tahta))
	  (baslax (caar hareket))
	  (baslay (cadar hareket))
	  (bitisx   (caadr hareket))
	  (bitisy   (cadadr hareket)))

      (cond 
       ;; eger hareket ziplama hareketi ise
       ((equal 2 (abs (- baslax bitisx)))
	(cond
	 ((equal eleman (list "s" nil))
	  (if (and (equal 2 (- bitisy baslay))
		   (equal (car (tas-degeri-ver
				(atlayarak-gidilen-konumlar 
				 (car hareket)
				 (cadr hareket)) tahta)) "b"))
	      t
	    )
	  )
	 ((equal eleman (list "b" nil))
	  (if (and (equal -2 (- bitisy baslay))
		   (equal (car (tas-degeri-ver
				(atlayarak-gidilen-konumlar 
				 (car hareket)
				 (cadr hareket)) tahta)) "s"))
	      t
	    )
	  )
	 ((equal (cadr eleman) t)
	  (if (and (equal 2 (abs (- bitisy baslay)))
		   (equal (car (tas-degeri-ver
				(atlayarak-gidilen-konumlar 
				 (car hareket)
				 (cadr hareket)) tahta))
			  (renk-degistir renk)))
	      t
	    )
	  )
	 )
	)
       ;; ziplama degil ise
       ((equal 1 (abs (- baslax bitisx)))
	(cond
	 ((equal eleman (list "s" nil))
	  (if (equal 1 (- bitisy baslay))
	      t
	    )
	  )
	 ((equal eleman (list "b" nil))
	  (if (equal -1 (- bitisy baslay))
	      t
	    )
	  )
	 ((equal (cadr eleman) t)
	  (if (equal 1 (abs (- baslay bitisy))) t)
	  )
	 )
	)
       )
      )
    )
   )
  )

(defun menzildemi (kon)
  (let ((x (car kon)) (y (cadr kon)))
    (cond 
     ((and (< x 8) (< y 8) (> x -1) (> y -1)) t)
     (t nil)
     )
    )
  )

(defun renk-degistir (color)
  (cond
   ((equal "b" color) "r")
   (t "b")
   )
  )

;;
;; girilen renk ve tahtaya gore, oyuncunun yapabilecegi
;; butun hareketleri geri getir
(defun hareket-listesi-hesapla (renk tahta)
  (setq tas-konumlari (tas-yerlerini-ver renk tahta))
  (let ((muhtemel-hareketler nil) (mumkun-atlamalar nil))
    (dolist (kon tas-konumlari)
      (setq mumkun-atlamalar 
	    (append mumkun-atlamalar
		    (mecburi-hareketleri-ver kon tahta renk)))
      (if (null mumkun-atlamalar) 
	  (progn
	    (if (hamle-kurallara-uygunmu
		 (list kon (list (- (car kon) 1)
				 (- (cadr kon) 1))) tahta renk)
		(setq muhtemel-hareketler
		      (append 
		       muhtemel-hareketler
		       (list (list kon (list (- (car kon) 1)
					     (- (cadr kon) 1)))))))
	    (if (hamle-kurallara-uygunmu
		 (list kon (list (- (car kon) 1)
				 (+ (cadr kon) 1))) tahta renk)
		(setq muhtemel-hareketler
		      (append 
		       muhtemel-hareketler
		       (list (list kon (list (- (car kon) 1)
					     (+ (cadr kon) 1)))))))
	    (if (hamle-kurallara-uygunmu
		 (list kon (list (+ (car kon) 1)
				 (- (cadr kon) 1))) tahta renk)
		(setq muhtemel-hareketler
		      (append 
		       muhtemel-hareketler
		       (list (list kon (list (+ (car kon) 1)
					     (- (cadr kon) 1)))))))
	    (if (hamle-kurallara-uygunmu
		 (list kon (list (+ (car kon) 1)
				 (+ (cadr kon) 1))) tahta renk)
		(setq muhtemel-hareketler
		      (append 
		       muhtemel-hareketler
		       (list (list kon (list (+ (car kon) 1)
					     (+ (cadr kon) 1)))))))
	    ) ;; progn sonu
	) ;; if sonu
      )
    (if mumkun-atlamalar mumkun-atlamalar muhtemel-hareketler)
    )
  )

(defun tas-yerlerini-ver (renk tahta)
    (setq konumlar nil)

    (if (equal renk (car (tas-degeri-ver '(0 0) tahta)))
        (setq konumlar (append konumlar '((0 0)))))
    (if (equal renk (car (tas-degeri-ver '(0 1) tahta)))
        (setq konumlar (append konumlar '((0 1)))))
    (if (equal renk (car (tas-degeri-ver '(0 2) tahta)))
        (setq konumlar (append konumlar '((0 2)))))
    (if (equal renk (car (tas-degeri-ver '(0 3) tahta)))
        (setq konumlar (append konumlar '((0 3)))))
    (if (equal renk (car (tas-degeri-ver '(0 4) tahta)))
        (setq konumlar (append konumlar '((0 4)))))
    (if (equal renk (car (tas-degeri-ver '(0 5) tahta)))
        (setq konumlar (append konumlar '((0 5)))))
    (if (equal renk (car (tas-degeri-ver '(0 6) tahta)))
        (setq konumlar (append konumlar '((0 6)))))
    (if (equal renk (car (tas-degeri-ver '(0 7) tahta)))
        (setq konumlar (append konumlar '((0 7)))))

    (if (equal renk (car (tas-degeri-ver '(1 0) tahta)))
        (setq konumlar (append konumlar '((1 0)))))
    (if (equal renk (car (tas-degeri-ver '(1 1) tahta)))
        (setq konumlar (append konumlar '((1 1)))))
    (if (equal renk (car (tas-degeri-ver '(1 2) tahta)))
        (setq konumlar (append konumlar '((1 2)))))
    (if (equal renk (car (tas-degeri-ver '(1 3) tahta)))
        (setq konumlar (append konumlar '((1 3)))))
    (if (equal renk (car (tas-degeri-ver '(1 4) tahta)))
        (setq konumlar (append konumlar '((1 4)))))
    (if (equal renk (car (tas-degeri-ver '(1 5) tahta)))
        (setq konumlar (append konumlar '((1 5)))))
    (if (equal renk (car (tas-degeri-ver '(1 6) tahta)))
        (setq konumlar (append konumlar '((1 6)))))
    (if (equal renk (car (tas-degeri-ver '(1 7) tahta)))
        (setq konumlar (append konumlar '((1 7)))))

    (if (equal renk (car (tas-degeri-ver '(2 0) tahta)))
        (setq konumlar (append konumlar '((2 0)))))
    (if (equal renk (car (tas-degeri-ver '(2 1) tahta)))
        (setq konumlar (append konumlar '((2 1)))))
    (if (equal renk (car (tas-degeri-ver '(2 2) tahta)))
        (setq konumlar (append konumlar '((2 2)))))
    (if (equal renk (car (tas-degeri-ver '(2 3) tahta)))
        (setq konumlar (append konumlar '((2 3)))))
    (if (equal renk (car (tas-degeri-ver '(2 4) tahta)))
        (setq konumlar (append konumlar '((2 4)))))
    (if (equal renk (car (tas-degeri-ver '(2 5) tahta)))
        (setq konumlar (append konumlar '((2 5)))))
    (if (equal renk (car (tas-degeri-ver '(2 6) tahta)))
        (setq konumlar (append konumlar '((2 6)))))
    (if (equal renk (car (tas-degeri-ver '(2 7) tahta)))
        (setq konumlar (append konumlar '((2 7)))))

    (if (equal renk (car (tas-degeri-ver '(3 0) tahta)))
        (setq konumlar (append konumlar '((3 0)))))
    (if (equal renk (car (tas-degeri-ver '(3 1) tahta)))
        (setq konumlar (append konumlar '((3 1)))))
    (if (equal renk (car (tas-degeri-ver '(3 2) tahta)))
        (setq konumlar (append konumlar '((3 2)))))
    (if (equal renk (car (tas-degeri-ver '(3 3) tahta)))
        (setq konumlar (append konumlar '((3 3)))))
    (if (equal renk (car (tas-degeri-ver '(3 4) tahta)))
        (setq konumlar (append konumlar '((3 4)))))
    (if (equal renk (car (tas-degeri-ver '(3 5) tahta)))
        (setq konumlar (append konumlar '((3 5)))))
    (if (equal renk (car (tas-degeri-ver '(3 6) tahta)))
        (setq konumlar (append konumlar '((3 6)))))
    (if (equal renk (car (tas-degeri-ver '(3 7) tahta)))
        (setq konumlar (append konumlar '((3 7)))))

    (if (equal renk (car (tas-degeri-ver '(4 0) tahta)))
        (setq konumlar (append konumlar '((4 0)))))
    (if (equal renk (car (tas-degeri-ver '(4 1) tahta)))
        (setq konumlar (append konumlar '((4 1)))))
    (if (equal renk (car (tas-degeri-ver '(4 2) tahta)))
        (setq konumlar (append konumlar '((4 2)))))
    (if (equal renk (car (tas-degeri-ver '(4 3) tahta)))
        (setq konumlar (append konumlar '((4 3)))))
    (if (equal renk (car (tas-degeri-ver '(4 4) tahta)))
        (setq konumlar (append konumlar '((4 4)))))
    (if (equal renk (car (tas-degeri-ver '(4 5) tahta)))
        (setq konumlar (append konumlar '((4 5)))))
    (if (equal renk (car (tas-degeri-ver '(4 6) tahta)))
        (setq konumlar (append konumlar '((4 6)))))
    (if (equal renk (car (tas-degeri-ver '(4 7) tahta)))
        (setq konumlar (append konumlar '((4 7)))))

    (if (equal renk (car (tas-degeri-ver '(5 0) tahta)))
        (setq konumlar (append konumlar '((5 0)))))
    (if (equal renk (car (tas-degeri-ver '(5 1) tahta)))
        (setq konumlar (append konumlar '((5 1)))))
    (if (equal renk (car (tas-degeri-ver '(5 2) tahta)))
        (setq konumlar (append konumlar '((5 2)))))
    (if (equal renk (car (tas-degeri-ver '(5 3) tahta)))
        (setq konumlar (append konumlar '((5 3)))))
    (if (equal renk (car (tas-degeri-ver '(5 4) tahta)))
        (setq konumlar (append konumlar '((5 4)))))
    (if (equal renk (car (tas-degeri-ver '(5 5) tahta)))
        (setq konumlar (append konumlar '((5 5)))))
    (if (equal renk (car (tas-degeri-ver '(5 6) tahta)))
        (setq konumlar (append konumlar '((5 6)))))
    (if (equal renk (car (tas-degeri-ver '(5 7) tahta)))
        (setq konumlar (append konumlar '((5 7)))))

    (if (equal renk (car (tas-degeri-ver '(6 0) tahta)))
        (setq konumlar (append konumlar '((6 0)))))
    (if (equal renk (car (tas-degeri-ver '(6 1) tahta)))
        (setq konumlar (append konumlar '((6 1)))))
    (if (equal renk (car (tas-degeri-ver '(6 2) tahta)))
        (setq konumlar (append konumlar '((6 2)))))
    (if (equal renk (car (tas-degeri-ver '(6 3) tahta)))
        (setq konumlar (append konumlar '((6 3)))))
    (if (equal renk (car (tas-degeri-ver '(6 4) tahta)))
        (setq konumlar (append konumlar '((6 4)))))
    (if (equal renk (car (tas-degeri-ver '(6 5) tahta)))
        (setq konumlar (append konumlar '((6 5)))))
    (if (equal renk (car (tas-degeri-ver '(6 6) tahta)))
        (setq konumlar (append konumlar '((6 6)))))
    (if (equal renk (car (tas-degeri-ver '(6 7) tahta)))
        (setq konumlar (append konumlar '((6 7)))))

    (if (equal renk (car (tas-degeri-ver '(7 0) tahta)))
        (setq konumlar (append konumlar '((7 0)))))
    (if (equal renk (car (tas-degeri-ver '(7 1) tahta)))
        (setq konumlar (append konumlar '((7 1)))))
    (if (equal renk (car (tas-degeri-ver '(7 2) tahta)))
        (setq konumlar (append konumlar '((7 2)))))
    (if (equal renk (car (tas-degeri-ver '(7 3) tahta)))
        (setq konumlar (append konumlar '((7 3)))))
    (if (equal renk (car (tas-degeri-ver '(7 4) tahta)))
        (setq konumlar (append konumlar '((7 4)))))
    (if (equal renk (car (tas-degeri-ver '(7 5) tahta)))
        (setq konumlar (append konumlar '((7 5)))))
    (if (equal renk (car (tas-degeri-ver '(7 6) tahta)))
        (setq konumlar (append konumlar '((7 6)))))
    (if (equal renk (car (tas-degeri-ver '(7 7) tahta)))
        (setq konumlar (append konumlar '((7 7)))))

    konumlar
)


(defun mecburi-hareketleri-ver (kon tahta renk)
  (let ((muhtemel-hareketler nil) (eleman (tas-degeri-ver kon tahta)))
    (if (cadr eleman)
	;;
	;;  KRALLAR
	;;
	(progn
	  (setq muhtemel-hareketler
		(atlamalari-bul
		 (copy-tree tahta) kon (+ (car kon) 2)
		 (+ (cadr kon) 2) renk muhtemel-hareketler))
	  (setq muhtemel-hareketler
		(atlamalari-bul
		 (copy-tree tahta) kon (+ (car kon) 2)
		 (- (cadr kon) 2) renk muhtemel-hareketler))
	  (setq muhtemel-hareketler
		(atlamalari-bul
		 (copy-tree tahta) kon (- (car kon) 2)
		 (+ (cadr kon) 2) renk muhtemel-hareketler))
	  (setq muhtemel-hareketler
		(atlamalari-bul
		 (copy-tree tahta) kon (- (car kon) 2)
		 (- (cadr kon) 2) renk muhtemel-hareketler))
	  )
      ;;
      ;;  NORMAL
      ;;
      (progn
	(if (equal renk "b") 
	    (progn
	      ;;
	      ;;  BEYAZ  :: Bu demektir ki normal parcalar icin + kullan
	      ;;
	      (setq muhtemel-hareketler
		    (atlamalari-bul
		     (copy-tree tahta) kon (+ (car kon) 2)
		     (- (cadr kon) 2) renk muhtemel-hareketler))
	      (setq muhtemel-hareketler
		    (atlamalari-bul
		     (copy-tree tahta) kon (- (car kon) 2)
		     (- (cadr kon) 2) renk muhtemel-hareketler))
	      )
	  (progn
	    ;;
	    ;;  SIYAH  :: Buna gore normal parcalar icin '-' kullan
	    ;;
	    (setq muhtemel-hareketler
		  (atlamalari-bul
		   (copy-tree tahta) kon (+ (car kon) 2)
		   (+ (cadr kon) 2) renk muhtemel-hareketler))
	    (setq muhtemel-hareketler
		  (atlamalari-bul
		   (copy-tree tahta) kon (- (car kon) 2)
		   (+ (cadr kon) 2) renk muhtemel-hareketler))
	    )
	  )
	)
      )
    muhtemel-hareketler
    )
  )

;;
;; 
(defun atlamalari-bul (gecici_tahta kon yenix yeniy renk m-hareketler)
  (let ((muhtemel-hareketler nil))
    (if (hamle-kurallara-uygunmu 
	 (list kon (list yenix yeniy)) gecici_tahta renk)
	(progn
	  (setq gecici_tahta 
		(tas-oynat (list kon (list yenix yeniy)) gecici_tahta))
	  (setq gecici 
		(mecburi-hareketleri-ver 
		 (list yenix yeniy) gecici_tahta renk))
	  (if gecici 
	      (dolist (b gecici)
		(setq a (list kon (list yenix yeniy)))
		(if muhtemel-hareketler
		    (setq muhtemel-hareketler
			  (append 
			   muhtemel-hareketler 
			   (list (append a (cdr b)))))
		  (setq muhtemel-hareketler
			(list (append muhtemel-hareketler a (cdr b))))
		  )
		)
	    (setq muhtemel-hareketler (list (list kon (list yenix yeniy))))
	    )
	  ))
    (if muhtemel-hareketler 
	(append m-hareketler muhtemel-hareketler) m-hareketler)
    )
  )

;; Oyunun bitip bitmedigine karar veren islev budur. Cagirmak icin
;; bir renk ve tahtanin durumu bildirilir, eger renk icin hic bir
;; hareket kalmadi ise, bu oyuncu icin oyun bitmis demektir.
;; Bu oyuncunun belki taslarinin onu kapanmistir, ya da hic
;; tasi kalmamistir. Her iki halde de hareket-listesi-hesapla
;; hic hareket listesi geri getirmez. 
(defun oyun-bitti-mi?(renk tahta)
  (cond
   ((equal (hareket-listesi-hesapla renk tahta) nil) t)
   (t nil))
  )


;; -------------------------------------------------------------
;;
;; testler

(oyuna-basla *beyaz*)
(setf sonuc (tas-oynat '((0 2)(1 2)) *tahta*))
(setf beklenen-sonuc
      '(((nil ("b" nil) nil ("b" nil) nil ("b" nil) nil ("b" nil))
	 (("b" nil) nil ("b" nil) nil ("b" nil) nil ("b" nil) nil)
	 (nil ("b" nil) nil ("b" nil) nil ("b" nil) nil ("b" nil))
	 (nil nil nil nil nil nil nil nil) (nil nil nil nil nil nil nil nil)
	 (nil ("s" nil) ("s" nil) nil ("s" nil) nil ("s" nil) nil)
	 (nil nil nil ("s" nil) nil ("s" nil) nil ("s" nil))
	 (("s" nil) nil ("s" nil) nil ("s" nil) nil ("s" nil) nil))
	(12 11)) )
(test "tek hareket yap" (equal beklenen-sonuc sonuc) t)

(oyuna-basla *beyaz*)
(test "yanlis hareket yap"
      (hamle-kurallara-uygunmu '((0 2)(1 2)) *tahta* *beyaz*) nil)

(oyuna-basla *beyaz*)
(test "dogru hamle yap" 
      (hamle-kurallara-uygunmu '((0 2)(1 3)) *tahta* 'USER::BLACK) T)

(oyuna-basla *beyaz*)
(setf beklenen-hareket-listesi 
      '(((1 5) (0 4)) ((1 5) (2 4)) ((3 5) (2 4))
	((3 5) (4 4)) ((5 5) (4 4)) ((5 5) (6 4)) ((7 5) (6 4))))
(setf sonuc (hareket-listesi-hesapla "b" *tahta*))
(test "oyuna uygun hareket" (equal sonuc beklenen-hareket-listesi ) t)

(oyuna-basla *beyaz*)
(setf gecici-sonuc (tas-oynat '((0 2)(2 7)) *tahta*))
(setf beklenen-tahta
      '(((nil ("b" nil) ("s" T) ("b" nil) nil ("b" nil) nil ("b" nil))
	 (("b" nil) nil ("b" nil) nil ("b" nil) nil ("b" nil) nil)
	 (nil ("b" nil) nil ("b" nil) nil ("b" nil) nil ("b" nil))
	 (nil nil nil nil nil nil nil nil) 
	 (nil nil nil nil nil nil nil nil)
	 (nil nil ("s" nil) nil ("s" nil) nil ("s" nil) nil)
	 (nil ("s" nil) nil ("s" nil) nil ("s" nil) nil ("s" nil))
	 (("s" nil) nil ("s" nil) nil ("s" nil) nil ("s" nil) nil))
	(12 11)))
(test "kral yap" (equal gecici-sonuc beklenen-tahta) t)

(setf hic-siyah-tassiz-tahta
  '(((nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil) )
     (("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil)
     (nil ("b" nil)  nil ("b" nil)  nil ("b" nil)  nil ("b" nil) )
     (nil nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil nil)
     (nil nil nil nil nil nil nil nil)(nil nil nil nil nil nil nil nil )
     (nil nil nil nil nil nil nil nil)) (12 0)))
(test "siyah icin oyun bitti" 
      (oyun-bitti-mi? *siyah* hic-siyah-tassiz-tahta) t)

(print "Tamam. Temel Birim Testler Gecti")
