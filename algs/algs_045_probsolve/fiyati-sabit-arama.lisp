(load "ortak.lisp")


;; Fiyati sabit arama islemi, baslangictan sonuca giden en kisa yolu
;; bulmaya ugrasir. En kisa derken, bahsettigimiz, algoritmanin daldan
;; dala atlarken (dugumleri takip ederken) her dal fiyatinin 1 degeri
;; tasidigi algoritmadan bahsediyoruz. Bu '1' degerleri her atlayista
;; toplanir, ve, ayni dugume degisik bir yoldan gelinecek olursa, eski
;; deger (ve eski dugumun) atilip, en kisa dugumun, yani yolun,
;; kullanilmasi gerekir.  Katedilen yol miktari, G-Guncel
;; degiskeninde, her dugum icinde saklanir.

;; Dugum temsil seklini
;; (Durum Kaydir-Ismi G-Guncel Ust-Dugum)
;; olarak degistirmemiz gerekiyor.

;; bu yuzden yeni erisim islemlerini tanimlayalim.
(Defun Durum (node) (first node))
(Defun Kaydir (node) (second node))
(Defun G-guncel (node) (third node))
(Defun Ust (node) (fourth node))

;;
;; Fiyati sabit arama sirasinda her dugumun fiyati, ust dugum
;; fiyati + 1 olarak hesaplanir. Burada sadece 1 degeri geri veriyoruz. 
(defun FIYAT (ust cocuk) 1)

;;
;; Unutmayalim; cocuklari-yaratan islemi de degistirmemiz gerekecek. 
;; Cunku artik FIYAT degeri, problem cozumunun bir parcasi oldu.
;;

(defun fiyati-sabit-arama (d0 ds cocuklar)
  (let ( ( acik   ;; ic-liste aslinda bir dugum
	   (list  (list s0 nil 0 nil)))
	 ;;g-guncel burada 0 
	 ( tamamlanan nil )
	 ( n      nil )
	 ( kizlar nil )) 

    (setq kaydir-sayisi 0)
    (loop
     (if (null acik) (return 'hata)) 
     
     (setf n (pop acik)) ;; Oldu. Ilk dugumu cikart.

     ;; acik listesini guncellestir
     (push n tamamlanan) ;; n dugumunu tamamlanan listesine ekle

     ;; Dikkat ederseniz, acik ve tamamlanan listelerini tutmamizin
     ;; sebebi, ayni dugumlere tekrar tekrar gelmeyi engellemek.  Bu
     ;; listelerde kaydedilmis dugumleri bir daha islemiyoruz.
     (if (equal (durum n) ds) ;;have we found our goal state?
	 (let () 
	   (print "Sonuc Bulundu. Iste Asagida:")
	   (return (sonuc-izini-bul n)))) 

     ;; yeni cocuk dugumler yaratiyoruz
     (setf kizlar (apply cocuklar (list n))) 
     
     ;; rapor verme kismi
     (kac-dugum-yarattik)

     (setf kizlar
	   (DIFF kizlar tamamlanan)) ;; Tekrar eden dugumleri cikart.

     ;; Unutmayin, ayni dugume tekrar geldiysek, bu yeni yol
     ;; mutlaka eskisinden daha uzun olacaktir. Boylece, tek yapmamiz
     ;; gereken bu dugumu acik listesinden cikartmak.

     (setf acik (UPDATE kizlar acik)) 

     ;; Acik listesini her dugumun G-guncel degerine gore
     ;; siraya diz. En azdan, en coga gore.
     (setf acik	
	   (sort acik #'(lambda(dugum1 dugum2)
			  (< (g-guncel dugum1) (g-guncel dugum2))) ))

     ;; Boylece ilk dugumu aldigimizda, bu dugumun fiyati en az
     ;; olan dugum oldugu garanti.

     ) ;; loop sonu
    ) ;;let sonu
  ) ;;defun sonu


(defun UPDATE (kizlar acik)
 
  (let  ( ( m  nil ) (bulunmus-eski-m nil) )

    (loop 

     ;; hepsini islediysek, isimiz bitti..
     (if (null kizlar) (return acik)) 

     (setf m (pop kizlar)) ;;ilk dugum uzerinde islem yapalim
  
     (setf bulunmus-eski-m 
	   (MEMBER-DURUM (durum m) acik)) ;; acikta bulduk mu?

     ;; oyle ise, bulunmus-eski-m degeri acik listesinin bir
     ;; alt-listesi olacak.
     (if bulunmus-eski-m ;; eski m acik listede bulundu ise
	 ;; durum degeri ayni olan dugumu al
	 (let ((old-m (first bulunmus-eski-m))) 
	   ;;yeni cocuk degeri g-guncel degeri 
	   (if (< (g-guncel m) (g-guncel old-m)) 
	       ;; daha ucuz ise yeni dugumu tutmak lazim
	       ;; eski dugumu ise yaramaz haline cevir
	       ;; yeni cocuk m ile degistir
	       ;; Dikkat edelim, bu sonuc ACIK listesine 
	       ;; kalici bir degisiklik yapacak.
	       (setf (first bulunmus-eski-m) m) ))

       ;; yoksa ayni degerde eski dugum bulamadik
       (push m acik)     
       )

     ;; o zaman m dugumunu acik listeye ekle. 

     ) ;; loop sonu, sonraki dugumu dene
    ) ;;let sonu
  ) ;;defun sonu

;;
;;Bu islem, sadece icinde dugum bulunan alt-listeyi geri getiriyor.
;;Yani bu listenin icindeki 'durum' bildirgec olarak verilen durum
;;ile ayni ise geri veriliyor
(Defun MEMBER-DURUM (durum dugum-listesi)
  (if (null dugum-listesi) ;;if exhausted then
      nil ;;return nil
    (if (equal durum (first dugum-listesi))	;;else if we find one
	dugum-listesi ;;return where we found it
      ;;else keep looking recursively      
      (MEMBER-DURUM durum (rest dugum-listesi))))) 


(defun cocuklari-getiren-islem (dugum)
  (let ( (butun-cocuk-dugumler nil)
	 (cocuk-dugumler nil)
	 (cocuk-durumlar nil)
	 (durum (first dugum)) )

    ;;Dikkat: Yeni ogul dugumun G-guncel degeri nasil degistiriliyor.
    ;;Ust dugumun g-guncel degeri, arti oglan dugume gelmenin fiyati
    ;;Bu FIYAT formulu her algoritmaya gore degisik
    ;;olabilir. Fiyati-sabit-arama icin tanim boyledir.

    ;; yukari
    ;;apply problem dependent operator 1
    (setf cocuk-durumlar (kaydir-yukari durum)) 

    (setf cocuk-dugumler (list (list
			   cocuk-durumlar 'north
			   (+ (g-guncel dugum)
			      (FIYAT (durum dugum) cocuk-durumlar))
			   dugum)))        

    (setf butun-cocuk-dugumler (append butun-cocuk-dugumler cocuk-dugumler))


    ;; south
    ;;apply problem dependent operator 1
    (setf cocuk-durumlar (kaydir-asagi durum)) 

    (setf cocuk-dugumler (list (list
			   cocuk-durumlar 'south
			   (+ (g-guncel dugum)
			      (FIYAT (durum dugum) cocuk-durumlar))
			   dugum)))        

    (setf butun-cocuk-dugumler 
	  (append butun-cocuk-dugumler cocuk-dugumler))


    ;; east
    ;;apply problem dependent operator 1
    (setf cocuk-durumlar (kaydir-saga durum)) 

    (setf cocuk-dugumler (list (list
			   cocuk-durumlar 'east
			   (+ (g-guncel dugum)
			      (FIYAT (durum dugum) cocuk-durumlar))
			   dugum)))        

    (setf butun-cocuk-dugumler (append butun-cocuk-dugumler cocuk-dugumler))

    ;; west
    ;;apply problem dependent operator 1
    (setf cocuk-durumlar (kaydir-sola durum)) 

    (setf cocuk-dugumler (list (list
			   cocuk-durumlar 'west
			   (+ (g-guncel dugum)
			      (FIYAT (durum dugum) cocuk-durumlar))
			   dugum)))        

    (setf butun-cocuk-dugumler 
	  (append butun-cocuk-dugumler cocuk-dugumler))
    
    (setq kaydir-sayisi 
	  (+ kaydir-sayisi (length butun-cocuk-dugumler)))

    ;;and that's it. cocuk-dugumler is returned by the function
    butun-cocuk-dugumler))

;;
;; tests
;;
(setq s0 '((1 NIL 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1)(0 1)))
(setq s1 '((1 3 4 1) (2 3 NIL 1) (5 5 4 1)(1 1 1 1)(1 2)))
(test "uniform cost search"
      (not (eql (fiyati-sabit-arama s0 s1 #'cocuklari-getiren-islem) 'fail)) t)

(print "OK. Uniform Cost Tests Passed")
