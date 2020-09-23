(load "ortak.lisp")

;;
;; her kati arayip, bitince sonraki katta arama yapmak. 
(defun kat-kat-ara (d0 ds cocuklar)

  ;; dikkat edin, icerideki liste bir dugum
  (let ( ( acik (list (list d0 nil nil) ) ) 
	 ( tamamlanmis nil ) 
	 ( n nil ) 
	 ( kizlar nil )) 
    (setq kaydir-sayisi 0)
    (loop 
     (if (null acik) (return 'hata)) ;;hata var, geri rapor ver
       
     (setf n (pop acik)) ;; ilk dugumu cikart

     ;; tamamlanmis listesine n dugumunu koy, cunku birazdan onu 
     ;; isleyecegiz
     (push n tamamlanmis) 

     ;; sonuc dugumune geldik mi? (bulduk mu?)
     (when (equal (durum n) ds)
       (print "Sonuc bulundu. Nasil buldugumuz asagida")
       (sonuc-raporu-ver acik tamamlanmis d0)
       (return (sonuc-izini-bul n))
       )

     ;; iste burada yeni cocuk dugumler cikartiyoruz
     (setf kizlar (apply cocuklar (list n)))

     ;; hata ayiklama icin lazim olabilir
     (kac-dugum-yarattik)

     ;; iki kere tekrar eden dugumleri cikar. (DIFF=fark demektir, iki
     ;; liste arasindaki benzerleri cikartir, farki getirir.
     (setf kizlar 
	   (DIFF kizlar (append acik tamamlanmis))) 

     ;; dikkat: yeni dugumleri listenin SONUNA koyuyoruz yazilim
     ;; bilimde buna 'kuyruk' veri yapisi denir. Yani, kuyruga son
     ;; giren, son cikar. Yukarida 'pop (cikart)' deyince su anda
     ;; koydugumuz deger gelmeyecek. (liste dolu ise). Eger bu noktada
     ;; kuyruk yapisi kullaniliyorsa, bu algoritmayi kat-kat arama
     ;; haline cevirecek.
     (setf acik (append acik kizlar))	

    )))

;;
;; testler
;;
(setq d0 '((1 NIL 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1)(0 1)))
(setq ds '((1 3 4 1) (2 3 NIL 1) (5 5 4 1)(1 1 1 1)(1 2)))
(test "basit kat-kat-ara"
      (not (eql (kat-kat-ara d0 ds #'cocuklari-getiren-islem) 'hata)) t )

(print "OLDU. KAT-KAT-ARA Testleri Calisti")
