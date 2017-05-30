(defvar s0)
(defvar s1)
(defvar hata-bulma-seansi nil)
(defvar kaydir-sayisi 0)

(Defun Durum (dugum) (first dugum)) 
(Defun Kaydir (dugum) (second dugum)) 
(Defun Ust (dugum) (third dugum)) 
(defun nx (durum) (nth 0 (nth 4 durum)) )
(defun ny (durum) (nth 1 (nth 4 durum)) )

(defun nil-yerini-guncellestir (dugum x y)
  (setf dugum (remove (list (nx dugum) (ny dugum)) 
		      dugum :test #'equal ))
  (setf dugum (append dugum (list (list x y))))
  )

;;
;; X ve Y eksen degerlerine bore liste uzerinde degis tokus 
;; yap. Boylece LISP'de cok kullanilan bir suru car ve cdr 
;; kullanmaya gerek kalmiyor. 
;;
(defun degis-tokus-xy (matris x1 y1 x2 y2)
  (let (( gecici (nth y1 (nth x1 matris))))
    (setf (nth y1 (nth x1 matris)) (nth y2 (nth x2 matris)))
    (setf (nth y2 (nth x2 matris)) gecici)
    matris
  ))

;;
;; Eger verilen noktada MIL var ise, geriye T (dogru) cevabi gonder.
(defun null-xy(matris x y)
  (null (nth y (nth x matris))))


;;
;; cocuklari toplayan (yaratan) islem.
;;
(defun cocuklari-getiren-islem (dugum) 
(let ( (result-son-dugums nil) (son-durum nil) (durum (first dugum)) )

  (setf son-durum (kaydir-yukari durum))
  (setf result-son-dugums (list (list son-durum 'yukari dugum)))
  
  (setf son-durum (kaydir-asagi durum))
  (setf result-son-dugums
	(append result-son-dugums (list (list son-durum 'asagi dugum))))
  
  (setf son-durum (kaydir-saga durum))
  (setf result-son-dugums
	(append result-son-dugums (list (list son-durum 'saga dugum))))
  
  (setf son-durum (kaydir-sola durum))
  (setf result-son-dugums
	(append result-son-dugums (list (list son-durum 'sola dugum))))

  (setq kaydir-sayisi (+ kaydir-sayisi (length result-son-dugums)))
  
  result-son-dugums)) ;;and that's it Son-dugums is returned  
                      ;;by the function 

;;
;; copy wan't provided in my version of lisp.
;;
(defun copy (obj)
 (cond ( (null obj) nil)
       ( (listp obj)  (cons  (copy (first obj)) (copy (rest obj))))
       ( t obj) ) )

;;
;; My lisp has not defined this macro
;;
(defun caadddr (x)
  (car(car(cdr(cdr(cdr x))))))

;;
;; the yukari kaydir
(defun kaydir-yukari (Ust-durum)
  (let ((durum (copy Ust-durum))) 
    (cond 
     ((eql (nx durum) 0) nil) 
     (t
      (degis-tokus-xy durum
	       (- (nx durum) 1)
	       (ny durum)
	       (nx durum)
	       (ny durum))
      (setf durum (nil-yerini-guncellestir durum (- (nx durum) 1) 
					   (ny durum)))
      )
     )
    durum))

;;
;; the asagi kaydir
(defun kaydir-asagi (Ust-durum)
  (let ((durum (copy Ust-durum))) 
    (cond 
     ((eql (nx durum) 3) nil)
     (t
      (degis-tokus-xy durum
	       (+ 1 (nx durum))
	       (ny durum)
	       (nx durum)
	       (ny durum))
      (setf durum (nil-yerini-guncellestir 
		   durum (+ (nx durum) 1) (ny durum)))))
    durum))

;;
;; the saga kaydir
(defun kaydir-saga (Ust-durum)
  (let ((durum (copy Ust-durum))) 
    (cond 
     ((eql (ny durum) 3) nil)
     (t
      (degis-tokus-xy durum
	       (nx durum)
	       (+ 1 (ny durum))
	       (nx durum)
	       (ny durum))
    (setf durum (nil-yerini-guncellestir 
		 durum (nx durum) (+ 1 (ny durum))))))

    durum))

;;
;; the sola kaydir
(defun kaydir-sola (Ust-durum) 
  (let ((durum (copy Ust-durum))) 
    (cond 
     ((eql (ny durum) 0) nil)
     (t
      (degis-tokus-xy durum
	       (nx durum)
	       (- (ny durum) 1)
	       (nx durum)
	       (ny durum))
      (setf durum (nil-yerini-guncellestir 
		   durum (nx durum) (- (ny durum) 1) ))))
    durum))

;;
;; Trace
(defun sonuc-izini-bul (dugum)
  (cond ((null dugum) (print "Cozumun Baslangici") nil) 
	(t 
	 (sonuc-izini-bul (Ust dugum)) 
	 (print (Kaydir dugum)))))

;;
;; funds the difference between two sets
(defun diff (x y)
  (cond ((null x) nil)
	((not (member (first x) y  :test #'equal ))
	 (cons (first x)
	       (diff (rest x) y)))
	(t (diff (rest x) y))))

;;
;; show # of elements in lists and number of dugums generated.
(defun sonuc-raporu-ver (acik tamamlanan baslangic)
  (print "ACIK listesindeki dugum sayisi")
  (print (length acik))
  (print "TAMAMLANAN listesindeki dugum sayisi")
  (print (length tamamlanan))
  (print "Toplam Yaratilan Dugum Sayisi")
  (print kaydir-sayisi)
  )

;;
;; show # of elements depending on the debug flag status
(defun kac-dugum-yarattik()
  (when (eql hata-bulma-seansi T)
    (print "Su anda hafizada bu kadar dugum var")
    (print kaydir-sayisi))
  )

;;
;; test evaluator function
(defun test (isim exp sonuc)
  (cond
   ((equal exp sonuc) t)
   (t (print isim) (error "HATA! Birim test calismadi! "))  
   ))

;;
;; tests
;;
(setq s0 '((1 3 3 1) (2 6 4 1) (5 8 2 1)))
(test "copy" (copy s0) '((1 3 3 1) (2 6 4 1) (5 8 2 1)))

(setq s0 '((1 3 3) (2 6 4) (5 8 nil)(2 2)))
(test "degis-tokus xy 1" (degis-tokus-xy s0 2 2 1 2) 
      '((1 3 3) (2 6 NIL) (5 8 4)(2 2)))

(setq s0 '((1 3 3) (2 6 4) (5 8 nil)(2 2)))
(test "null xy" (null-xy s0 2 2) t)

(setq s0 '((1 3 3 1) (2 6 4 1) (5 8 nil 1)(1 1 1 1)(2 2)))
(test "yukari kaydir 1 test" (kaydir-yukari s0)
      '((1 3 3 1)(2 6 nil 1)(5 8 4 1)(1 1 1 1)(1 2)))

(setq s0 '((1 3 3 1) (2 6 4 1) (5 nil 4 1)(1 1 1 1)(2 1)))
(test "yukari kaydir 2 test" (kaydir-yukari s0)
      '((1 3 3 1)(2 nil 4 1)(5 6 4 1)(1 1 1 1)(1 1)))

(setq s0 '((1 3 nil 1) (2 6 4 1) (5 8 4 1)(1 1 1 1)(0 2)))
(test "asagi kaydir 1 test" (kaydir-asagi s0)
      '((1 3 4 1)(2 6 nil 1)(5 8 4 1)(1 1 1 1)(1 2)))

(setq s0 '((1 3 3 1)(2 6 4 1)(5 8 nil 1)(1 1 1 1)(2 2)))
(test "degis-tokus xy 2" (degis-tokus-xy s0 1 1 2 1)
      '((1 3 3 1)(2 8 4 1)(5 6 nil 1)(1 1 1 1)(2 2)))

(setq s0 '((1 3 3 1) (2 nil 4 1) (5 5 4 1)(1 1 1 1)(1 1)))
(test "asagi kaydir 2 test" (kaydir-asagi s0)
      '((1 3 3 1)(2 5 4 1)(5 nil 4 1)(1 1 1 1)(2 1)))

(setq s0 '((1 nil 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1)(0 1)))
(test "saga kaydir 1 test" (kaydir-saga s0)
      '((1 3 nil 1)(2 3 4 1)(5 5 4 1)(1 1 1 1)(0 2)))

(setq s0 '((1 3 3 1) (2 nil 4 1) (5 5 4 1)(1 1 1 1)(1 1)))
(test "saga kaydir 2 test" (kaydir-saga s0)
      '((1 3 3 1)(2 4 nil 1)(5 5 4 1)(1 1 1 1)(1 2)))

(setq s0 '((1 nil 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1)(0 1)))
(test "sola kaydir 1 test" (kaydir-sola s0)
      '((nil 1 3 1)(2 3 4 1)(5 5 4 1)(1 1 1 1)(0 0)))

(setq s0 '((1 3 3 1) (2 nil 4 1)(5 5 4 1)(1 1 1 1)(1 1)))
(test "sola kaydir 2 test" (kaydir-sola s0)
      '((1 3 3 1)(nil 2 4 1)(5 5 4 1)(1 1 1 1)(1 0)))

(setq s0 '((1 3 3) (2 nil 4) (5 5 4)))
(setq s0 (append s0 '((2 3 4))))
(test "ekleme testi" s0 '((1 3 3) (2 nil 4) (5 5 4)(2 3 4)) )

(setq s0 '((1 3 3 1) (2 nil 4 1) (5 5 4 1)(1 1 1 1)(1 1)))
(setq s0 (list s0 nil nil))
(setq s1 '((((1 NIL 3 1) (2 3 4 1) (5 5 4 1) (1 1 1 1) (0 1)) YUKARI
	    (((1 3 3 1) (2 NIL 4 1) (5 5 4 1) (1 1 1 1) (1 1)) NIL NIL))
	   (((1 3 3 1) (2 5 4 1) (5 NIL 4 1) (1 1 1 1) (2 1)) ASAGI
	    (((1 3 3 1) (2 NIL 4 1) (5 5 4 1) (1 1 1 1) (1 1)) NIL NIL))
	   (((1 3 3 1) (2 4 NIL 1) (5 5 4 1) (1 1 1 1) (1 2)) SAGA
	    (((1 3 3 1) (2 NIL 4 1) (5 5 4 1) (1 1 1 1) (1 1)) NIL NIL))
	   (((1 3 3 1) (NIL 2 4 1) (5 5 4 1) (1 1 1 1) (1 0)) SOLA
	    (((1 3 3 1) (2 NIL 4 1) (5 5 4 1) (1 1 1 1) (1 1)) NIL NIL))))
(test "cocuk dugum bulucu islemi test" (cocuklari-getiren-islem s0) s1)


(setq s0 '((1 NIL 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1)(0 1)))
(setq s1 '((1 NIL 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1)(0 1)))
(test "durum equality" (equal s0 s1) t)

(setq s0 '(((1 NIL 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1))
	   YUKARI (((1 3 3 1) (2 NIL 4 1) (5 5 4 1)(1 1 1 1)) NIL NIL)))
(test "durum test" (equal (durum s0)
			  '((1 NIL 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1))) t)

(setf s0 '((((1 3 3 1) (2 NIL 4 1) (5 5 4 1)(1 1 1 1 )(1 1)) NIL NIL)
	   (((1 3 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1)(1 1)) NIL NIL)
	   (((1 3 3 1) (2 5 4 1) (5 5 4 1)(1 1 1 1)(1 1)) NIL NIL)))
(setq s1 '((((1 3 3 1) (2 NIL 4 1) (5 5 4 1)(1 1 1 1)(1 1)) NIL NIL)
	   (((1 3 3 1) (2 5 4 1) (6 6 6 1)(1 1 1 1)(1 1)) NIL NIL)) )
(test "diff test" (diff s1 s0)
      '((((1 3 3 1) (2 5 4 1) (6 6 6 1)(1 1 1 1)(1 1)) NIL NIL))   )

(print "OLDU. Ortak Birim Testler Basari Ile Calisti")

