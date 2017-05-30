(load "ortak.lisp")

(defun kat-engelli-da (node goal succesors depth)

  ;;Hmmmm...where did Open, Closed and N go?
  (block
   DBDFS (let ((daughters nil) )
	   
	   (if (equal (durum node) goal)
	       (return-from DBDFS (sonuc-izini-bul node)))

	   ;;bottomed out in the search space
	   (if (= depth 1) (return-from DBDFS nil)) 
	   ;;without finding a goal down this path

	   ;;presumably we have a LIST of derived
	   (setf daughters (cocuklari-getiren-islem node))
	   ;;here we generate new derived durums

	   ;; debugging
	   (kac-dugum-yarattik)

	   (loop ;;so we iterate down them looking for 
	    ;;a solution path...this could easily
	    ;;be changed to a do form....

	    ;;failed to find a solution
	    (if (null daughters) (return-from DBDFS nil))

	    (if (kat-engelli-da ;;recursive call with
		 (pop daughters) ;;the first of daughters& daughters updated
		 goal ;;same old goal durum
		 daughters ;;same old set of operators
		 (- depth 1)) ;;but a shallower depth!

		;;here we did find a solution so we leave happy
		(return-from DBDFS t)

	      )	;;;;end if
	    ) ;;;;ends loop
	   ) ;;ends let
   ) ;; ends block
  ) ;;ends defun  {otherwise known as "]"  :)  }


;;
;; outside method
;;
(defun kat-kat-arama (d0 ds cocuklar derinlik-limiti)
  (setf kaydir-sayisi 0)
  (kat-engelli-da (list d0 nil nil) ds cocuklar derinlik-limiti)
  (print "kac dugum yaratildi")
  (print kaydir-sayisi)
  )

;;
;; outside method for iterative deepening
;;
(defun gitgide-icin-kka (d0 dg cocuklar derinlik-limiti)
  (kat-engelli-da (list d0 nil nil) ds cocuklar derinlik-limiti)
  )


;;
;; tests
;;
(setq s0 '((1 NIL 3 1) (2 3 4 1) (5 5 4 1)(1 1 1 1)(0 1)))
(setq s1 '((1 3 4 1) (2 3 NIL 1) (5 5 4 1)(1 1 1 1)(1 2)))
(test "kka derinlik limiti" (not (eql (kat-kat-arama s0 s1 nil 3) 'fail)) t )

(print "OLDU. Kat Engelli Kat-Kat Arama Basari Ile Gecti")
