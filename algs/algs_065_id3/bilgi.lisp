;; Parca bilgi degerini hesaplamak icin yardimci ufak
;; program.
;;
;; id3.lisp'den alinmistir
;;
(setf dogru-sayisi 2) ;; dogru yanlis sayisini buraya girin
(setf yanlis-sayisi 5) ;; dogru yanlis sayisini buraya girin

(setf toplam (+ dogru-sayisi yanlis-sayisi))
(setf dogru-orani (/ dogru-sayisi toplam))
(setf yanlis-orani (/ yanlis-sayisi toplam))

(setf logp (* (* -1 dogru-orani)
	      (if (zerop dogru-orani) 0 (log dogru-orani 2))))
(setf logn (* (* -1 yanlis-orani)
	      (if (zerop yanlis-orani) 0 (log yanlis-orani 2))))
(setf bilgi-icerik (+ logp logn))

(print bilgi-icerik)