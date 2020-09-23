;;
;; Ilk kolon, her satir icin kimlik gorevini yapiyor. Yani, d1'i
;; kullanarak d1 ile baslayan tum veri satirina ulasmak mumkun.
;; Python kodundaki veri farkli olarak ikinci kolondaki etiket degerini
;; en sona atiyor
;; 
;; LISP dilinde bu isi gerceklestirebilmek icin, veri satirindaki bilgileri
;; anahtar-deger deger cifti olarak kimlik kolonu 'uzerinde' sakliyoruz.
;; LISP komutlarindan 'get', bu isi goruyor. 
;;
(setf *egitim-verisi*
      '(
	(d1 EVET EVET HAYIR HAYIR EVET BIRAZ DDD HAYIR EVET FRANSIZ T0)
	(d2 HAYIR EVET HAYIR HAYIR EVET DOLU D HAYIR HAYIR TAYLAND T30)
	(d3 EVET HAYIR EVET HAYIR HAYIR BIRAZ D HAYIR HAYIR KEBAP T0)
	(d4 EVET EVET HAYIR EVET EVET DOLU D EVET HAYIR TAYLAND T10)
	(d5 HAYIR EVET HAYIR EVET HAYIR DOLU DDD HAYIR EVET FRANSIZ T60)
	(d6 EVET HAYIR EVET HAYIR EVET BIRAZ DD EVET EVET ITALYAN T0)
	(d7 HAYIR HAYIR EVET HAYIR HAYIR HIC D EVET HAYIR KEBAP T0)
	(d8 EVET HAYIR HAYIR HAYIR EVET BIRAZ DD EVET EVET TAYLAND T0)
	(d9 HAYIR HAYIR EVET EVET HAYIR DOLU D EVET HAYIR KEBAP T60)
	(d10 HAYIR EVET EVET EVET EVET DOLU DDD HAYIR EVET ITALYAN T10)
	(d11 HAYIR HAYIR HAYIR HAYIR HAYIR HIC D HAYIR HAYIR TAYLAND T0)
	(d12 EVET EVET EVET EVET EVET DOLU D HAYIR HAYIR KEBAP T30)
	))

(setf *dogru-sayisi* 0)
(setf *yanlis-sayisi* 0)
(setf *toplam-veri-sayisi* 0)

(setf *basliklar*
      '(BEKLEYELIMMI
	BASKA
	BAR
	HAFTASONU
	ACMIYIZ
	MUSTERILER
	FIYAT
	YAGMUR
	RESERVASYON
	YEMEKTURU
	BEKLEMESURESI))

(defun deger-koy (baslik satir deger)
  (setf (get satir baslik) deger))

(defun deger-al (baslik satir)
  (get satir baslik))

;; uzerinde irdeleme yaptigimiz kolon degerini bulup geri
;; getirir. 
(defun hedef-baslik-degeri (satir)
  (get satir 'BEKLEYELIMMI))

(defun hedef-baslik () (return 'BEKLEYELIMMI))

(defun veriyi-satirkimligine-cevir (ornekler)
  (loop for satir in ornekler collect
	(car satir)))

;; verileri olusturan butun satirlarin irdeleme sonucu ayni mi?
;; yani, verilen satilarinin hepsinin 'bekleyelimmi ozelligi
;; ayni cevabi mi tasiyor?
(defun ayni-cevap? (satirlar)
  ( let ((sonuc nil))
    (setq ilkdeger (hedef-baslik-degeri (car satirlar)))
    (setf sonuc (every #'(lambda(e)
			   (equal ilkdeger (hedef-baslik-degeri e)))
		       (cdr satirlar))
	  ) sonuc ))
   
;; bu islevin, program basladiktan hemen sonra cagrilmasi
;; gerekiyor. anahtar/deger bilgilerini bu fonksiyon yaratip,
;; kimlik sembolu uzerine koyuyor
(defun egitim-verilerini-cevir (veri basliklar)
  (loop for d in veri do
	(loop for baslik in basliklar
	      as deger in (cdr d)
	      do
	      (setf kimlik-no (first d))
	      (deger-koy baslik kimlik-no deger)
	      ))

  (loop for d in veri do
	(if (equal (hedef-baslik-degeri (car d)) 'EVET) (incf *dogru-sayisi*))
	(if (equal (hedef-baslik-degeri (car d)) 'EVET) (incf *yanlis-sayisi*))
	)

  (setf *toplam-veri-sayisi* (+ *dogru-sayisi* *yanlis-sayisi*))
  )

;;
;;
;; Iste Algoritma
;;
;;
(defun karar-agaci-egit (ornekler basliklar)
  (let ((sonuc nil))
    ;; ornekler, butun egitim verisini olusturur
    (cond
     ((equal basliklar nil)
      (setf sonuc (encok-gorulen-deger ornekler)))
      
     ;; butun satirlarin klasmani ayni ise, bulunan bu klasmani getir
     ;; hepsi ayni ise, herhangi birinin klasmani yeter
     ((ayni-cevap? ornekler)
      (setf sonuc (hedef-baslik-degeri (car ornekler))))

     ;; Burada, parca listelerinin listesini olustur. Bu kocaman liste
     ;; elimizdeki veriyi her basligi kullanrak bolmus ve biraraya
     ;; konulmus bir halidir. parca-sec islevi, girdisini boyle bekliyor.
     (t (progn
	  (setq parca-listenin-listesi
		(loop for baslik in basliklar collect (parcala ornekler baslik)))
	
	  (setf eniyi (parca-sec parca-listenin-listesi))

	  (setf sonuc (cons (car eniyi)
			    (loop for dal in (cdr eniyi) collect
				  (list (car dal)
					(karar-agaci-egit
					 (cdr dal)
					 (remove (car eniyi) basliklar))
					))
			    ))
	  ))
     
     ) sonuc ))



(defun kazanc (parca-listesi)
  (let ((kazanc 0))

    ;; Burada ufak bir numaraya dikkat. Gecici bir sekilde, parcalari
    ;; "butun" tek parcaya topluyoruz ki bolunmeden onceki bilgi
    ;; icerigini hesaplayabilelim.
    (setf birlesim (reduce #'append (cdr parca-listesi)))

    (setf ust-bilgi-icerigi
	  (parca-bilgi-icerigi birlesim))

    (setf cocuklarin-bilgi-icerigi (bilgi-icerigi parca-listesi))

    (setf kazanc (- ust-bilgi-icerigi cocuklarin-bilgi-icerigi))
    kazanc ))

(defun bilgi-icerigi (parca-listesi)
  ;; her parcanin bilgi icerigini hesaplayip bu degerleri topla
  (let ((toplam 0))
    (dolist (parca (cdr parca-listesi))	;; cdr komutu baslik kismini kesip atiyor
      (incf toplam
	    (parca-bilgi-icerigi parca))
      ) toplam ))

(defun encok-gorulen-deger (baslik satirlar)
  (let ((enuzun nil))
    (loop for p in (parcala baslik satirlar) do
          (when (> (length p) length)
            (setq length (length p))
            (setq enuzun p)))
    (car enuzun)))

(defun parca-bilgi-icerigi (parca)
  ;; bu parca icindeki dogru ve yanlis satirlari say. Dogru
  ;; ve yanlis 'hedef basligina' gore bulunuyor tabii
  (let ((dogru-sayisi 0)(yanlis-sayisi 0))
    (dolist (kimlik-no parca)
      (cond
       ;; eger satir BEKLEYELIMMI=EVET ise
       ((and (member kimlik-no parca)
	     (equal (hedef-baslik-degeri kimlik-no) 'EVET))
	(incf dogru-sayisi))
       ;; eger satir BEKLEYELIMMI=HAYIR ise
       ((and (member kimlik-no parca)
	     (equal (hedef-baslik-degeri kimlik-no) 'HAYIR))
	(incf yanlis-sayisi))
       (t nil))
      )

    (setf toplam (+ dogru-sayisi yanlis-sayisi))
    (setf dogru-orani (/ dogru-sayisi toplam))
    (setf yanlis-orani (/ yanlis-sayisi toplam))

    ;; Asagida gorulan (zerop ..) kullanimi guzel bir LISP numarasi.
    ;; Eger dogru-orani 0 ise, hesabin geri kalani icin 0 kullan.
    ;; Fakat (zerop xx) 0 ise, yani xx 0 degil ise :), o zaman
    ;; log hesabini yap. Vay anasini.
    ;; Bu numaradan once 'sifirla bolunme (division by zero)' hatasi
    ;; aliyordum. log 0 hesap edilir bir deger degil demek ki,
    ;; tanim olarak 0 oldugu kabul ediliyor. Sinifta hoca da oyle
    ;; soylemisti.
    (setf logp (* (* -1 dogru-orani)
		  (if (zerop dogru-orani) 0 (log dogru-orani 2))))
    (setf logn (* (* -1 yanlis-orani)
		  (if (zerop yanlis-orani) 0 (log yanlis-orani 2))))

    (setf log-toplam (+ logp logn))

    (setf butune-olan-d-y-orani (/ toplam *toplam-veri-sayisi*))
    
    (setf bilgi-icerik (* log-toplam butune-olan-d-y-orani))

    bilgi-icerik ))

;;
;; veriyi bolmek icin en iyi basligi bul
(defun parca-sec (parca-listenin-listesi)
  ;;
  ;;
  (let ((sonuc (car parca-listenin-listesi)))    
    (dolist (parca-listesi parca-listenin-listesi)
      (if (> (kazanc parca-listesi)
	     (kazanc sonuc))
	  (setf sonuc parca-listesi)))
    sonuc ))


;; karar agacina bunun ile soru sorabilirsin.
(defun soru-sor (satir agac)
  (let (deger dal)
    (if (atom agac) (return-from soru-sor agac))
    (setf deger (deger-al (car agac) satir))
    (setf dal (second (assoc deger (cdr agac))))
    (soru-sor satir dal)))

;;
;; Verilen basliga gore veriye bakar, basligin altindaki verinin
;; tekabul eden degerine gore guruplama yapip, veriyi parcalara ayirir
(defun parcala (satirlar baslik)
  (let ((gecici-liste ())(e nil)(kimlik-no nil)(bulunanlar-sayisi 0)(iteration 0))
    
    (dolist (kimlik-no satirlar)

      (setf dongu 0)
      (setf bulunanlar-sayisi -1)
	  
      ;; eger konol degeri zaten mevcut ise, kimlik-no'yi bu alt
      ;; listeye ekle
      (dolist (su-anki-parca gecici-liste)
	;; su anki parcanin ilk satirina bakmak yeterli, cunku
	;; otekilerinde degeri ayni olacak
	(setf su-anki-ornek-deger (car su-anki-parca))

	;; degerler ayni ise
	(if (equal su-anki-ornek-deger (deger-al baslik kimlik-no))
	    (progn
	      ;; demekki satir bu parcaya ait. ekle.
	      (setf bulunanlar-sayisi dongu)
	      (return)
	      )
	  )
	(incf dongu)
	
	) ;; dolist sonu

      (if (> bulunanlar-sayisi -1)
	  (progn
	    ;; buraya dikkat edin; bir liste icerigini degil, gostergecini
	    ;; (pointer) degistiriyoruz. Nth'in geri getirdigi, normal
	    ;; deger degil, gostergec degeri. Yeni listenin gostergecini bu deger
	    ;; uzerine yazinca, eski liste kaybolmus oluyor.
	    ;; Yeni liste bir oncekinin bir fazlasi aslinda..
	    (setf (nth bulunanlar-sayisi gecici-liste)
		  (append (nth bulunanlar-sayisi gecici-liste) (list kimlik-no)))
	    ))

      ;; yoksa, yeni bir alt-liste baslat, ve gecici-listeye ekle
      (if (equal bulunanlar-sayisi -1)
	  (progn
	    (setf gecici-liste
		  (append gecici-liste
			  (list (list (deger-al baslik kimlik-no) kimlik-no))))
	    ))
	  
      )

    ;; baslik degerini listenin onune koy
    (setf gecici-liste (append (list baslik) gecici-liste))
    
    gecici-liste
    ))
                  

(defun agac-goster (agac &optional (derinlik 0))
  (tab derinlik)
  (format t "~A~%" (first agac))
  (loop for alt-agac in (cdr agac) do
        (tab (+ derinlik 1))
        (format t "= ~A" (first alt-agac))
        (if (atom (second alt-agac))
	    (format t " => ~A~%" (second alt-agac))
          (progn (terpri)(agac-goster (second alt-agac) (+ derinlik 5))))))

(defun tab (n)
  (loop for i from 1 to n do (format t " ")))



;; bu satiri silme
(egitim-verilerini-cevir *egitim-verisi* *basliklar*)

;;
;; testler

;;
;; test degerlendiren fonksiyon
(defun test (isim deyim sonuc)
  (cond
   ((equal deyim sonuc) t)
   (t (print isim) (error "HATA! Birim Testler Hata Yakaladi! "))  
   ))

;; her sembolun bir ozellik listesi var
(test "ozellik listesi bos olan sembol" (get 'ornek-sembol 'baharatlar) NIL)

;; her sembolun bir ozellik listesi var
(setf (get 'ornek-sembol 'baharatlar) 'tuz) 
(test "ornek sembole baska bir deger ekle" (get 'ornek-sembol 'baharatlar) 'tuz)

;; her sembolun bir ozellik listesi var
(setf (get 'ornek-sembol 'tatlilar) 'baklava) 
(test "degisik bir sembole birsey ekle"
      (get 'ornek-sembol 'tatlilar) 'baklava)

;; baslik degerini eriselim
(test "tablo dogru kuruldu" (hedef-baslik-degeri 'd1) 'EVET)

;; yemekturu uzerinde parcalara ayiralim
(setf beklenen-parcalar '(YEMEKTURU (FRANSIZ D1 D5)
				    (TAYLAND D2 D4 D8 D11)
				    (KEBAP D3 D7 D9 D12)
				    (ITALYAN D6 D10)))
(test "parcala (yemekturu)"
      (parcala (veriyi-satirkimligine-cevir *egitim-verisi*) 'YEMEKTURU)
      beklenen-parcalar)

;; musteriler uzerinde parcalara ayiralim
(setf beklenen-parcalar '(MUSTERILER (BIRAZ D1 D3 D6 D8)
				     (DOLU D2 D4 D5 D9 D10 D12)
				     (HIC D7 D11)))
(test "musteriler uzerinden parcala"
     (parcala (veriyi-satirkimligine-cevir *egitim-verisi*) 'musteriler)
     beklenen-parcalar)

;; bilgi icerigi
(setf *toplam-veri-sayisi* 12)
(test "bilgi icerigi" (parca-bilgi-icerigi
		      (veriyi-satirkimligine-cevir *egitim-verisi*))
     1)

;; eniyi parcayi bul, MUSTERILER basliginin secilmesi lazim.
(setf yemekturu-parcalari
     '(TYPE
	(FRANSIZ D1 D5)
	(TAYLAND D2 D4 D8 D11)
	(KEBAP D3 D7 D9 D12)
	(ITALYAN D6 D10)))
(setf musteri-parcalari
     '(patrons
	(BIRAZ D1 D3 D6 D8)
	(TAYLAND D2 D4 D5 D9 D10 D12)
	(HIC D7 D11)))
(setf ornek-girdi (list yemekturu-parcalari musteri-parcalari))
(test "parca-sec"
     (parca-sec ornek-girdi) musteri-parcalari)

;; eniyi parcalamayi sec, MUSTERILER basliginin secilmesi lazim
(setf girdi '(D1 D3))
(test "ayni cevap 1" (ayni-cevap? girdi) t)

(setf girdi '(D2 D4))
(test "ayni cevap 2" (ayni-cevap? girdi) nil)

;; ana veriyi, kimlik no'lara cevir
(test "kimlik no ya ceviri testi" (veriyi-satirkimligine-cevir *egitim-verisi*)
     '(D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12))

(setf agac (karar-agaci-egit (veriyi-satirkimligine-cevir *egitim-verisi*)
			     '(BASKA
			       BAR
			       HAFTASONU
			       ACMIYIZ
			       MUSTERILER
			       FIYAT
			       YAGMUR
			       RESERVASYON
			       YEMEKTURU
			       BEKLEMESURESI)			     
			     ))

(agac-goster agac)

;; egitim verisinden bir satir kullanip soru sor
(test "soru-sor 1" (soru-sor 'd6 agac) 'EVET)
(test "soru-sor 2" (soru-sor 'd2 agac) 'HAYIR)
(test "soru-sor 3" (soru-sor 'd3 agac) 'EVET)
(test "soru-sor 4" (soru-sor 'd4 agac) 'EVET)
(test "soru-sor 5" (soru-sor 'd5 agac) 'HAYIR)
(test "soru-sor 6" (soru-sor 'd6 agac) 'EVET)
(test "soru-sor 7" (soru-sor 'd7 agac) 'HAYIR)
(test "soru-sor 8" (soru-sor 'd8 agac) 'EVET)
(test "soru-sor 9" (soru-sor 'd9 agac) 'HAYIR)
(test "soru-sor 10" (soru-sor 'd10 agac) 'HAYIR)
(test "soru-sor 11" (soru-sor 'd11 agac) 'HAYIR)
(test "soru-sor 12" (soru-sor 'd12 agac) 'EVET)

(print "Tamam. Birim Testler Isledi.")
