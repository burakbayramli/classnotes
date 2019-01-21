# Otomatik System.out üretmek


Otomatik System.out üretmek



 Java programlarınızda hata ararken, bazen birkaç değişkeni ekrana basıp uygulamada neler olduğunu anlamamız gerekebiliyor. Bunun için, geçici bir şekilde olmak üzere kodumuzun içine System.out.println ibareleri koyabiliriz.               Not: Kalıcı olarak bilmemiz gereken ve gösterilecek değişkenler/mesajlar için, günlük (log) bazlı bir yaklaşımı kullanmamız daha yerinde olur. Mesela Apache Log4J projesi, bu şekilde bir yaklaşımdır. Kalıcı günlük mesajlarını Log4J kullanarak dosyaya, hâtta başka bir bilgisayarda çalışan Unix syslog programına bile aktarabilirsiniz.              Bu yazımızda sadece, geçici türden mesajları Emacs içinden basit bir tuş hareketi ile üretmeyi göstereceğiz.               Sürekli tekrar ettiğiniz başka kod parçaları var ise, Emacs'in tempo adlı paketini kullanarak onları da basit tuş kısa yolu ile tanımlayabilirsiniz.                Aşağıdaki kod, Control C ve Control w tuşlarına basarak                System.out.println("degisken" + degisken);              şeklinde bir ibare üretmeyi gösteriyor. Ayrıca, Control C ve Control e tuşları ile değişkensiz tek mesaj üretmek mümkün.              Ekte gösterilen LISP kodunu .emacs (ya da _emacs) dosyanız içine koyabilirsiniz.                ;; ...;; ...;; kod uretimi icin gerekli olan paketi hazir et(setq tempo-interactive t);; degisken girince, System.out.println("degisken" + degisken);; uretir(tempo-define-template"degisken-goster" ;; sablon ismi'("System.out.println(\""      (p "Gosterilecek degiskeni girin: " degisken) ;; kullaniciya degisken sor"=\" + "(s degisken)");")"d" ;; kisaltma"Otomatik hata mesaji ve degiskeni uretir") ;; sablon belgesi(tempo-define-template"goster" ;; sablon ismi'("System.out.println(\""      (p "Gosterilecek mesaji girin: ") ;; kullaniciya mesaj sor"\");")"d" ;; kisaltma"Otomatik hata mesaji uretir") ;; sablon belgesi(global-set-key "\C-c\C-w" degisken-goster);; otomatik system.out uret(global-set-key "\C-c\C-e" goster)




