# Bekleme İşareti (ProgressDialog)

Android programımız başlarken, ya da daha sonraki ekranlarda çok zaman
alabilecek bir işlem yaparken kullanıcıya bir tür bekleme mesajı
göstermek iyi olur (yoksa kullanıcı programın kitlendiğini,
çalışmadığını zannedebilir). Eğer Activity class'ımız önCreate metotu
içinde bir ProgressDialog yarattıysak, bu dialoğun iş işten geçtikten
sonra çalıştığını görürdük. Sebep, UI thread yapısında gizli, özet
olarak ekrandaki görüntüler önCreate bitmeden ekranda resmedilmiyor;
bizim daha önce çalışmasını beklediğimiz ProgressDialog dahil.Burada
kullanılan numara, şu olacak. önCreate sadece ProgressDialog ve
(işlemini beklemediğiniz) türden bazı GUİ elementlerini yaratacak, ve
hemen arkasından bir Thread üzerinde çok zaman alacak o işi
başlatacak. Thread'in elinde bizim ProgressDialog, UI erişimi olacak,
bu işlem bitince ProgressDialog'u görüntüden kaldıracak.

```
public class MyActivity extends Activity implements Runnable {

   private ProgressDialog pd;
   @Override
   public void onCreate(Bundle savedInstanceState) {
      super.onCreate(savedInstanceState);
      setContentView(R.layout.main);
      pd = ProgressDialog.show(this, "Working..", "Preparing Database", true, false);
      Thread thread = new Thread(this); thread.start();
   }

   public void run() { // uzun surecek  islem burada
      handler.sendEmptyMessage(0);}private Handler handler = new Handler() {
         @Override public void handleMessage(Message msg) {
              pd.dismiss(); // diger gui elementleri burada yaratilabilir }
      };
}
```
