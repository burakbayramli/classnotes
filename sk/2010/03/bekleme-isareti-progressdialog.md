# Bekleme Isareti (ProgressDialog)

Android programimiz baslarken, ya da daha sonraki ekranlarda cok zaman
alabilecek bir islem yaparken kullaniciya bir tur bekleme mesaji
gostermek iyi olur (yoksa kullanici programin kitlendigini,
calismadigini zannedebilir). Eger Activity class'imiz onCreate metotu
icinde bir ProgressDialog yarattiysak, bu dialogun is isten gectikten
sonra calistigini gorurduk. Sebep, UI thread yapisinda gizli, ozet
olarak ekrandaki goruntuler onCreate bitmeden ekranda resmedilmiyor;
bizim daha once calismasini bekledigimiz ProgressDialog dahil.Burada
kullanilan numara, su olacak. onCreate sadece ProgressDialog ve
(islemini beklemediginiz) turden bazi GUI elementlerini yaratacak, ve
hemen arkasindan bir Thread uzerinde cok zaman alacak o isi
baslatacak. Thread'in elinde bizim ProgressDialog, UI erisimi olacak,
bu islem bitince ProgressDialog'u goruntuden kaldiracak.public class
MyActivity extends Activity implements Runnable {private
ProgressDialog pd;@Overridepublic void onCreate(Bundle
savedInstanceState) { super.onCreate(savedInstanceState);
setContentView(R.layout.main); pd = ProgressDialog.show(this,
"Working..", "Preparing Database", true, false); Thread thread = new
Thread(this); thread.start();}public void run() { // uzun surecek
islem burada handler.sendEmptyMessage(0);}private Handler handler =
new Handler() { @Override public void handleMessage(Message msg) {
pd.dismiss(); // diger gui elementleri burada yaratilabilir } };}





