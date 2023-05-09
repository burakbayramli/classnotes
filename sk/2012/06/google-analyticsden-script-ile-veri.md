# Google Analytics'den Script ile Veri Almak

Bilindigi gibi bir sitenin kullanim verilerini GA'ye kaydettirmek
mumkun. GA siteniz hakkinda bir suru degerli bilgiyi kayit edecek, ve
bir admin araci uzerinden sunacak. Eger bu bilgileri, en azindan
gunluk bazinda ozetlenmis bir kismini, kendi sisteminize ham veri
olarak almak istiyorsaniz, GA'nin Python ile baglanilacak bir arayuzu
var.

http://code.google.com/p/gdata-python-client/

Kurduktan sonra gdata-2.0.17/samples/analytics altina bakin, burada
data_feed_demo.py var. Bu kod komut satirindan elle kullanici, sifre
falan istiyor, tam bitmis halde degil. Bu kodun __init__ kismini soyle
degistirin:

```
  def
__init__(self):
     SOURCE_APP_NAME = 'Google-dataFeedDemoPython-v2'
    my_client = gdata.analytics.client.AnalyticsClient(source=SOURCE_APP_NAME)
    # your pass needs to be in $HOME/pass file
    password = open("%s/pass" % os.environ['HOME']).read()
   
    try:
      my_client.client_login("[KULLANICI]",password,SOURCE_APP_NAME,
                             service='analytics')
     
    except gdata.client.BadAuthentication:
      exit('Invalid user credentials given. Please your password in $HOME/pass file')
    except gdata.client.Error:
      exit('Login Error')
    table_id = "ga:[TABLO ID]"
```

[TABLO ID] nedir? GA admin aracina girince URL'e bakin, p23234 gibi bir kisim var, p'den sonra gordugunuz tum sayilari alip [TABLO ID] icin kullanabilirsiniz. 

Ustteki degisim sonrasi sifreniz $HOME/pass adli bir dosyadan geliyor olacak, boylece koda gommeniz gerekmez. 

Ornegin geri kalaninda ga:visits verisinin alindigi gorulur, bu sadece ziyaret verisi, daha fazlasi da var. Tam liste icin suraya bakiniz.

Bir degisik ornek,

```
import numpy as npimport gdata.analytics.clientimport gdata.sample_utilimport osSOURCE_APP_NAME = 'Google-dataFeedDemoPython-v2'my_client = gdata.analytics.client.AnalyticsClient(source=SOURCE_APP_NAME)
# your pass needs to be in $HOME/pass file
password = open("%s/pass" % os.environ['HOME']).read()
    try:
  my_client.client_login("[KULLANICI]",password,SOURCE_APP_NAME,
                         service='analytics')  except gdata.client.BadAuthentication:
  exit('Invalid user credentials given. Please your password in $HOME/pass file')
except gdata.client.Error:
  exit('Login Error')table_id = "[TABLO ID]"# DataFeedQuery simplifies constructing API queries and uri encodes params.data_query = gdata.analytics.client.DataFeedQuery({
    'ids': table_id,
    'start-date': '2012-04-01',
    'end-date': '2012-06-24',
    'dimensions': 'ga:date',
    'metrics': 'ga:visits,ga:newVisits,ga:percentNewVisits,ga:visitors,ga:bounces,ga:timeOnSite',
    'sort': 'ga:date',
    'max-results': '10000'})feed = my_client.GetDataFeed(data_query)
data = []for entry in feed.entry:  line = []
  for dim in entry.dimension:
    line.append( int(dim.value.replace("-",""))  )
  for met in entry.metric:
    line.append(float(met.value))
  data.append(line)
  data = np.array(data)np.savetxt("foo.csv", data, delimiter=",", fmt='%10.2f')
```

Bu kodla gunluk bazda bir suru olcum noktasini aliyoruz, kolonlar
olarak bir matris icine yerlestiriyoruz, ve csv dosyasi olarak diske
yaziyoruz.





