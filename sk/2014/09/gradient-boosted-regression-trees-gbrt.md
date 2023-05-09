# Gradyan Destekli Regresyon Agaclari (Gradient Boosted Regression Trees -GBRT-)

Yapay Ogrenimde agac teknigini ilerleten yaklasimlardan biri: bu arada
artik kimse tek agac egitmiyor (mesela ID3'te oldugu gibi), Rasgele
Ormanlar (Random Forests) tekniginde pek cok agac, degisik kolon
altgruplari uzerinde egitilir, ya da diger teknikler diger sekillerde
cok agacli ortamda is gorur.

GBRT ile her agac bir once egitilen agacin egitim verisindeki
etiketleri tahmin etmekteki hatasi uzerinde (on the residuals)
egitilir, yani onceki agacin hatalari bir nevi yeni etiketler haline
gelir. Bu niye yapilir? Boylece takip eden yeni agaclarin onceki
agaclarin hatalarini "duzeltebilecegi" ongorulur.

Konu hakkinda kaynaklar 

http://www.slideshare.net/DataRobot/gradient-boosted-regression-trees-in-scikitlearn

GBRT icin en iyi kod

https://github.com/tqchen/xgboost

Kod C++ ile yazilmistir ve hizli islemektedir. Bizim icin en onemli
ozelligi scipy kutuphanesinin seyrek matris formatlari ile
calisabilmesi (kiyasla scikit-learn kutuphanesindeki mesela rasgele
orman algoritmasi bunu yapmamiza izin vermiyor). Kurmak icin unzip,
xgboost altinda make yeterli. Ornek olarak
xgboost/demo/binary_classification altinda sh runexp.sh ile ornek kod
isletebilirsiniz. Kod isledikten sonra sonuc dump.raw.txt icinde, oge
isimleri ise featmap.txt icinde. Mesela

```
0
    cap-shape=bell
    i1
    cap-shape=conical
    i2
    cap-shape=convex
    i3 
   cap-shape=flat
    i..
```

Eger numerik bir oge var ise, bu oge "i" yerine "q" ile
isaretlenir. GBRT algoritmasi ikisel degerlerle oldugu gibi numerik
degerlerle de gayet guzel (ve ayni anda) calisabilir. Mesela numerik
boy ogesi secilmis ise agac mesela 160'dan kucuk olma / olmama sarti
uzerinden bir dallanma yaratmis olabilir. Bu numerik degerin hangi
noktasindan bolunecegi de dal yaratma karar mekanizmasinin bir
parcasidir.

https://github.com/tqchen/xgboost/wiki/Binary-Classification

https://archive.ics.uci.edu/ml/datasets/Mushroom

Makina ogreniminde metotlarin hiperparametreleri ile oynanir, en
optimal olani bulunmaya ugrasilir, mesela KMeans ile bu kume
sayisidir, xgbrt ile, ya da herhangi bir agac algoritmasi icin "agac
derinligi" bu parametrelerden biri. Bu derinligi bst:max_depth
parametresi kontrol eder. Bir digeri, ozellikle xgbrt icin, "kac tane
agac olacagi" parametresi (altta num_round). Ornek bazi ayarlar altta,

```
num_round = 10
param = {}
param['scale_pos_weight'] = 10.
param['bst:eta'] = 0.4
param['bst:max_depth'] = 3
param['eval_metric'] = 'auc'
param['bst:nthread'] = 4
param['silent'] = 1
bst = xgb.train( param, [egitim verisi], num_round, [....] )
```

Parametre scale_pos_weight ile eger 1 etiketi 0 etiketinden cok daha
az ise, bunu dengelemek icin 1'lerin onemini suni olarak arttirabilme
sansini elde ediyoruz.

Simdi bazi taklalara gelelim: biz bir proje icin bu kodun icinde bazi
degisiklikler yaptik. Mesela eger 1. seviyede isminde vs metni olan
ogeler olsun, 2. seviyede sunlar, 3. seviyede sunlar olsun gibi bazi
ek kurallar gerekirse, bunlari kodun icine sokusturmak mumkundur. Bu
tur kolon kurallari icin xgboost/booster/tree/xgboost_col_treemaker.hpp
icinde, tum #include ibarelerinden sonra,

```
#include <iostream>
#include <sstream>
#include <string>
#include <fstream>
#include <vector>
#include <map>

using namespace std;

typedef std::map<std::string, std::string> TStrStrMap;

typedef std::pair<std::string, std::string> TStrStrPair;

TStrStrMap create_map(){
    TStrStrMap tMap;
    ifstream infile("[DIZIN]/featmap.txt");
    string line = "";
    vector<string> all_words;
    while (getline(infile, line))
    {
        stringstream strstr(line);
        string word = "";
        int i = 0; string key; string val;
        while (getline(strstr,word, '\t'))
        {
          if (i == 0) key = word;
          if (i == 1) val = word;
          i++;
        }
        if (val.find("[filan]") != std::string::npos) {
          tMap.insert(TStrStrPair(key, "[filan]"));
        }
        else if (val.find("[falan]") != std::string::npos) {
          tMap.insert(TStrStrPair(key, "[falan]"));
        }
        else if (val.find("[fisman]") != std::string::npos) {
          tMap.insert(TStrStrPair(key, "[fisman]"));
        }
    }
    return tMap;
}
```

Daha sonra FindSplit() metodu icinde 

```
for( unsigned i = 0; i < nsize; ++ i ){
   const unsigned fid = feat_index[i];
...
```

dongusu icinde

```
if (depth == 0 && tMap[std::to_string(fid)] !=
      "[falan]") continue;
else if (depth == 1 && tMap[std::to_string(fid)] != "[filan]") continue;
...
```

eklenir. Ne yapmis olduk? Ilk once, en basta featmap.txt icindeki oge
isimlerini aldik. Bu oge isimlerini taradik, ve icinde belli bir metni
tasiyanlari bir map uzerine koyduk, bu baslangica sahiptir diye
isaretlemis olduk. Daha sonra agac algoritmasinin bolme noktasi arayan
algoritmasina kod sokusturduk, burada dedik ki, mesela ilk seviyede
sadece "falan" etiketi tasiyan kolonu bolmek icin kullan, yani agacin
bu seviyesinde sadece o tur kolonlari kullanabilirsin kurali
getirdik. 

Bu sayede diyoruz ki, mesela elde bir musteri kaydi var diyelim, ve bu
kayitlarda lokasyana ait bilgiler belli kolonlarda, finansla alakali
bilgiler diger kolonlarda (ve yine diyelim ki bu kolonlarin isminde bu
farkliligi isaretledik, "lokasyon_" ya da "finans_" gibi..), ustteki
gibi bir degisiklik ile agacin ilk seviyesi lokasyon, ikincisi finans,
ucuncusu vs. kolon tipinde olsun gibi bir yonlendirmeyi ekleyebiliriz.

Ustteki degisiklikleri derlemek icin hem ana xgboost dizini hem de
xgboost/python altindaki Makefile dosyasina girip export CFLAGS
listesine  -std=c++11 eklemeniz lazim, cunku bizim ek kodlar yeni C++
standardindaki bazi yenilikleri kullaniyor (to_string metodu gibi). 





