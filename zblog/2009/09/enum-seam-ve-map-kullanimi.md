# Enum, Seam ve Map Kullanimi


Enum, Seam ve Map Kullanimi



Enum degerleri Seam sayfalarina donduruldugunde bu degerlerin String'e otomatik olarak cevrildigini soylemistik. Peki ters yonde gidersek ne olur? Mesela elimizde, anahtari "enum tipi olan" bir HashMap var ve bu enum tiplerinden birinin String karsiligini anahtar olarak kullanarak bir degere ulasmaya ugrasiyoruz.Bu kullanim ne yazik ki biraz takla atmadan islemiyor. Seam forumlarinda biraz gezindik ve en temiz cozumun enum tipleri iceren HashMap istendiginde o anda (on the fly) onun kopyasi ama anahtari String bazli bir alternatif HashMap olusturmak ve xhtml sayfalara bunu dondurmek olduguna karar verdik. Anahtar String olunca xhtml duzgun calisacak, Java servis kodu tarafinda guclu tip bazli Enum kullanabilecegiz, sayfa tarafinda String kullanarak istedigimiz veriye erisebilecegiz. Guzel. Ornek, diyelim kienum Permission {NORMAL, ADMIN}private Map<Permission, Boolean> permissions = new HashMap<Permission, Boolean>();public Map<Permission, Boolean> getPermissions() ...public void setPermission(Map<Permission, Boolean> arg) ...public Map<String, Boolean> getStringPermissions() {  Map<String, Boolean> map = new  HashMap<String, Boolean>();  for (Permission p : permissions.keySet()) {    map.put(p.toString(), permissions.get(p));  }  return map;}Sayfa tarafinda artik #{obj.stringPermissions['ADMIN']} gibi bir kullanim mumkun olacaktir.Burada odenen ek bedel gecici olarak olusturulan yeni Map objesi, ve o objenin doldurulmasidir. Tabii bu cozum cok buyuk Map objeleri icin iyi olmaz; fakat ustteki "izinler listesi" kullanimi cogunlukla birkac tane obje icerir, performans problemi cikartmaz.




