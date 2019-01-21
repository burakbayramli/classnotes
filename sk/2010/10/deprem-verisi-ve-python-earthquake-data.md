# Deprem Verisi ve Python (Earthquake Data)


Deprem Verisi ve Python (Earthquake Data)




USGS sitesinden aldigimiz centennial (yuz yillik) deprem verisini biraz temizledik, onemli kolonlari sectik ve Python Numpy loadtxt ile yuklenebilecek hale getirdik. Bu veriyi ve onu okuyan ornek Python script'i ekteki zip dosyasinda bulabilirsiniz. Ornek olarak iki veri noktasini ekrana basiyoruz: 1999 Izmit ve 1906 San Fransisco depremleri. Ornek komutlar soyle:
print q[(q[:,0] == 1999) & (q[:,1] == 8) & (q[:,2] == 17)]

print q[(q[:,0] == 1906) & (q[:,1] == 4) & (q[:,2] == 18)]
English: You can find a scrubbed version of the centennial earthquake data (from USGS site) shared below. We also included a sample Python script that shows how to filter based on date. As an example we display two events, one on 1999 Turkey, the other 1906 San Fransisco.

Dosyalar




