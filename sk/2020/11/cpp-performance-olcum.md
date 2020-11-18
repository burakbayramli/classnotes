# C++ Programlarının Performansını Ölçmek

Kodun içinde hangi bloğun ne kadar zaman aldığını ölçmek kodu optimize
etmek için gündeme gelir. Altta bir kronometre kodu var, [1]'de
görülen hali alttaki sınıfla bir kronometre objesi yaratmak ve onu
başlatıp durdurduktan sonra ne kadar zaman geçtiğini raporlamak.

Bizim yaptığımız ek birden fazla başlatıp durdurabilmek, böylece bir
fonksiyon pek çok kez çağrılıyorsa mesela o fonksiyon başında sonunda
sürekli başlatıp durdurma sonrası toplam o fonksiyonda ne kadar zaman
harcandığını ölçebilmek. 

Ya farklı kod bloklarını ölçmek istersek? O zaman alttaki sınıftan her
blok için ayrı obje yaratabiliriz böylece o her bloğun zaman kaydı
ayrı tutulur.


```clike
#include <iostream>
#include <chrono>
#include <ctime>
#include <cmath>

class Timer
{

    double totalElapsed = 0.f;

private:
    std::chrono::time_point<std::chrono::system_clock> m_StartTime;
    std::chrono::time_point<std::chrono::system_clock> m_EndTime;
    bool                                               m_bRunning = false;
    
public:
    void start()
    {
        m_StartTime = std::chrono::system_clock::now();
        m_bRunning = true;
    }
    
    void stop()
    {
        m_EndTime = std::chrono::system_clock::now();
        m_bRunning = false;
	totalElapsed = totalElapsed + std::chrono::duration_cast<std::chrono::milliseconds>(m_EndTime - m_StartTime).count();
    }
    
    double elapsedMilliseconds()
    {        
        return totalElapsed;
    }
    
    double elapsedSeconds()
    {
        return elapsedMilliseconds() / 1000.0;
    }

};

```

```clike
#include "cronometer.h"

long fibonacci(unsigned n)
{
    if (n < 2) return n;
    return fibonacci(n-1) + fibonacci(n-2);
}

int main()
{    
    Timer timer;
    timer.start();
    for (int i=0;i<100;i++){
	fibonacci(30);
    }
    timer.stop();
    std::cout << "Milliseconds: " << timer.elapsedMilliseconds() << std::endl;
    timer.start();
    for (int i=0;i<100;i++){
	fibonacci(30);
    }
    timer.stop();
    
    std::cout << "Milliseconds: " << timer.elapsedMilliseconds() << std::endl;
}
```

```
g++ test.cpp  -o /tmp/a.exe; /tmp/a.exe
```




Kaynaklar

[1] https://gist.github.com/mcleary/b0bf4fa88830ff7c882d


