# C++ Programlarının Performansını Ölçmek

Kodun içinde hangi bloğun ne kadar zaman aldığını ölçmek kodu optimize
etmek için gündeme gelir. Altta bir kronometre kodu var, [1]'de
görülen bir kronometre objesi yaratmak ve onu başlatıp durdurduktan
sonra ne kadar zaman geçtiğini raporlamak.

Bizim yaptığımız ek birden fazla başlatıp durdurabilmek, böylece bir
fonksiyon pek çok kez çağrılıyorsa mesela o fonksiyon başında sonunda
sürekli başlatıp durdurma sonrası toplam o fonksiyonda ne kadar zaman
harcandığını ölçebilmek. 

Ya farklı kod bloklarını ölçmek istersek? O zaman alttaki sınıftan her
blok için ayrı obje yaratabiliriz böylece o her bloğun zaman kaydı
ayrı tutulur.


```cpp
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
    
public:
    void start()
    {
        m_StartTime = std::chrono::system_clock::now();
    }
    
    void stop()
    {
        m_EndTime = std::chrono::system_clock::now();
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

```cpp
#include "cronometer.h"

long fibonacci(unsigned n)
{
    if (n < 2) return n;
    return fibonacci(n-1) + fibonacci(n-2);
}

int main()
{    
    Timer timer;

    // ilk kez baslat
    timer.start();
    for (int i=0;i<100;i++){
	fibonacci(30);
    }
    timer.stop();    
    std::cout << "Milliseconds: " << timer.elapsedMilliseconds() << std::endl;

    // ikinci kez baslat, onceki zamanin ustune eklenmesi lazim
    timer.start();
    for (int i=0;i<100;i++){
	fibonacci(30);
    }
    timer.stop();
    
    std::cout << "Milliseconds: " << timer.elapsedMilliseconds() << std::endl;
}
```

Derlemek ve işletmek için,

```
g++ test.cpp  -o /tmp/run.exe; /tmp/run.exe
```

Dikkat: Bir fonksiyonun başında `start()` sonunda `stop()` koyduğumuz
zaman unutmayalım, bu her zaman yeterli olmayabilir, eğer fonksiyonun
kodununun ortasında belli bazı şartlarda `return` ile direk dönüş
yapıyorsak, o zaman bu dönüş öncesi de bir `stop` eklememiz gerekir.



Kaynaklar

[1] https://gist.github.com/mcleary/b0bf4fa88830ff7c882d



