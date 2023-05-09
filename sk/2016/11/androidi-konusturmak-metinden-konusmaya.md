# Bilgisayari Konusturmak,  Metinden Konusma (Text-to-Speech)

Python

```
pip install gtts, pyttsx

from gtts import gTTS
import os
tts = gTTS(text='Good morning', lang='en')
tts.save("good.mp3")
```

ya da 

```
import pyttsx

engine = pyttsx.init()

engine.say('Good morning.')

engine.runAndWait()
```

Not: Python 3 ile pyttsx3 gerekli.

Ustteki iki yaklasimdan gtts daha iyi fakat bu paket Internet
uzerinden Google'a eriserek ses degisimini yapiyor.

Android uygulamasi icinde cep telefonunu konusturmak icin arayuz
var. Herhangi bir Activity icinde

```
import android.speech.tts.TextToSpeech;

public class MyActivity extends Activity
{
    ...

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        ...
        t1=new TextToSpeech(getApplicationContext(), new TextToSpeech.OnInitListener() {
                @Override
                public void onInit(int status) {
                    if(status != TextToSpeech.ERROR) {
                        t1.setLanguage(Locale.UK);
                    }
                }
            });
     }
}
```

Ustteki onInit konusturma yapmadan once islemis olmali. Simdi herhangi
bir metni okutmak icin

```
String toSpeak = "How much wood could a woodchuck chuck";
t1.speak(toSpeak, TextToSpeech.QUEUE_FLUSH, null);     
```
