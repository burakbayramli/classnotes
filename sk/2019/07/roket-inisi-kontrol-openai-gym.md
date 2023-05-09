# Roket İnişi, Kontrol, OpenAI Gym

OpenAİ Gym altyapısı altında paylaşılan simülatörlerden biri aya iniş
(lunar lander), bir diğeri, yine Gym altyapısı üzerinde yazılmış ama
ayrı bir proje, rocket-lander. Her iki proje her ne kadar takviyeli
öğrenim / yapay zeka için yazılmış olsa da, (matematiksel modelleme
içeren) kontrol teorisi amaçlı kullanılabilir. İniş problemi kolay
değil; rüzgar, yerçekim etkileriyle boğuşurken en az yakıt harcayip ve
yumuşak şekilde iniş yapmak ciddi bir optimizasyon problemi.

<img width="400" src="https://1.bp.blogspot.com/-OIxLXQe2s7U/XSwvOIA8XQI/AAAAAAAAB1M/WNK70QseuswPtK7E7Zn1ogVhzqSGrE44ACLcBGAs/s1600/rocket-0100.png"/>

İkinci iniş simülasyonu birinciyi kıyasla daha çetrefil bir iniş
modelliyor, geçende deniz üstünde bir platforma iniş yapan SpaceX'in
Falcon 9 ilk aşama roketi modellenmiş. Ufak bir hedefe iniş yapıldığı
için iş çok daha zor, ayrıca bu simülatörde kontroller sürekli
(continuous) olabiliyor, aya iniş ayrıksal kontrolle işliyor.

Aya iniş için `gym==0.9.4` kurulur, altta rasgele hareketler yaptıran
bir kod (ayrıca simülasyondan resim alınıyor),

```python
import gym, six, pyglet, pandas as pd, numpy as np
import matplotlib.pyplot as plt

env = gym.make('LunarLander-v2')

observation = env.reset() 

for i in range(100):
    action = env.action_space.sample() 
    env.render()
    observation, reward, done, info = env.step(action)

    buffer = pyglet.image.get_buffer_manager().get_color_buffer()            
    image_data = buffer.get_image_data()
    if i % 10 == 0:
        image_data.save(filename='frames/lunar-%04d.png' % i)
    
    print (observation)
    if done: break
    
env.close()
```

Falcon 9 için ana proje

[https://github.com/arex18/rocket-lander](https://github.com/arex18/rocket-lander)

Ama tüm bu kodlara gerek yok, simülatör için iki dosya yeterli
(`rocketlander.py` ve `constants.py`), bu dosyaları biz aldık, ve
alttaki dizinde bulunabilir ,

[https://github.com/burakbayramli/kod/tree/master/sk/2019/07](https://github.com/burakbayramli/kod/tree/master/sk/2019/07)

Kontrol için taslak bir kod (sabit kontrol veriliyor). Kodda mesela
rüzgar etkisi nasıl eklenir görüyoruz,
`env.apply_random_x_disturbance` ile. İniş yapılan platform sanal bir
dalga sebebiyle sola ya da sağa kayıyor olabilir, bunu
`env.move_barge_randomly` ile veriyoruz.

```python
from rocketlander import RocketLander
from constants import LEFT_GROUND_CONTACT, RIGHT_GROUND_CONTACT
import numpy as np
import pyglet

if __name__ == "__main__":
    settings = {'Side Engines': True,
                'Clouds': True,
                'Vectorized Nozzle': True,
                'Starting Y-Pos Constant': 1,
                'Initial Force': 'random'}  # (6000, -10000)}

    env = RocketLander(settings)
    s = env.reset()
    
    left_or_right_barge_movement = np.random.randint(0, 2)
    
    for i in range(30):

            a = [10.0, 1.0, 1.0]                        
            obs, r, done, info = env.step(a)
            env.render()
            buffer = pyglet.image.get_buffer_manager().get_color_buffer()            
            image_data = buffer.get_image_data()
            if i % 10 == 0:
                image_data.save(filename='frames/rocket-%04d.png' % i)
            env.draw_marker(env.landing_coordinates[0], env.landing_coordinates[1])
            env.refresh(render=False)

            if s[LEFT_GROUND_CONTACT] == 0 and s[RIGHT_GROUND_CONTACT] == 0:
                env.move_barge_randomly
                (0.05, left_or_right_barge_movement)

                env.apply_random_x_disturbance \
                (epsilon=0.005, \
                 left_or_right=left_or_right_barge_movement)
                env.apply_random_y_disturbance(epsilon=0.005)
            if done: break
```

`step` çağrısından gelen `obs` içinde o adımda bilinen roket hakkında
ölçümler var. Konumda

```
x_pos, y_pos, x_vel, y_vel, lateral_angle, angular_velocity
```

`x_pos, y_pos` roketin yatay, dikey konumu. `x_vel,y_vel` yatay, dikey
hız. `lateral_angle` roketin eğimi, `angular_velocity` açısal hız,
yani bu açının ne hızda değiştiği.

Kontrol aksiyonları `Fe,Fs,Psi`, bunlardan `Fe` ana itiş roketi, dikey
yönde. `Fs` yandaki nitrojenle çalışan iticileri kontrol ediyor. `Psi`
ile roketin açısının değiştirmek mümkün.

Diğer değişkenlerden, `env.remaining_fuel` kalan yakıtı raporlar,
`env.initial_mass` başlangıçtaki (yakıt dahil) kütle,
`env.get_landing_coordinates()` platformun yerini. Raporlanabilen tüm
değişkenleri `dir(env)` ile görebiliriz.



