# Temel Fizik 3, Parçacıklar, Çarpışma, Hareket

Elastik Çarpışma (Elastic Collision)

$m_1,m_2$ kütlesine sahip $v_1,v_2$ hızında iki küre arasında mükemmel bir
elastik çarpışma olduğunu düşünelim, yani çarpışma öncesi ve sonrası enerji
kaybı yok, bu durumda, sistemin toplam momentumu da önce ve sonra aynı
olacaktır, 

$$
m_1 \vec{v}_1 + m_2 \vec{v}_2 = m_1 \vec{v}_1' + m_2 \vec{v}_2' 
$$

ki $\vec{v}_1,\vec{v}_2,\vec{v}_1',\vec{v}_2'$ hız vektörleri,
$\vec{v}_1',\vec{v}_2'$ çarpışma sonrası hız vektörleri.

![](phy_005_basics_06.png)

Eğer momentum muhafaza ediliyorsa, birinci topun kaybettiği ya da kazandığı
momentum ikinci topa eklenecek ya da ondan çıkartılacaktır.

$$
m_1 \vec{v}_1 = m_1 \vec{v}_1' - \Delta \vec{p}
$$

$$
m_2 \vec{v}_2 = m_2 \vec{v}_2' + \Delta \vec{p}
$$

Üstteki idealize ortamda momentum transferi sadece çarpışma çizgisi üzerinde
olabilir, bu çizgi, ya da vektör yönü eğer iki topun arasında teğet bir düzlem
düşünsek ona dik olan bir vektör olacaktır, ona $n$ diyelim. O zaman, ve $p$
vektörünün büyüklüğünü $P$ ile gösterirsek,

$$
\vec{v}_1' = \vec{v}_1 - (P / m_1) \vec{n}
\qquad (1)
$$

$$
\vec{v}_2' = \vec{v}_2 + (P / m_2) \vec{n}
\qquad (2)
$$

Eğer $P$ skalar büyüklüğünü bulabilirsek, çarpışma sonrası yeni hızı elde
edebiliriz. 

Üstteki resme bakınca görüyoruz ki $v_1$ ve $v_2$ her biri iki tane ayrı
vektörün toplamı olarak temsil edilebilir, bu vektörlerden biri çarpışma,
momentum transfer çizgisine dik, diğeri ona paralel. Bu bilgi ile,
$v_1,v_1',v_2,v_2'$ şöyle temsil edilebilir,

$$
\vec{v}_1 = a_1 \vec{n} + b_1 \vec{q}, \qquad \vec{v}_2 = a_2 \vec{n} + b_2 \vec{q}
\qquad (3)
$$

$$
\vec{v}_1' = a_1' \vec{n} + b_1' \vec{q}, \qquad v_2' = a_2' \vec{n} + b_2' \vec{q}
\qquad (4)
$$

$a_1,a_2,b_1,b_2$ tek sayı değerleridir. 

(1) formülüne (3a)'yı sokarsak,

$$
v_1' = a_1 \vec{n} + b_1 \vec{q} - (P/m_1) \vec{n}
$$

$$
 = (a_1 - p/m_1) \vec{n} + b_1 \vec{q}
$$


$$
v_2' = a_2 \vec{n} + b_2 \vec{q} + (P/m_2) \vec{n}
$$

$$
= (a_2 + P/m_2) \vec{n} + b_2 \vec{q}
$$

Ve tabii ki form olarak $\vec{v}_1' = a_1' \vec{n} + b_1' \vec{q}$, ve
$\vec{v}_2' = a_2' \vec{n} + b_2' \vec{q}$ olduğunu biliyoruz, o zaman birbirine
tekabül eden kısımlara bakarak

$$
a_1' = a_1 - (P/m_1), \qquad b_1' = b_1
\qquad (5)
$$

$$
a_2' = a_2 + (P/m_2), \qquad b_2' = b_2
\qquad (6)
$$

Şimdi $P$ tek sayı değerini bulmak için enerji muhafazası formülünü
kullanabiliriz. Tek boyutta $1/2 m v^2$ şeklinde olan formülü $\frac{1}{2} m
\cdot \vec{v}\cdot\vec{v}$ olarak değiştirmek lazım. Ya da $\frac{1}{2} m
<\vec{v},\vec{v}>$, ya da $\frac{1}{2} m ||v||^2$.  O zaman

$$
\frac{m_1}{2} ||v_1||^2 + \frac{m_2}{2} ||v_2||^2  =
\frac{m_1}{2} ||v_1'||^2 + \frac{m_2}{2} ||v_2'||^2 
$$

$||v_1||^2$ ve $||v_1'||^2$, vs hesabının kolay bir yolu var, eğer üstteki resme
bakarsak mesela $||v_1||$ büyüklüğü kenarları $a_1$ ve $b_1$ olan bir üçgenin
hipotenüsü olarak görülebilir.

$$
\frac{m_1}{2} (a_1^2+b_1^2) + \frac{m_2}{2} (a_2^2+b_2^2) =
\frac{m_1}{2} (a_1'^2+b_1'^2) + \frac{m_2}{2} (a_2'^2+b_2'^2) 
$$

Daha önce bulduğumuz (5),(6) değerlerini üstteki formüle sokunca,

$$
\frac{m_1}{2} (a_1^2+b_1^2) + \frac{m_2}{2} (a_2^2+b_2^2) =
\frac{m_1}{2} \left( \left(a_1-\frac{P}{m_1} \right)^2 + b_1^2 \right)  +
\frac{m_2}{2} \left( \left(a_2-\frac{P}{m_1} \right)^2 + b_2^2 \right) 
$$

$b_1^2$ ve $b_2^2$ iptal olur. Her şeyi $P$ sol tarafta olacak şekilde tekrar
düzenlersek,

$$
P = \frac{2 m_1 m_2 (a_1-a_2)}{m_1+m_2}
$$


Bu degeri (1) ve (2)'ye sokarsak,


$$
\vec{v}_1' = \vec{v}_1 - \frac{2 m_2 (a_1-a_2)}{m_1+m_2} \vec{n}
$$

$$
\vec{v}_2' = \vec{v}_2 + \frac{2 m_1 (a_1-a_2)}{m_1+m_2} \vec{n}
$$

Üstteki formülü değişik kaynaklarda, mesela [3], biraz farklı formda görüyoruz,
mesela

$$
\vec{v}_1' =
\vec{v}_1 - \frac{2m_2}{m_1+m_2}
\frac{< \vec{v}_1-\vec{v}_2, \vec{x}_1-\vec{x}_2 >}{||\vec{x}_1-\vec{x}_2||^2}
(\vec{x}_1-\vec{x}_2)
$$

$$
\vec{v}_2' =
\vec{v}_2 - \frac{2m_1}{m_1+m_2}
\frac{< \vec{v}_2-\vec{v}_1, \vec{x}_2-\vec{x}_1 >}{||\vec{x}_2-\vec{x}_1||^2}
(\vec{x}_1-\vec{x}_2)
$$

Fakat biraz dikkat edilince mesela $a_1-a_2$'nin $\vec{n}$ yönündeki hız farkı
olduğunu görürüz, yani

$$
a_1-a_2=\frac{< \vec{v}_1-\vec{v}_2,\vec{x}_1-\vec{x}_2 >}{||\vec{x}_1-\vec{x}_2||}
$$

Geri kalanlardan zaten $\vec{n} = \vec{x}_1-\vec{x}_2/||\vec{x}_1-\vec{x}_2||$
ve $m_1,m_2$ değerleri de aynı şekilde iki tarafta uyar.

İki kütlenin eşit olduğu durumlarda (ki moleküler simülasyonlarda bu çok rahat
kabul edilebilir), formül daha da basitleşir [4],

$$
v_1' = v_1 - \left( (v_1-v_2)  \cdot \vec{n} \right) \vec{n}
$$

$$
v_2' = v_2 - \left( (v_2-v_1)  \cdot \vec{n} \right) \vec{n}
$$

ki $\vec{n} = \frac{x_1-x_2}{|x_1-x_2|}$

Basınç (Pressure) ve Parçacık Çarpışması

Bir sıvı içinde duran bir objeye tek uygulanan etki, stres onu
sıkıştıran türden bir etkidir. Diğer bir deyişle bir sıvı içindeki
objenin hissettiği kuvvet onun yüzeyine her zaman diktir.

![](phy_005_basics_07.png)

Bir sıvının içindeki objeye uyguladığı basıncı, o objeye uygulanan
birim alanda uygulanan kuvvet olarak temsil edilebiliriz, kuvvet $F$
ve alan $A$ ise,

$$
P \equiv \frac{F}{A}
$$

Eğer belli bir noktadan bahsetmek istersek, diyelim $dA$ sonsuz
ufaklıktaki bir alana uygulanan $dF$ kuvveti,

$$
P = \frac{dF}{dA}
$$

O zaman belli bir alandaki basınç için o alan üzerinden entegral almak gerekir. 

Basıncın birimi $N / m^2$, şaşırtıcı olmasa gerek, kuvvet birimi
Newton, ve alan birimi $m^2$.

Simulasyon

Önce basit bir simülasyon kodlayalım. Bazı toplar var, onları başta bir kuvvetle
rasgele yönlere iteceğiz ve ne yapacaklarına bakacağız. Fiziksel parametreler
şöyle, yerçekimi sabiti $g = 0.8$ (dünyadan daha az), topların birbirine ya da
duvara çarpması sonucu hiç enerji kaybı olmuyor.

Bu tür bir sistemin konumu, o anki hali her parçacık için bazı değişkenlerin
takip edilmesiyle olacak, bu değişkenler pozisyon, hız, kuvvet. Kütle her
parçacık için aynı olacak.

Parçacık hareketi o parçacık üzerinde uygulanan kuvvet ile belirlenir, Newton
denklemi $m \bar{a} = \bar{f}$, ki ivme ve kuvvet çok boyutlu dikkat edelim, o
sebeple vektör notasyonu olarak üstte çizgi kullandık. Peki ivmeden, hiza ve yer
değişikliğine nasıl gideriz? Newton formülünü bir ODE olarak tekrar düzenlersek
onu ileri doğru entegre edebiliriz. Yer $\bar{x}$, hız $\bar{v}$ olmak üzere
[9,10] ve her $i$ parçacığı için,

$$
\dot{\bar{v}}_i = \bar{f}_i / m_i
$$

$$
\dot{\bar{x}}_i = \bar{v}_i
$$

Bu tür bir sistemi entegre etmek için Euler'in metotu kullanılabilir [5, sf 5],
her $n$ anında bir sonraki $n+1$ değeri için

$$
\bar{x}^{n+1} = \bar{x}^n + h \bar{v}^n
$$

$$
\bar{v}^{n+1} = \bar{v}^n + h \bar{a}^n
$$

ki $h$ ufak zaman aralığı olarak alınır, bir diğer isim $\Delta t$ olabilir,
alttaki kodda `dt` . O zaman her zaman diliminde her parçacığa etki eden
kuvvetler toplanır, bir nihai kuvvet vektörü elde edilir. Ardından üstteki
formüllerle sistem her parçacık için entegre edilir ve bir sonraki sistem durumu
elde edilir.

Bu ilk sistemde bazı basitleştirmeler var; kuvvet uygulanma ve onun hıza
dönüşmesine her koşulda bakmıyoruz, duvarlar ve parçacıklar arası etkileri direk
hız üzerinde uyguluyoruz. Topların birbirine çarpma sonucu hız vektörlerinin
hesabı [8]'te.

Kodlama notu, çarpışma hesabı için her parçacığın diğer parçacığa yakınlık
kontrolü pahalı olursa, daha fazla parçacık için mesela, bunun için böleç
tekniği kullanılabilir [7].

Genel grafik yöntemi şurada [1] işlendi.

```python
from mpl_toolkits.mplot3d import Axes3D
from random import random
from collections import defaultdict
import sys, numpy.linalg as lin, datetime, os

G = np.array([0.0, 0.0, -0.8])

m = 0.1
B = 10 # number of balls
BS = 10.0 # bounding box size

EPS = 0.1
BOUND_DAMPING = -0.6

if not os.path.exists("/tmp/sim"): os.mkdir("/tmp/sim")

class Simulation:
    def __init__(self):
        self.r   = 0.8
        self.rvec   = np.ones(B) * self.r
        self.dt  = 0.1
        self.balls = []
        self.cor = 0.5
        # Corrected self.mmax to use BS for consistency with the global bounding box size
        self.mmax =  BS - self.r
        self.mmin = 0.0 + self.r

    def init(self):
        for b in range(B):
            # Randomize initial velocity for each ball
            v = 5 * (np.random.rand(3) - 0.5) # Random values between -2.5 and 2.5
            p = np.array([np.random.rand() * BS, np.random.rand() * BS, np.random.rand() * BS]) # Scale initial positions
            f = 5 * np.array([np.random.rand(), np.random.rand(), np.random.rand()])
            self.balls.append({'x':p, 'f':f, 'v': v, 'i': b})

    def computeForces(self, i):
        if (i==0): # Only compute forces once per step
            for j,b in enumerate(self.balls):
                b['f'] = np.array([0.0, 0.0, 0.0]) # Reset forces

            # Gravity
            for j,b in enumerate(self.balls):
                b['f'] += m * G

            # Ball-Ball Collisions (simplified - just adds a repulsion force)
            for j in range(len(self.balls)):
                for k in range(j + 1, len(self.balls)):
                    ball_j = self.balls[j]
                    ball_k = self.balls[k]
                    dist_vec = ball_k['x'] - ball_j['x']
                    distance = lin.norm(dist_vec)
                    if distance < (self.r + self.r):
                        overlap = (self.r + self.r) - distance
                        direction = dist_vec / distance
                        # Simple repulsion force
                        # You might need to adjust the magnitude of this force (e.g., 100)
                        # or implement a more physically accurate impulse-based collision response.
                        repulsion_force = 10 * overlap * direction
                        ball_j['f'] -= repulsion_force
                        ball_k['f'] += repulsion_force

    def evolve(self):
        self.computeForces(0) # Compute forces for all balls

        for b in self.balls:
            # Update velocity
            b['v'] += (b['f'] / m) * self.dt

            # Update position
            b['x'] += b['v'] * self.dt

            # Boundary collisions
            for i in range(3): # x, y, z dimensions
                if b['x'][i] < self.mmin:
                    b['x'][i] = self.mmin
                    b['v'][i] *= BOUND_DAMPING
                elif b['x'][i] > self.mmax:
                    b['x'][i] = self.mmax
                    b['v'][i] *= BOUND_DAMPING


def visualize_matplotlib(sim):
    fig = plt.figure(figsize=(8, 8))
    ax = fig.add_subplot(111, projection='3d')

    # Plot balls
    x_coords = [b['x'][0] for b in sim.balls]
    y_coords = [b['x'][1] for b in sim.balls]
    z_coords = [b['x'][2] for b in sim.balls]
    ax.scatter(x_coords, y_coords, z_coords, s=sim.rvec*500, c='white', edgecolors='black') # s is marker size, scaled for visibility

    # Plot bounding box
    # Lines along x-axis
    ax.plot([0, BS], [0, 0], [0, 0], color='red')
    ax.plot([0, BS], [0, 0], [BS, BS], color='black')
    ax.plot([0, BS], [BS, BS], [0, 0], color='black')
    ax.plot([0, BS], [BS, BS], [BS, BS], color='black')

    # Lines along y-axis
    ax.plot([0, 0], [0, BS], [0, 0], color='green')
    ax.plot([0, 0], [0, BS], [BS, BS], color='black')
    ax.plot([BS, BS], [0, BS], [0, 0], color='black')
    ax.plot([BS, BS], [0, BS], [BS, BS], color='black')

    # Lines along z-axis
    ax.plot([0, 0], [0, 0], [0, BS], color='blue')
    ax.plot([0, 0], [BS, BS], [0, BS], color='black')
    ax.plot([BS, BS], [0, 0], [0, BS], color='black')
    ax.plot([BS, BS], [BS, BS], [0, BS], color='black')


    ax.set_xlim(0, BS)
    ax.set_ylim(0, BS)
    ax.set_zlim(0, BS)
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Z')
    ax.set_title('Ball Simulation (Matplotlib)')
    ax.set_facecolor('black') # Set background to black
    ax.grid(False) # Turn off grid

    return fig, ax


sim = Simulation()
sim.init()

# Initial plot
fig, ax = visualize_matplotlib(sim)

# To create an animation similar to the original Mayavi code,
# you would typically run the simulation for multiple steps
# and save each frame.
# Here's an example of how you would save frames:
#
num_frames = 50
for i in range(num_frames):
    sim.evolve()
    fig, ax = visualize_matplotlib(sim) # Recreate plot for each frame or update existing plot
    plt.savefig(f'/tmp/sim/frame_{i:04d}.png', dpi=150)
    plt.close(fig) # Close the figure to free memory

```

Tüm resimleri birleştirirsek,

```python
os.system("convert -delay 10 -loop 0 /tmp/sim/*.png /tmp/balls1.gif")
```

Sonuç [2]'de görülebilir.

Kaynaklar

[1] Bayramlı, *OpenGL, PyOpenGL*, [https://burakbayramli.github.io/dersblog/sk/2020/08/pyopengl.html](https://burakbayramli.github.io/dersblog/sk/2020/08/pyopengl.html)

[2] Bayramlı, *Simulasyon 1 Animasyon*,
    [https://www.dropbox.com/scl/fi/0hfn6b7wltqfs9hf68uxv/balls1.gif?rlkey=m6atp3r1mx89v10u0adcum4wx&st=e4uvvrv1&raw=1](https://www.dropbox.com/scl/fi/0hfn6b7wltqfs9hf68uxv/balls1.gif?rlkey=m6atp3r1mx89v10u0adcum4wx&st=e4uvvrv1&raw=1)

[3] Wikipedia, *Elastic collision*, [https://en.wikipedia.org/wiki/Elastic_collision](https://en.wikipedia.org/wiki/Elastic_collision)

[4] Masson, *Elastic Collisions in 3D*, [https://exploratoria.github.io/exhibits/mechanics/elastic-collisions-in-3d/index.html](https://exploratoria.github.io/exhibits/mechanics/elastic-collisions-in-3d/index.html)

[6] Levi, *Classical Mechanics with Calculus of Variations and Optimal Control*

[7] Bayramlı, *Bilgisayar Bilim, Geometrik Anahtarlama (Spatial Hashing) ve Izgara (Grid) ile En Yakın Noktaları Bulmak*

[8] Bayramlı, Fizik, *Temel Fizik 2, Dönüşler, Basınç, Çarpışma*

[9] Müller, *Fluid Simulation SIGGRAPH 2007 Course Notes*,

[10] *Visual Interactive Simulation (Spring 15)*,
    [https://www8.cs.umu.se/kurser/5DV058/VT15/](https://www8.cs.umu.se/kurser/5DV058/VT15/)


