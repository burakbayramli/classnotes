#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

Fakat hala işimiz bitmedi, $A$ noktasından başlayıp $B$ noktasına
giden yolu, kontrol zincirini hesaplamak için doğru $\theta_A$'yi
seçmemiz gerekiyor. Eğer elimizdekiler $u=u(y)$ ve $v=v(y)$ olsaydı o
zaman (2) denklemi şu hale gelirdi, 

$$
\dot{\lambda}_x = 0 \to \lambda_x = \textrm{sabit}
$$

Ve (7) denklemi şu hale dönüşür,

$$
\frac{\cos\theta}{V + u(y)\cos\theta + v(y)\sin\theta} = \textrm{sabit}
$$

$u = -V(y/h), v=0$ üzerinden

$$
\frac{\cos\theta}{V - V(y/h)\cos\theta} = \frac{\cos\theta_f}{V} = \textrm{sabit}
$$

$$
\cos\theta = \frac{\cos\theta_f}{1 + (y/h)\cos\theta_f} 
$$

Bağımsız değişken için $t$ yerine $\theta$ kullanmak daha
rahat. Elimizde bir $y(\theta)$ var, 

$$
\frac{y}{h} = \sec\theta - \sec\theta_f
$$

(6) şu hale gelir, 

$$
\frac{\ud t}{\ud \theta} = \frac{h}{V} \sec^2\theta \to
\frac{V(t_f-t)}{h} = \tan\theta - \tan\theta_f
$$

Bu nasıl oldu? (6)'nin son terimi haricinde tüm diğer 
terimleri $u = -V(y/h), v=0$ yerine koyunca yokolur, kalanlardan

$$
-\cos^2 \theta \frac{\partial u}{\partial y} = 
\cos^2 \theta \frac{V}{h}=\frac{\ud \theta}{\ud t}
$$

Ve

$$
\frac{1}{\frac{\ud \theta}{\ud t}} = \frac{\ud \theta}{\ud t}
$$

üzerinden görülen sonuç varılır. 


```python
from scipy.integrate import odeint

V = 1.0
h = 1.0
def wave(z, t):
    x,y,theta = z
    tmp1 = V*np.cos(theta) + -V*(y/h)
    tmp2 = V*np.sin(theta) 
    tmp3 = -np.cos(theta)**2*(-V/h)
    return [tmp1, tmp2, tmp3]

t = np.linspace(0, 10, 50)

z0 = [3.66,-1.86,np.deg2rad(105)]
sol = odeint(wave, z0, t)
idx = 27
print (sol[idx,:])
print (np.rad2deg(sol[idx,2]))
print (t[idx])
print (sol)
```

```text
[-0.04096147 -0.0363544   4.20008988]
240.64742384230138
5.510204081632653
[[ 3.66       -1.86        1.83259571]
 [ 3.96528883 -1.66325273  1.8470006 ]
 [ 4.2275255  -1.46735252  1.86303511]
 [ 4.44656676 -1.27245131  1.88098251]
 [ 4.62224918 -1.07873864  1.90119291]
 [ 4.75438641 -0.88645346  1.92410295]
 [ 4.84276635 -0.69590085  1.95026263]
 [ 4.88714908 -0.50747525  1.98037137]
 [ 4.88726685 -0.32169378  2.0153271 ]
 [ 4.8428285  -0.13924419  2.05629252]
 [ 4.75353392  0.03894505  2.10478274]
 [ 4.61910865  0.21160268  2.16277586]
 [ 4.43937943  0.37696641  2.23283746]
 [ 4.2144301   0.53257455  2.31821901]
 [ 3.9449087   0.67499519  2.42281526]
 [ 3.63259782  0.79954001  2.55071628]
 [ 3.28136413  0.90014006  2.70490193]
 [ 2.8984305   0.96978203  2.88473245]
 [ 2.4953645   1.00198806  3.08307899]
 [ 2.0875329   0.9931725   3.28607986]
 [ 1.69134401  0.94435907  3.47789556]
 [ 1.32076135  0.86066071  3.64724463]
 [ 0.98544781  0.74903953  3.79003261]
 [ 0.69103634  0.61621829  3.9075356 ]
 [ 0.44036361  0.46768673  4.0034417 ]
 [ 0.23463697  0.30756717  4.08185558]
 [ 0.07421273  0.1388421   4.14642084]
 [-0.04096147 -0.0363544   4.20008988]
 [-0.11111516 -0.21649088  4.24516304]
 [-0.13653995 -0.40045717  4.28340638]
 [-0.11753488 -0.58743768  4.31617269]
 [-0.05438215 -0.77682439  4.34450253]
 [ 0.05266213 -0.96815731  4.36920229]
 [ 0.20337007 -1.16108348  4.39090258]
 [ 0.39754083 -1.35532839  4.41010131]
 [ 0.63499819 -1.55067584  4.42719546]
 [ 0.91558765 -1.74695356  4.44250457]
 [ 1.23917367 -1.94402279  4.45628821]
 [ 1.6056371  -2.14177065  4.46875901]
 [ 2.01487292 -2.34010447  4.48009264]
 [ 2.46678828 -2.53894749  4.49043529]
 [ 2.96130082 -2.73823566  4.49990955]
 [ 3.49833725 -2.93791513  4.50861887]
 [ 4.07783215 -3.1379403   4.51665113]
 [ 4.6997269  -3.33827233  4.52408141]
 [ 5.36396884 -3.53887796  4.53097418]
 [ 6.07051049 -3.73972852  4.53738514]
 [ 6.81930895 -3.94079918  4.54336265]
 [ 7.6103253  -4.14206834  4.54894882]
 [ 8.44352418 -4.34351711  4.55418056]]
```











