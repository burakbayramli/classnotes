
BOUNDARY VALUE PROBLEM FOR ORDINARY DIFFERENTIAL EQUATIONS
WITH APPLICATIONS TO OPTIMAL CONTROL
Sergey AVVAKUMOV
CMC Department, Moscow State Lomonosov University
Moscow, 119899, Russia
and
Yuri KISELEV


Süreksiz, Kopukluğu Olan Fonksiyonları Yaklaşıksal Olarak Temsil Etmek

SGN

```python
v = 0.0
s = np.linspace(-5,5,100)
y = s/np.sqrt(v + s**2)
plt.plot(s,y)
plt.savefig('tst_02.png')
```

```text

![](tst_02.png)


```python
s = np.linspace(-5,5,100)
def f(s):
   if np.abs(s) <= 1: return s
   else: 
      return np.sign(s)

y = [f(x) for x in s]
plt.plot(s,y)
plt.savefig('tst_03.png')
```

DEZ

```python
s = np.linspace(-5,5,100)
def f(s):
   if np.abs(s) <= 1: return 0.0
   else: 
      return np.sign(s)

y = [f(x) for x in s]
plt.plot(s,y)
plt.savefig('tst_04.png')
```








































































































































































