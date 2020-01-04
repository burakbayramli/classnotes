


```python
import math
a,b,c,d = (1, -1.4, 2, 2.5)
x = np.linspace(0,5,100)
def f(x):
    return a + \
           b*np.max([0,x-2]) + \
           c*np.max([0,x-3]) + \
           d*np.max([0,x-4])
	   
    
y = np.array([f(xx) for xx in x])
plt.plot(x,y,'.')
plt.savefig('out.png')
```


```python

a1,b1,c1,d1 = (1, -1.4, 2, 2.5)
a2,b2,c2,d2 = (1, 1.4, 1, 2.5)
def f(t):
    x = a1 + \
        b1*np.max([0,t-2]) + \
        c1*np.max([0,t-3]) + \
        d1*np.max([0,t-4])
	   
    y = a2 + \
        b2*np.max([0,t-2]) + \
        c2*np.max([0,t-3]) + \
        d2*np.max([0,t-4])
    return x,y	   
    
tmp = np.linspace(0,5,100)
res = np.array([f(tt) for tt in tmp])
print (res)
plt.plot(res[:,0],res[:,1],'.')
plt.savefig('out.png')
```


```python
rho = 5.0
def sig(x,a):
   return 1/(1+np.exp(-rho*(x-a)))
   
a1,b1,c1,d1 = (1, -0.4, 1, 2.5)
a2,b2,c2,d2 = (1, 1.4, 0.4, -0.5)
def f(t):
    x = a1 + \
        b1*t*sig(t,2) + \
        c1*t*sig(t,3) + \
        d1*t*sig(t,4)
	   
    y = a2 + \
        b2*t*sig(t,2) + \
        c2*t*sig(t,3) + \
        d2*t*sig(t,4)
    return x,y	   
    
tmp = np.linspace(0,5,100)
res = np.array([f(tt) for tt in tmp])
plt.plot(res[:,0],res[:,1],'.')
plt.savefig('out.png')
```


```python
rho = 5.0
def sig(x):
   return 1/(1+np.exp(-rho*x))
   
x = np.linspace(-3,3,100)
y = sig(x)
plt.plot(x,y,'.')
plt.savefig('out.png')
```

```python
rho = 7.0
def sig(x,a):
   return (x-a)*1/(1+np.exp(-rho*(x-a)))

#def sig(x,a):
#   return np.max([0,x-a])

a1,b1,c1,d1,e1 = (1, 2.4, 1, -3.5, -2.1)
a2,b2,c2,d2,e2 = (1, 0.4, -0.1, 0.5, -3)
def f(t):
    x = a1 + \
        b1*sig(t,2) + \
        c1*sig(t,3) + \
        d1*sig(t,3.5) + \
        e1*sig(t,4.0)
	   
    y = a2 + \
        b2*sig(t,2) + \
        c2*sig(t,3) + \
        d2*sig(t,3.5) + \
        e2*sig(t,4.0)
    return x,y	   
    
tmp = np.linspace(0,5,100)
res = np.array([f(tt) for tt in tmp])
plt.plot(res[:,0],res[:,1],'.')
plt.savefig('out.png')
```




