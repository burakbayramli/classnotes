

```python
from scipy.optimize import minimize, Bounds, rosen

def constraint(x):
    return x[0] + x[1] - 1

cons = [{'type':'eq', 'fun': constraint}]

minimize(rosen, [0, 0], method='trust-constr', 
         bounds=Bounds([-2,-2],[2, 2]), constraints=cons)
```

```text
Out[1]: 
 barrier_parameter: 2.560000000000001e-07
 barrier_tolerance: 2.560000000000001e-07
          cg_niter: 11
      cg_stop_cond: 1
            constr: [array([0.]), array([0.61879562, 0.38120438])]
       constr_nfev: [36, 0]
       constr_nhev: [0, 0]
       constr_njev: [0, 0]
    constr_penalty: 1.0
  constr_violation: 0.0
    execution_time: 0.15960264205932617
               fun: 0.14560701802826012
              grad: array([-0.34072562, -0.34072558])
               jac: [array([[1., 1.]]), array([[1., 0.],
       [0., 1.]])]
   lagrangian_grad: array([-6.68706079e-10,  6.68705970e-10])
           message: '`gtol` termination condition is satisfied.'
            method: 'tr_interior_point'
              nfev: 36
              nhev: 0
               nit: 20
             niter: 20
              njev: 0
        optimality: 6.687060790177414e-10
            status: 1
           success: True
         tr_radius: 1236718.7182473193
                 v: [array([0.34072553]), array([8.80386589e-08, 5.02605228e-08])]
                 x: array([0.61879562, 0.38120438])
```






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
plt.savefig('/tmp/out.png')
```



```python
rho = 7.0
def sig(x,a):
   return (x-a)*1/(1+np.exp(-rho*(x-a)))

#def sig(x,a):
#   return np.max([0,x-a])

a1,b1,c1,d1 = (1, 2.4, 1, -3.5)
a2,b2,c2,d2 = (1, 0.4, -0.1, 0.5)
def f(t):
    x = a1 + \
        b1*sig(t,1) + \
        c1*sig(t,2) + \
        d1*sig(t,3) 
	   
    y = a2 + \
        b2*sig(t,1) + \
        c2*sig(t,2) + \
        d2*sig(t,3) 
    return x,y	   

print (a1+b1*(5-1)+c1*(5-2)+d1*(5-3))
print (a2+b2*(5-1)+c2*(5-2)+d2*(5-3))

tmp = np.linspace(0,5,100)
res = np.array([f(tt) for tt in tmp])
plt.plot(res[:,0],res[:,1],'.')
plt.xlim(0,ex)
plt.ylim(0,ey)
plt.savefig('/tmp/out.png')
```





