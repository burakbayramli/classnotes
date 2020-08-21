
Acceleraton to speed 80 km/h = 22.2 m/s

typical car 120 horsepower = 90 KWatts


```python
80*1000.0 / 3600.0
```

```text
Out[1]: 22.22222222222222
```


```python
v = 22.2
m = 3000
```


Air Resistance

```python
h = 3.0
w = 2.5
l = 6.0
A  = w*h
D = 0.5
rho = 1.3
fa = 1/2 * D * rho * A * v**2
print (fa)
```

```text
1201.2975
```


Friction

```python
mu = 0.016
fr = mu * m * 9.8
print (fr)
```

```text
470.40000000000003
```

```python
ft = fr + fa
print (ft * v)
```

```text
37111.684499999996
```


```python
dt = 120
a = 22.2 / 30.0
F = m*a
print (F*v)
```

```text
49284.0
```








Honda EU3000IH1A Handi - https://amzn.to/354Ovhw
3000 Watts, 35 kg. 4 hp.
At full throttle, a single tank will last 3.5 hours.

refigrator 250 watt
heater 1500
stove 3000 watts

Tiny house weight around 3,000 pounds (1.3 tons)
Assume 3 ton with everything

Horsepower = 	Weight / (ET/5.825)**3.
ET=55
3.93 horsepower

https://www.calculator.net/engine-horsepower-calculator.html?v1weight=1.5&v1weightunit=ton&v1time=55&v1timeunit=second&calctype=et&x=93&y=9#et

https://www.stealth316.com/2-calc-hp-et-mph.htm

http://physics.bu.edu/~duffy/py105/Power.html

https://www.engr.colostate.edu/~allan/fluids/page8/page8.html

https://www.thoughtco.com/power-2699001

power = force x velocity
resnick 8th 206 drag, friction, car



```python
(1500*2.2) / (55.0/5.825)**3
```

```text
Out[1]: 3.9202490702479356
```

```python
402.3 * ((60*60)/55.0) / 1000.0
```

```text
Out[1]: 26.332363636363635
```


