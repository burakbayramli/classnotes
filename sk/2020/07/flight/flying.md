

dualcopter
https://youtu.be/b14_XQ5nd2A

duocopter with legs
https://youtu.be/NgvFFxGhTmU

quadcopter, simulation
https://github.com/WittmannF/quadcopter-best-practices/
https://github.com/WittmannF/quadcopter-best-practices/blob/master/README.md#visualizations
http://rodolfojt.github.io/projects/Quadcopter_Project.html
https://github.com/sandeeppaulraj/RL-Quadcopter-2/blob/master/Quadcopter_Project.ipynb

two rotor cylindrical
Bouabdallah
https://youtu.be/kxfCGZ-pN3E
https://www.skyfilabs.com/project-ideas/cylinder-shaped-coaxial-drone
https://diydrones.com/profiles/blogs/tdrone-open-source-coaxial-drone

How drones fly - it's all about forces
newton -> kg conversion for thrust 1 kg = 10N
https://youtu.be/AftJGgRk5f0

swash plate-less
https://www.modlabupenn.org/2014/10/23/underactuated-rotor/
https://youtu.be/KZe7l5_LfoA
https://youtu.be/d80oXSCcHTk?t=49

--------------------------------------------------------

2 generators, 4000 Watt, 40 kg each


```python
rho0 = 1.226
W = (70+10+20+50+50) * 9.8
r = 2.2*2
A = np.pi * r**2
print ( np.sqrt( W**3 / (2.0 * rho0 * A)  )  )
print (3800*2)
print (110 / 2.2)
```

```text
7105.526463908701
7600
49.99999999999999
```

```python
rho0 = 1.226
W = (70+10+20+35+35) * 9.8
r = 2.2*2
A = np.pi * r**2
print ( np.sqrt( W**3 / (2.0 * rho0 * A)  )  )
```

```text
5568.325955750578
```


---------------------------------------------------------

3000 Watt generators weight about 30 kg

1. Generac GP3000i - https://amzn.to/2nhZT8F

2. WEN 56380i - https://amzn.to/31IQHsZ
   50 kg, 3400 Watt

3. Champion 75537i - https://amzn.to/2LKPPhZ

4. Briggs and Stratton P 3000 - https://amzn.to/2IE3DsP

5. Honda EU3000IH1A Handi - https://amzn.to/354Ovhw 3000 Watts, 35
   kg. At full throttle, a single tank will last 3.5 hours.

Turkiye Inverter Motor

https://www.koctas.com.tr/jeneratorler/c/106001

helicop
https://www.youtube.com/watch?v=HrsGM0PzQFo


