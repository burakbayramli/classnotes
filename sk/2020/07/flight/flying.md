

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


Thrust Vectoring Mount - 
https://youtu.be/mA7TwcemOh0
https://youtu.be/gU74BF0-DcA
https://youtu.be/izXohCO6ing

top bottom two rotor, dualcopter
https://www.youtube.com/watch?v=b14_XQ5nd2A
Multiwii Mega flight board, version 2.3 firmware

2dof simple
https://youtu.be/FEZ8B2HyTEw

dual axis servo control
https://youtu.be/mz9E5B5VlTE


universal joint
https://youtu.be/ZIFDbHsuOV0
https://youtu.be/CZIlFiRCmBg
https://youtu.be/Tjn5BAqSb2Q
https://youtu.be/cUC3W_rW7dc

3D printing

http://www.prototipyap.com/?gclid=EAIaIQobChMIlqTk5_mY6gIVkU8YCh0qAQJNEAMYASAAEgJju_D_BwE


3D, gazebo, CAD
https://youtu.be/aP4sDyrRzpU
http://gazebosim.org/tutorials?tut=build_model
https://3dwarehouse.sketchup.com/model/0a36a48e-ce57-4f41-bfc5-216b656cc9f3/Gimbal-lock

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


Benzin motoru

```python
rho0 = 1.226
engw = 50
myw = 80
luggw = 20
W = (myw+luggw+engw*2) * 9.8
r = 1.5*2
A = np.pi * r**2
w = np.sqrt( W**3 / (2.0 * rho0 * A)  ) 
print ( w / 750, 'hp' )
```

```text
13.895251751643682 hp
```


https://www.irhaltarim.com.tr/urun/honda-gx200-yatay-milli-motor-6-5-hp/4810

