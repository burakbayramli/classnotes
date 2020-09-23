from KineticsKit import *
from visual import vector

system = System(timestep=0.04, gravity=0.4)

mass1 = Mass(m=0.05, pos=(0.0, 0.0, 0.0), fixed=1)
mass2 = Mass(m=0.05, pos=(0.0, 0.5, 0.0))
mass3 = Mass(m=0.05, pos=(0.0, 1.0, 0.0))
mass4 = Mass(m=0.2, pos=(0.0, 1.5, 0.0))
mass5 = Mass(m=0.05, pos=(0.0, 2.0, 0.0))
mass6 = Mass(m=0.05, pos=(0.0, 2.5, 0.0))
mass7 = Mass(m=0.05, pos=(0.0, 3.0, 0.0), fixed=1)

system.insertMass(mass1)
system.insertMass(mass2)
system.insertMass(mass3)
system.insertMass(mass4)
system.insertMass(mass5)
system.insertMass(mass6)
system.insertMass(mass7)

spring1 = SingleHelixSpring(m0=mass1, m1=mass2, k=1, damping=0.5)
system.insertSpring(spring1)
spring2 = SingleHelixSpring(m0=mass2, m1=mass3, k=1, damping=0.5)
system.insertSpring(spring2)
spring3 = SingleHelixSpring(m0=mass3, m1=mass4, k=1, damping=0.5)
system.insertSpring(spring3)
spring4 = SingleHelixSpring(m0=mass4, m1=mass5, k=1, damping=0.5)
system.insertSpring(spring4)
spring5 = SingleHelixSpring(m0=mass5, m1=mass6, k=1, damping=0.5)
system.insertSpring(spring5)
spring5 = SingleHelixSpring(m0=mass6, m1=mass7, k=1, damping=0.5)
system.insertSpring(spring5)

loc_1 = [mass2.sphere.pos.y, mass3.sphere.pos.y, 
         mass4.sphere.pos.y, mass5.sphere.pos.y, 
         mass6.sphere.pos.y]

count = 0

while 1:
    system.step()
    count += 1
    if count == 100: break
    
loc_2 = [mass2.sphere.pos.y, mass3.sphere.pos.y, 
         mass4.sphere.pos.y, mass5.sphere.pos.y, 
         mass6.sphere.pos.y]

from itertools import izip
for x,y in izip(loc_1, loc_2):
    print x-y
    
      
