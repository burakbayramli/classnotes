# ipyvolume

```python
import numpy as np
import ipyvolume as ipv 

class Particle():
    def __init__ (self, pos_vec, vel_vec, box_size, time_step, grav):
        self.pos = pos_vec
        self.vec = vel_vec
        self.grav = grav 
        self.box_size = box_size
    
    def pos_update(self, time_step):
        for i in range(3):
            if self.pos[i] > self.box_size:
                self.vec[i] = self.vec[i]*-1
                self.pos[i] += -(self.pos[i]-self.box_size)

            if self.pos[i]<0:
                self.vec[i] = self.vec[i]*-1
                self.pos[i] += -self.pos[i]
        
                if self.grav == True:
                    if i==2:
                        self.vec = self.vec*0.8
       
        if self.grav == True: 
            self.vec[2]= self.vec[2]-time_step*1

        self.pos[0] += self.vec[0]*time_step
        self.pos[1] += self.vec[1]*time_step
        self.pos[2] += self.vec[2]*time_step
        

def initialize_particles(number_of_particles,box_size,time_step, grav = False):

    velocities = np.random.randn(number_of_particles,3)

    mean_vel = np.full([number_of_particles,3], np.mean(velocities, axis=0))
    
    velocities = np.subtract(velocities,mean_vel)

    velocities[:,2] = 0
    
    pos_vec = np.random.random([number_of_particles,3])*box_size #Generate random positions
    
    particle_list = [] 
    
    for i in range(number_of_particles):
        particle_list.append(Particle(pos_vec[i,:], velocities[i,:], box_size,time_step, grav))
        
    return particle_list

def bounce(particle1,particle2, particle_radius, time_step):
    a = particle1.pos - particle2.pos
    b = (a[0]**2 + a[1]**2 + a[2]**2)**(1/2)
    n = np.divide(a,b,out=np.zeros_like(a), where=b!=0)       
      
    v_rel = particle1.vec - particle2.vec
    
    v_norm = np.dot(v_rel,n)*n
    
    particle1.vec += -v_norm
    particle2.vec += +v_norm

    while particledist(particle1, particle2)<(2*particle_radius):
        particle1.pos_update(time_step)
        particle2.pos_update(time_step)


def particledist(particle1, particle2):
    r_x = particle1.pos[0] - particle2.pos[0]
    r_y = particle1.pos[1] - particle2.pos[1]
    r_z = particle1.pos[2] - particle2.pos[2]
    
    r = (r_x**2 + r_y**2 + r_z**2)**(1/2)
    
    return r
    
def isbounce(particle_list, particle_radius, time_step):
    for particle1 in particle_list:
        for particle2 in particle_list:

            if particledist(particle1,particle2)<2*particle_radius and particledist(particle1,particle2)!=0:
                
                bounce(particle1, particle2, particle_radius, time_step) #Call bounce function

def avgvel(particle_list):
    vel_vec = np.array([0,0,0])
    for particle in particle_list:
        vel_vec =vel_vec + particle.vec
        
    avg_vec = vel_vec/len(particle_list)
    
    return avg_vec

def tot_eng(particle_list):
    tot_eng = 0
    for particle in particle_list:
        tot_eng = tot_eng + particle.vec[0]**2 + particle.vec[1]**2 + particle.vec[2]**2
        
    return tot_eng

def particleSimulate(num_particles, box_size, total_time, time_step, particle_radius,
                     grav = False, save = False):
    
    particle_list = initialize_particles(num_particles, box_size, time_step, grav) #Generate starting points
    
    x=np.zeros([total_time,num_particles,1])
    y=np.zeros([total_time,num_particles,1])
    z=np.zeros([total_time,num_particles,1])
    
    time = 0
    
    while time < total_time:
        
        isbounce(particle_list, particle_radius, time_step)    
        
        
        for i in range(len(particle_list)):
            particle_list[i].pos_update(time_step) #Update position
            
            x[time,i] = particle_list[i].pos[0]
            y[time,i] = particle_list[i].pos[1]
            z[time,i] = particle_list[i].pos[2]
        
        time += 1
        
        if (time/total_time)*100%10==0:
            print(str(time/total_time*100) + "% complete")
            
    colors = []
    for i in range(num_particles):
        colors.append([0,0,1])
    
    ipv.figure()
    s = ipv.scatter(x, z, y, color = colors , size=7, marker="sphere")
    ipv.animation_control(s, interval=1)
    ipv.xlim(-1,box_size+1)
    ipv.ylim(-1,box_size+1)
    ipv.zlim(-1,box_size+1)
    
    ipv.style.axes_off()

    ipv.save('./particle_sim.html')

num_particles = 30
box_size = 10
total_time = 1000
time_step = 0.02
particle_radius = 0.5

particleSimulate(num_particles, box_size, total_time, time_step, particle_radius)
```

