import matplotlib.animation as animation
import gym, six, random
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

env = gym.make('CartPole-v0')

observation = env.reset() 
rewards = 0
print (env.action_space)

for t in range(100):
    #action = env.action_space.sample()
    #print (action)
    action = random.choice([0,1])
    #action = 1
    img = env.render(mode='rgb_array') 
    if t%10 == 0:
        #plt.imshow(img)
        #plt.savefig('/tmp/cart-%d' % t)
        print (img.shape)
    observation, reward, done, info = env.step(action)
    print (observation)

print(env.observation_space.high)
print(env.observation_space.low)
    
env.close()
