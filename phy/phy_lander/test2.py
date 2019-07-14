import matplotlib.animation as animation
import gym, six
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

env = gym.make('LunarLander-v2')

observation = env.reset() 

for t in range(100):
    action = env.action_space.sample() 
    env.render()
    observation, reward, done, info = env.step(action)
    print (observation)
    if done: break
    
env.close()
