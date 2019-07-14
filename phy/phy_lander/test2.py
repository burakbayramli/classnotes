import gym, six
import pyglet
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

env = gym.make('LunarLander-v2')

observation = env.reset() 

for i in range(100):
    action = env.action_space.sample() 
    env.render()
    observation, reward, done, info = env.step(action)

    buffer = pyglet.image.get_buffer_manager().get_color_buffer()            
    image_data = buffer.get_image_data()
    if i % 10 == 0:
        image_data.save(filename='frames/out-%04d.png' % i)
    
    print (observation)
    if done: break
    
env.close()
