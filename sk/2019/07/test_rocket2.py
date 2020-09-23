# According to Pontryagin's maximum principle it's optimal to fire
# engine full throttle or turn it off. That's the reason this
# environment is OK to have discreet actions (engine on or off).
#
# Landing pad is always at coordinates (0,0). Coordinates are the
# first two numbers in state vector.  Reward for moving from the top
# of the screen to landing pad and zero speed is about 100..140
# points.  If lander moves away from landing pad it loses reward
# back. Episode finishes if the lander crashes or comes to rest,
# receiving additional -100 or +100 points. Each leg ground contact is
# +10. Firing main engine is -0.3 points each frame. Firing side
# engine is -0.03 points each frame. Solved is 200 points.
#
# Landing outside landing pad is possible. Fuel is infinite, so an
# agent can learn to fly and then land on its first attempt. Please
# see source code for details.

# Action is two floats [main engine, left-right engines].

# Main engine: -1..0 off, 0..+1 throttle from 50% to 100%
# power. Engine can't work with less than 50% power.

# Left-right: -1.0..-0.5 fire left engine, +0.5..+1.0 fire right
# engine, -0.5..0.5 off
import gym, six, pyglet, pandas as pd, numpy as np
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
        image_data.save(filename='frames/lunar-%04d.png' % i)
    
    print (observation)
    if done: break
    
env.close()
