import gym, time
import numpy as np

env = gym.make("CartPole-v0")
env.reset()
obs, reward, done, info = env.step(0)
print obs
print obs.shape

env.render()
time.sleep(10)
        
