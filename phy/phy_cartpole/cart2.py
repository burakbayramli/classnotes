# http://www.philipzucker.com/cart-pole-using-lyapunov-lqr-control-openai-gym/

import math
import gym
from gym import spaces
from gym.utils import seeding
import numpy as np
import logging
logger = logging.getLogger(__name__)
 
class CartPoleEnv(gym.Env):
    metadata = {
        'render.modes': ['human', 'rgb_array'],
        'video.frames_per_second' : 50
    }
 
    def __init__(self):
        self.gravity = 9.8
        self.masscart = 1.0
        self.masspole = 0.1
        self.total_mass = (self.masspole + self.masscart)
        self.length = 0.5 # actually half the pole's length
        self.polemass_length = (self.masspole * self.length)
        self.force_mag = 3.0
        self.tau = 0.02  # seconds between state updates
 
        # Angle at which to fail the episode
        # we expect full swings
        self.theta_threshold_radians =  np.pi  #12 * 2 * math.pi / 360
        self.x_threshold = 2.4
 
        # Angle limit set to 2 * theta_threshold_radians so failing observation is still within bounds
        high = np.array([
            self.x_threshold * 2,
            np.finfo(np.float32).max,
            self.theta_threshold_radians * 2,
            np.finfo(np.float32).max])
        high2 = np.array([1])
        self.action_space = spaces.Discrete(2) #spaces.Box(-high2, high2)# 
        self.observation_space = spaces.Box(-high, high)
 
        self._seed()
        self.viewer = None
        self.state = None
        self.buffer_size = 0

 
        self.steps_beyond_done = None
        self.steps = 0
        self.num_envs = 1
        self.viewer = None
    viewer = None
    def _seed(self, seed=None):
        self.np_random, seed = seeding.np_random(seed)
        return [seed]
 
    def _step(self, action):
        #assert self.action_space.contains(action), "%r (%s) invalid"%(action, type(action))
        #print(action)
        #action =action[0] # max(-1, min(action[0],1))
        state = self.state
        x, x_dot, theta, theta_dot = state
        force = self.force_mag if action==1 else -self.force_mag
        #force = self.force_mag * action
        #print(action)
        #print(state)
 
        x  = x + self.tau * x_dot
        theta = theta + self.tau * theta_dot
        costheta = math.cos(theta)
        sintheta = math.sin(theta)
        temp = (force + self.polemass_length * theta_dot * theta_dot * sintheta) / self.total_mass
        thetaacc = (self.gravity * sintheta - costheta* temp) / (self.length * (4.0/3.0 - self.masspole * costheta * costheta / self.total_mass))
        xacc  = temp - self.polemass_length * thetaacc * costheta / self.total_mass
        
        x_dot = x_dot + self.tau * xacc
        
        theta_dot = theta_dot + self.tau * thetaacc
        self.state = (x,x_dot,theta,theta_dot)
        
        done =  x < -self.x_threshold \
                or x > self.x_threshold \
                or theta < -np.pi * 5 \
                or theta > np.pi * 5 \
                or self.steps > 1024
        done = bool(done)
        
        self.steps += 1
        limit = 200
 
        reward = 0.0
        if  x < -self.x_threshold or x > self.x_threshold:
            reward -= 1.0
        reward += (np.cos(theta)+1)**2 / 4
        #reward -= 0.1 * action**2 
        reward += -0.1 * x**2
        if np.cos(theta) > 0.95:
            reward += 1
        #reward = reward/2048
        '''
        if not done:
            reward = 1.0
        elif self.steps_beyond_done is None:
            # Pole just fell!
            self.steps_beyond_done = 0
            reward = 1.0
        else:
            if self.steps_beyond_done == 0:
                logger.warning("You are calling 'step()' even though this environment has already returned done = True. You should always call 'reset()' once you receive 'done = True' -- any further steps are undefined behavior.")
            self.steps_beyond_done += 1
            reward = 0.0
        '''
        #print(np.array(self.state).reshape((1,-1)), reward, done, {})

        #obs = np.array([x,x_dot,np.cos(theta),np.sin(theta),theta_dot])# + self.action_buffer)
        self.buffer.append(np.copy(self.state))
        obs2 = self.buffer.pop(0)
        return obs2, reward, done, {}
 
    def _reset(self):
        #self.state = self.np_random.uniform(low=-0.05, high=0.05, size=(4,))
        #x, xdot, theta, thetadot
        self.state = np.array([0, 0, np.pi+0.2 , 0]) # np.pi+0.2 + self.np_random.uniform(low=-1.0, high=1.0, size=(4,))
        self.steps_beyond_done = None
        self.steps = 0
        self.buffer = []
        for i in range(self.buffer_size):
            self.buffer.append(self.state)
        return np.array(self.state)
 
    def _render(self, mode='human', close=False):
        if close:
            if self.viewer is not None:
                self.viewer.close()
                self.viewer = None
            return
 
        screen_width = 600
        screen_height = 400
 
        world_width = self.x_threshold*2
        scale = screen_width/world_width
        carty = 100 # TOP OF CART
        polewidth = 10.0
        polelen = scale * 1.0
        cartwidth = 50.0
        cartheight = 30.0
 
        if self.viewer is None:
            from gym.envs.classic_control import rendering
            self.viewer = rendering.Viewer(screen_width, screen_height)
            l,r,t,b = -cartwidth/2, cartwidth/2, cartheight/2, -cartheight/2
            axleoffset =cartheight/4.0
            cart = rendering.FilledPolygon([(l,b), (l,t), (r,t), (r,b)])
            self.carttrans = rendering.Transform()
            cart.add_attr(self.carttrans)
            self.viewer.add_geom(cart)
            l,r,t,b = -polewidth/2,polewidth/2,polelen-polewidth/2,-polewidth/2
            pole = rendering.FilledPolygon([(l,b), (l,t), (r,t), (r,b)])
            pole.set_color(.8,.6,.4)
            self.poletrans = rendering.Transform(translation=(0, axleoffset))
            pole.add_attr(self.poletrans)
            pole.add_attr(self.carttrans)
            self.viewer.add_geom(pole)
            self.axle = rendering.make_circle(polewidth/2)
            self.axle.add_attr(self.poletrans)
            self.axle.add_attr(self.carttrans)
            self.axle.set_color(.5,.5,.8)
            self.viewer.add_geom(self.axle)
            self.track = rendering.Line((0,carty), (screen_width,carty))
            self.track.set_color(0,0,0)
            self.viewer.add_geom(self.track)
 
        if self.state is None: return None
 
        x = self.state
        cartx = x[0]*scale+screen_width/2.0 # MIDDLE OF CART
        self.carttrans.set_translation(cartx, carty)
        self.poletrans.set_rotation(-x[2])
 
        return self.viewer.render(return_rgb_array = mode=='rgb_array')

import gym

import numpy as np
import scipy.linalg as linalg
lqr = linalg.solve_continuous_are

env = CartPoleEnv()
env.buffer_size = 5
gravity = 9.8
masscart = 1.0
masspole = 0.1
total_mass = (masspole + masscart)
length = 0.5 # actually half the pole's length
polemass_length = (masspole * length)
force_mag = 10.0
tau = 0.02 

def E(x):
	return 1/2 * masspole * (2 * length)**2 / 3 *  x[3]**2 + np.cos(x[2]) * polemass_length * gravity
def u(x):
	#print(E(x)-Ed)
	return 1.0 * (E(x)-Ed) * x[3] * np.cos(x[2])


H = np.array([
	[1,0,0,0],
	[0,total_mass, 0, - polemass_length],
	[0,0,1,0],
	[0,- polemass_length, 0, (2 * length)**2 * masspole /3]
	])

Hinv = np.linalg.inv(H)

A = Hinv @ np.array([
    [0,1,0,0],
    [0,0,0,0],
    [0,0,0,1],
    [0,0, - polemass_length * gravity, 0]
	])
B = Hinv @ np.array([0,1.0,0,0]).reshape((4,1))
Q = np.diag([0.1, 1.0, 100.0, 5.0])
R = np.array([[0.1]])

P = lqr(A,B,Q,R)
Rinv = np.linalg.inv(R)
K = Rinv @ B.T @ P
print(K)
def ulqr(x):
	x1 = np.copy(x)
	x1[2] = np.sin(x1[2])
	return np.dot(K, x1)

Ed = E([0,0,0,0])

observation = env.reset()
for _ in range(1000):
  observation = np.copy(observation)
  env.render()
  observation[0] += observation[1] * tau * env.buffer_size
  observation[2] += observation[3] * tau * env.buffer_size
  observation[3] += np.sin(observation[2]) * tau * env.buffer_size * gravity  / (length * 2) / 2
  #action = env.action_space.sample() # your agent here (this takes random actions)
  a = u(observation) - 0.3 * observation[0] -  0.1 * observation[1]
  if  abs(E(observation)-Ed) < 0.1 and np.cos(observation[2]) > 0.6: #abs(E(observation)-Ed) < 0.1 and
    
  	a = ulqr(observation)
  	env.force_mag = min(abs(a[0]), 10)
  	#print(a)
  else:
  	env.force_mag = 10.0
  if a < 0:
  	action = 0
  else:
  	action = 1



  observation, reward, done, info = env.step(action)


