from rocketlander import RocketLander
from constants import LEFT_GROUND_CONTACT, RIGHT_GROUND_CONTACT
import numpy as np
import pyglet

if __name__ == "__main__":
    # Settings holds all the settings for the rocket lander environment.
    settings = {'Side Engines': True,
                'Clouds': True,
                'Vectorized Nozzle': True,
                'Starting Y-Pos Constant': 1,
                'Initial Force': 'random'}  # (6000, -10000)}

    env = RocketLander(settings)
    s = env.reset()
    
    left_or_right_barge_movement = np.random.randint(0, 2)

    for i in range(50):

            a = [10.0, 1.0, 1.0]                        
            s, r, done, info = env.step(a)
            # -------------------------------------
            # Optional render
            env.render()
            # Draw the target
            buffer = pyglet.image.get_buffer_manager().get_color_buffer()            
            image_data = buffer.get_image_data()
            if i % 5 == 0:
                image_data.save(filename='frames/rocket-%04d.png' % i)
            
            env.draw_marker(env.landing_coordinates[0], env.landing_coordinates[1])
            # Refresh render
            env.refresh(render=False)

            # When should the barge move? Water movement, dynamics etc can be simulated here.
            if s[LEFT_GROUND_CONTACT] == 0 and s[RIGHT_GROUND_CONTACT] == 0:
                env.move_barge_randomly(0.05, left_or_right_barge_movement)
                
                # Random Force on rocket to simulate wind.
                env.apply_random_x_disturbance \
                (epsilon=0.005, \
                 left_or_right=left_or_right_barge_movement)
                env.apply_random_y_disturbance(epsilon=0.005)

            # Touch down or pass abs(THETA_LIMIT)
            if done: break
