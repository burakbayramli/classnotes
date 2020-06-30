import pymunk, pygame, random, sys, numpy as np, pyglet
from pymunk.pygame_util import DrawOptions

width = 600
height = 600

class Dualcopter:
    def __init__(self, position, space):
        self.mass = 1
        self.shape = pymunk.Poly.create_box(None, size=(10, 50))
        self.moment = pymunk.moment_for_poly(self.mass, self.shape.get_vertices())
        self.body = pymunk.Body(self.mass, self.moment)
        self.shape.body = self.body
        self.shape.body.position = position
        space.add(self.shape, self.body)

class Ground:
    def __init__(self, space):
        self.body = pymunk.Body(0, 0, body_type=pymunk.Body.STATIC)
        self.shape = pymunk.Poly.create_box(self.body, (width, 10))
        self.shape.body.position = (width//2, 10)
        space.add(self.shape, self.body)

def main():

    pygame.init()
    screen = pygame.display.set_mode((width, height))
    pygame.display.set_caption("The ball drops")
    clock = pygame.time.Clock()

    draw_options = DrawOptions(screen)

    space = pymunk.Space()
    space.gravity = 0, -100
    x = random.randint(120, 380)
    ground = Ground(space)
    ball = Dualcopter((x, 550), space)
    thrust_angle = 0
    thrust = 0

    i = 0
    while True:
        i += 1
        #if i%20==0: pygame.image.save(screen, "/tmp/out-%d.jpeg" % i)
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit(0)
            elif event.type == pygame.KEYDOWN:
                print ('thrust',thrust,'angle',thrust_angle)
                if event.key == 274:
                    print ('down')
                    thrust -= 20
                elif event.key == 273:
                    print ('up')
                    thrust += 20
                elif event.key == 275:
                    print ('right')
                    thrust_angle += 10
                elif event.key == 276:
                    thrust_angle -= 10
                    print ('left')

        ball.shape.body.apply_force_at_local_point((0, thrust), (5, -25))
        ball.shape.body.apply_force_at_local_point((thrust*np.sin(np.deg2rad(thrust_angle)), thrust*np.cos(np.deg2rad(thrust_angle)) ), (5, 25))

        screen.fill((0, 0, 0))
        space.debug_draw(draw_options)
        space.step(1/50.0)
        pygame.display.update()
        clock.tick(50)


if __name__ == '__main__':
    sys.exit(main())
