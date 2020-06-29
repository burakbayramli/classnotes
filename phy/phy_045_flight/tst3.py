import math, pyglet, pymunk, time
from pyglet.gl import *
from pyglet.window import key, mouse
from pymunk import Vec2d
import pymunk.pyglet_util

class Main(pyglet.window.Window):
    def __init__(self):
        
        pyglet.window.Window.__init__(self, vsync=False)
        self.set_caption('Vertical stack from box2d')

        pyglet.clock.schedule_interval(self.update, 1/60.0)
        self.fps_display = pyglet.window.FPSDisplay(self)
        
        self.text = pyglet.text.Label('',font_size=10,x=10, y=400)
        self.create_world()
        
        self.draw_options = pymunk.pyglet_util.DrawOptions()
        self.draw_options.flags = self.draw_options.DRAW_SHAPES 

    def create_world(self):
        self.space = pymunk.Space()
        self.space.gravity = Vec2d(0.,-900.)
        #self.space.sleep_time_threshold = 0.3
        self.space.sleep_time_threshold = 2.0
        
        static_lines = [pymunk.Segment(self.space.static_body, Vec2d(20,55), Vec2d(600,55), 1),
                        pymunk.Segment(self.space.static_body, Vec2d(550,55), Vec2d(550,400), 1)
                        ]
        for l in static_lines:
            l.friction = 0.3
        self.space.add(static_lines)
        
        mass = 10.0
        moment = pymunk.moment_for_box(mass, (20, 200))
        body = pymunk.Body(mass, moment)
        body.position = Vec2d(300 , 505)
        shape = pymunk.Poly.create_box(body, (20, 100))
        shape.friction = 0.3
        self.space.add(body,shape)
                
        
    def update(self, dt):

        print ('---------------')
        for shape in self.space.shapes:
            if isinstance(shape, pymunk.Poly):
                print ('shape')
                body = shape.body
                print (body.position)
                
        step_dt = 1/250.
        x = 0
        while x < dt:
            x += step_dt
            self.space.step(step_dt)

    def on_draw(self):
        self.clear()
        self.text.draw()
        self.fps_display.draw()  
        self.space.debug_draw(self.draw_options)

    def on_key_press(self, symbol, modifiers):
        print (symbol)
        #arrow_body.apply_impulse_at_world_point(impulse, arrow_body.position)
        if symbol == 65361:
            arrow_body.apply_impulse_at_world_point(impulse, arrow_body.position)
            print ('left arrow')
        elif symbol == 65363:
            print ('right arrow')
        elif symbol == 65362:
            print ('up arrow')
        elif symbol == 65364:
            print ('down arrow')
        
        
if __name__ == '__main__':
    main = Main()
    pyglet.app.run()
