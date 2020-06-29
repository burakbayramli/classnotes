import pymunk               # Import pymunk..

space = pymunk.Space()      # Create a Space which contain the simulation
space.gravity = 0,-1000     # Set its gravity

body = pymunk.Body(1,1666)  # Create a Body with mass and moment
body.position = 50,100      # Set the position of the body

poly = pymunk.Poly.create_box(body) # Create a box shape and attach to body
space.add(body, poly)       # Add both body and shape to the simulation

print_options = pymunk.SpaceDebugDrawOptions() # For easy printing

while True:                 # Infinite loop simulation
    space.step(0.02)        # Step the simulation one step forward
    space.debug_draw(print_options) # Print the state of the simulation
