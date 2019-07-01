#!/usr/bin/env python3
# The double pendulum: Lagrangian formulation - Diego Assencio
import dp_draw
import getopt
import os
import pygame
import sys


def print_usage():
    """Prints usage instructions to stderr and exits."""

    output = "Usage: %s [OPTIONS]\n\n" % os.path.basename(__file__)
    output += "    -h, --help                    "
    output += "prints these instructions\n"
    output += "    -v, --verbose                 "
    output += "activates verbose mode\n"
    output += "    -H, --hamiltonian             "
    output += "runs simulation using Hamilton's equations\n"
    output += "    -g, --gravity=ACCEL           "
    output += "sets the gravitational acceleration\n"
    output += "    -s, --time-step=STEP          "
    output += "sets the simulation time step\n"
    output += "    -m, --mass=MASS1,MASS2        "
    output += "sets the mass of each bob\n"
    output += "    -t, --theta=THETA1,THETA2     "
    output += "sets the initial angle of each bob\n"
    output += "    -w, --omega=OMEGA1,OMEGA2     "
    output += "sets the initial angular velocity of each bob\n"
    output += "    -L, --rodlen=LEN1,LEN2        "
    output += "sets the rod length for each bob\n"
    output += "        --geometry=WIDTH,HEIGHT   "
    output += "sets the window dimensions\n\n"
    output += "Keyboard shortcuts:\n"
    output += "    Up Arrow      increases the time step\n"
    output += "    Down Arrow    decreases the time step\n"
    output += "    v             toggles verbose mode\n"
    sys.stderr.write(output)
    sys.exit(0)


def print_error(errmsg):
    """Prints an error message and exits with an error code (1)."""

    sys.stderr.write("Error: %s\n" % errmsg)
    sys.exit(1)


def main():

    # default simulation parameter values
    g = 10
    dt = 0.01
    m1 = 10.0
    m2 = 20.0
    t1 = t2 = 1.5
    w1 = w2 = 0.0
    L1 = L2 = 1.0

    # default window dimensions
    Nx = Ny = 500

    # process the input parameters
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hvHm:t:w:L:s:g:",
                                   ["help", "verbose", "mass=", "theta=",
                                    "omega=", "rodlen=", "time-step=",
                                    "gravity=", "geometry=", "hamiltonian"])
    except getopt.GetoptError as err:
        print_error(str(err))

    verbose = False

    for opt, arg in opts:
        if opt == "-v":
            verbose = True
        elif opt in ("-H", "--hamiltonian"):
            lagrangian = False
        elif opt in ("-h", "--help"):
            print_usage()
        elif opt in ("-m", "--mass"):
            try:
                (m1, m2) = map(lambda x: float(x), tuple(arg.split(",")))
                if m1 <= 0 or m2 <= 0:
                    raise ValueError
            except:
                print_error("invalid mass values (masses must be positive)")
        elif opt in ("-t", "--theta"):
            try:
                (t1, t2) = map(lambda x: float(x), tuple(arg.split(",")))
            except:
                print_error("invalid initial angle values")
        elif opt in ("-w", "--omega"):
            try:
                (w1, w2) = map(lambda x: float(x), tuple(arg.split(",")))
            except:
                print_error("invalid initial angular velocity values")
        elif opt in ("-L", "--rodlen"):
            try:
                (L1, L2) = map(lambda x: float(x), tuple(arg.split(",")))
                if L1 <= 0 or L2 <= 0:
                    raise ValueError
            except:
                print_error(
                    "invalid rod length values (rod lengths must be positive)")
        elif opt in ("-s", "--time-step"):
            try:
                dt = float(arg)
                if dt <= 0:
                    raise ValueError
            except:
                print_error(
                    "invalid time step value (time step must be positive)")
        elif opt in ("-g", "--gravity"):
            try:
                g = float(arg)
            except:
                print_error("invalid gravitational acceleration value")
        elif opt in ("--geometry"):
            try:
                (Nx, Ny) = map(lambda x: int(x), tuple(arg.split(",")))
                if Nx <= 0 or Ny <= 0:
                    raise ValueError
            except:
                print_error(
                    "invalid window dimensions (dimensions must be positive)")
        else:
            print_usage()

    from dp_lagrangian import DoublePendulumLagrangian
    S = DoublePendulumLagrangian(g, m1, m2, t1, t2, w1, w2, L1, L2)

    # E0 = initial mechanical energy of the system
    E0 = S.mechanical_energy()

    step = 0

    # maximum energy change (compared to E0): too large => unstable simulation
    max_dE = 0

    pygame.init()

    clock = pygame.time.Clock()

    # create the output window
    window = pygame.display.set_mode((Nx, Ny), pygame.RESIZABLE)

    pygame.display.set_caption("double pendulum")

    # keep running the simulation until the user closes the window
    while True:

        dp_draw.draw(S, window, Nx, Ny, dt)

        # limit the while loop to a max of 25 times per second.
        clock.tick(25)

        # advance one time step
        S.time_step(dt)

        if verbose:
            Et = S.mechanical_energy()
            max_dE = max(abs(Et - E0), max_dE)
            line = "[%u] t = %f   Et = %f   E0 = %f   |Et - E0| = %f   " + \
                   "|Et - E0|_max = %f\n"
            sys.stdout.write(line % (
                step, step * dt, Et, E0, abs(Et - E0), max_dE
            ))

        step += 1

        # check window events: quit, resize, key presses
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            elif event.type == pygame.VIDEORESIZE:
                (Nx, Ny) = event.size
                window = pygame.display.set_mode((Nx, Ny), pygame.RESIZABLE)
            elif event.type == pygame.KEYDOWN:
                if event.unicode == u"v":
                    verbose = not verbose

        # the up and down arrow keys increase and decrease dt respectively
        pressed_keys = pygame.key.get_pressed()
        if pressed_keys[273]:
            dt *= 1.05
        if pressed_keys[274]:
            dt /= 1.05


if __name__ == "__main__":
    main()
