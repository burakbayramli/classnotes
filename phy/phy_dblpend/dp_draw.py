#!/usr/bin/env python3

"""Drawing function for a double pendulum system."""

import math
import numpy
import pygame


def draw(S, window, Nx, Ny, dt):
    """
    Draws the double pendulum system on a window.

    S - The double pendulum object.
    window - The window where the double pendulum will be shown.
    Nx - The window width (in pixels).
    Ny - The window height (in pixels).
    dt - The simulation time step.
    """

    m1 = S.m1
    m2 = S.m2
    t1 = S.t1
    t2 = S.t2
    L1 = S.L1
    L2 = S.L2

    # radius (in pixels) of each bob (min/max: 3/12 pixels)
    R1 = max(3, int(12 * (m1 / (m1 + m2))))
    R2 = max(3, int(12 * (m2 / (m1 + m2))))

    # length (in pixels) of each rod
    P1 = 0.85 * min(Nx / 2, Ny / 2) * (L1 / (L1 + L2))
    P2 = 0.85 * min(Nx / 2, Ny / 2) * (L2 / (L1 + L2))

    # positions (in (pixels,pixels)) of each bob
    X0 = numpy.array([int(Nx / 2), int(Ny / 2)])
    X1 = X0 + numpy.array([int(P1 * math.sin(t1)), int(P1 * math.cos(t1))])
    X2 = X1 + numpy.array([int(P2 * math.sin(t2)), int(P2 * math.cos(t2))])

    # color: rods and bobs
    color_L1 = (255, 255, 255)
    color_L2 = (128, 128, 128)
    color_m1 = (255, 0, 0)
    color_m2 = (0, 0, 255)

    # clear the window
    window.fill((0, 0, 0))

    # draw the rods and the bobs
    pygame.draw.line(window, color_L1, X0, X1, 3)
    pygame.draw.line(window, color_L2, X1, X2, 3)
    pygame.draw.circle(window, color_m1, X1, int(R1))
    pygame.draw.circle(window, color_m2, X2, int(R2))

    # write the time step value on the window
    myfont = pygame.font.SysFont("Arial", 15)
    label = myfont.render("dt = %.3g" % dt, 1, (128, 128, 128))
    window.blit(label, (10, 10))

    # update the screen
    pygame.display.flip()
