#!/usr/bin/env python3

"""Double pendulum simulator based on its Euler-Lagrange equations."""
# The double pendulum: Lagrangian formulation - Diego Assencio
import math
import numpy


class DoublePendulumLagrangian:

    def __init__(self, g, m1, m2, t1, t2, w1, w2, L1, L2):
        """
        Constructs a double pendulum simulator based on its
        Euler-Lagrange equations. Bob #1 is the one attached to the
        fixed pivot.

        g - The gravitational acceleration.
        m1 - The mass of bob #1.
        m2 - The mass of bob #2.
        t1 - The initial angle of bob #1.
        t2 - The initial angle of bob #2.
        w1 - The initial angular velocity of bob #1.
        w2 - The initial angular velocity of bob #2.
        L1 - The length of the rod for bob #1.
        L2 - The length of the rod for bob #2.
        """

        self.g = g
        self.m1 = m1
        self.m2 = m2
        self.t1 = t1
        self.t2 = t2
        self.w1 = w1
        self.w2 = w2
        self.L1 = L1
        self.L2 = L2

    def potential_energy(self):
        """Computes the potential energy of the system."""

        m1 = self.m1
        t1 = self.t1
        L1 = self.L1
        m2 = self.m2
        t2 = self.t2
        L2 = self.L2

        g = self.g

        # compute the height of each bob
        y1 = -L1 * math.cos(t1)
        y2 = y1 - L2 * math.cos(t2)

        return m1 * g * y1 + m2 * g * y2

    def kinetic_energy(self):
        """Computes the kinetic energy of the system."""

        m1 = self.m1
        t1 = self.t1
        w1 = self.w1
        L1 = self.L1
        m2 = self.m2
        t2 = self.t2
        w2 = self.w2
        L2 = self.L2

        # compute the kinetic energy of each bob
        K1 = 0.5 * m1 * (L1 * w1)**2
        K2 = 0.5 * m2 * ((L1 * w1)**2 + (L2 * w2)**2 +
                         2 * L1 * L2 * w1 * w2 * math.cos(t1 - t2))

        return K1 + K2

    def mechanical_energy(self):
        """
        Computes the mechanical energy (total energy) of the
        system.
        """

        return self.kinetic_energy() + self.potential_energy()

    def lagrange_rhs(self, t1, t2, w1, w2):
        """
        Computes the right-hand side of the Euler-Lagrange equations
        for the double pendulum and returns it as an array.

        t1 - The angle of bob #1.
        t2 - The angle of bob #2.
        w1 - The angular velocity of bob #1.
        w2 - The angular velocity of bob #2.
        """

        m1 = self.m1
        L1 = self.L1
        m2 = self.m2
        L2 = self.L2

        g = self.g

        a1 = (L2 / L1) * (m2 / (m1 + m2)) * math.cos(t1 - t2)
        a2 = (L1 / L2) * math.cos(t1 - t2)

        f1 = -(L2 / L1) * (m2 / (m1 + m2)) * (w2**2) * math.sin(t1 - t2) - \
            (g / L1) * math.sin(t1)
        f2 = (L1 / L2) * (w1**2) * math.sin(t1 - t2) - (g / L2) * math.sin(t2)

        g1 = (f1 - a1 * f2) / (1 - a1 * a2)
        g2 = (f2 - a2 * f1) / (1 - a1 * a2)

        return numpy.array([w1, w2, g1, g2])

    def time_step(self, dt):
        """
        Advances one time step using RK4 (classical Runge-Kutta
        method).
        """

        m1 = self.m1
        t1 = self.t1
        w1 = self.w1
        L1 = self.L1
        m2 = self.m2
        t2 = self.t2
        w2 = self.w2
        L2 = self.L2

        # y is an array with the generalized coordinates (angles +
        # angular velocities)
        y = numpy.array([t1, t2, w1, w2])

        # compute the RK4 constants
        k1 = self.lagrange_rhs(*y)
        k2 = self.lagrange_rhs(*(y + dt * k1 / 2))
        k3 = self.lagrange_rhs(*(y + dt * k2 / 2))
        k4 = self.lagrange_rhs(*(y + dt * k3))

        # compute the RK4 right-hand side
        R = 1.0 / 6.0 * dt * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

        # update the angles and angular velocities
        self.t1 += R[0]
        self.t2 += R[1]
        self.w1 += R[2]
        self.w2 += R[3]
