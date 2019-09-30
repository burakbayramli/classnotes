import sympy

vars = 't a0 a1 a2 a3 b0 b1 b2 b3 gamma x y m1x m1y m2x m2y'
t, a0, a1, a2, a3, b0, b1, b2, b3, gamma, x, y, m1x, m1y, m2x, m2y = sympy.symbols(vars)

tmp1 = sympy.Matrix([[x], [y]])-sympy.Matrix([[m1x], [m1y]])
tmp1 = tmp1.T.dot(tmp1)
tmp2 = sympy.Matrix([[x], [y]])-sympy.Matrix([[m2x], [m2y]])
tmp2 = tmp2.T.dot(tmp2)
f = sympy.Eq(0.5 * sympy.exp(-gamma * tmp1) + 0.5 * sympy.exp(-gamma * tmp2))

xdef = a0 + a1*t + a2*t**2 + a3*t**3 
ydef = b0 + b1*t + b2*t**2 + b3*t**3

dxdt = sympy.diff(xdef,t)
print (dxdt)
dydt = sympy.diff(ydef,t)
print (dydt)
sqrtdef = sympy.sqrt(sympy.diff(xdef,t)**2 + sympy.diff(ydef,t))
print (sqrtdef)

f2 = f.subs({x: xdef, y: ydef})
f3 = sqrtdef * f2
print (f3)

f4 = sympy.integrate(f3, (t, 0, 1))

