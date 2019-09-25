#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

```python
t = np.linspace(0,1.0,100)

a1,a2,a3 = 3.5, 1.0, 1.0
b1,b2,b3 = 9.5, 1.4, 4.0

x = 1.0 + np.sin(a1*t) + np.sin(a2*t**2) + np.sin(a2*t**3)
y = 1.0 + np.sin(b1*t) + np.sin(b2*t**2) + np.sin(b2*t**3)

plt.xlim(0,5.0)
plt.ylim(0,5.0)
plt.plot(x,y)
plt.savefig('out6.png')
```






















