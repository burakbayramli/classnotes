#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

```python
xmax = 10.
xmin = -10.
D = 20
x = np.linspace(xmin, xmax, D)
plt.xlim(-10,10)
plt.ylim(-10,10)

for i,t in enumerate(np.linspace(-3., 3., 30)):
    if i % 4 == 0: # her dort resimden birini sec
        plt.plot (x,((2. - x)/2.))
        xx=2*np.cos(t)**2
        yy=np.sin(t)**2
        plt.quiver(0,0,xx,yy)
        plt.plot(xx,yy,'rd')
        plt.savefig('1i3_' + str(i) + '.png')
        plt.figure()
```



