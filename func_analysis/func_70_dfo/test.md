#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

```python
import glob
b = "func_70_dfo.tex"
b = b[:-4]
print (b)
fs = glob.glob(b + '*.png')
plt_count_before =int(fs[-1][-6:-4])
print (plt_count_before)
```

```text
func_70_dfo
3
```


