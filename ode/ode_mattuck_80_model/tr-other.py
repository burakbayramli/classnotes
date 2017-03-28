from pylab import *

pdata = loadtxt('tr-short.txt')
pdiff = diff(pdata[:, 1])

pratios = pdiff / pdata[0:-1, 1]

p = polyfit(pdata[0:-1, 1], pratios, 1)

r = p[1]
K = -r/p[0]
print K

prev = pdata[-1,1] # en sonuncu veri noktasini al
prevy = pdata[-1,0]

for i in arange(4):
    curr = prev + (r * prev)*(1-(prev/K))
    curry = prevy + 5
    print(curry, curr)
    prevy=curry
    prev=curr
