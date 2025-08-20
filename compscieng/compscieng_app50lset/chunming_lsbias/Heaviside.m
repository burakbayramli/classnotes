function h = Heaviside(x,epsilon)    
h=0.5*(1+(2/pi)*atan(x./epsilon));