figure()
load wind, wind_speed=sqrt(u.^2+v.^2+w.^2);
wind_vel = sqrt(u.^2 + v.^2 + w.^2);
slice(x,y,z,wind_vel,[80,90,100,110,120],Inf,Inf)
axis equal
shading interp 
print -dpng /tmp/image.png
