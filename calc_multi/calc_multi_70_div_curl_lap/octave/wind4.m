figure()
load wind
%extract a portion of the volume
[x y z u v w] =...
subvolume(x,y,z,u,v,w,[105,120,nan,30,2,6]);
%compute the magnitude of the wind
wind_vel = sqrt(u.^2 + v.^2 + w.^2);
%slice at the extremities
lims=volumebounds(x,y,z,u,v,w);
slice(x,y,z,wind_vel,...
[lims(1),lims(2)],[lims(4)],[lims(5)])
%specify where to put cones
xrange = linspace(lims(1),lims(2),8);
yrange = linspace(lims(3),lims(4),8);
zrange = linspace(lims(5),lims(6),6);
[cx cy cz] = meshgrid(xrange,yrange,zrange);
coneplot(x,y,z,u,v,w,cx,cy,cz,wind_vel,1);
%pretty it up a bit
shading interp
axis equal 
print -dpng /tmp/image.png
