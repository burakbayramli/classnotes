load ../wind
figure()
cav = curl(x,y,z,u,v,w);
slice(x,y,z,cav,[90 134],[59],[0]); 
shading interp
daspect([1 1 1]); axis tight
colormap (hot);
camlight
print -dpng /tmp/image.png
