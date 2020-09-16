load wind
figure()
load wind
quiver3(x,y,z,u,v,w);
view(-45,20)
set(gca, 'Color', 'black');
axis tight
axis equal
print -dpng /tmp/image.png
