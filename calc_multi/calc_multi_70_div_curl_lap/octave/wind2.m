figure()
load ../wind

%idx = [2 4 6 8 10 12 14 16 18 20 22];
idx = [2  6  10  14  18  22];
idz = [2 4 6 8];

x = x(idx,idx,idz);y = y(idx,idx,idz);z = z(idx,idx,idz);
u = u(idx,idx,idz);v = v(idx,idx,idz);w = w(idx,idx,idz);

size(x)

quiver3(x,y,z,u,v,w);
view(-45,20)
set(gca, 'Color', 'black');
axis tight
axis equal
print -dpng /tmp/image.png
