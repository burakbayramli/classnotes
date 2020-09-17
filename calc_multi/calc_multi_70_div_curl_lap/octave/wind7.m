x = -3:0.5:3;
y = -3:0.5:3;
[X,Y] = meshgrid(x, y);
Z = Y.^2 - X.^2;
[U,V,W] = surfnorm(Z);
figure
quiver3(X,Y,Z,U,V,W)
view(-35,45)
print -dpng /tmp/image.png
