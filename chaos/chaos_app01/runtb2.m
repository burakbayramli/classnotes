pkg load odepkg;
[t,z] = ode45(@twobody2,[0 25], [-1 0 0 -1 1 0 0 1]);

% Particle 1 orbit in xy space -- first 25 seconds
disp(z(:,1));
disp(z(:,3));


% Particle 2 orbit in xy space -- first 25 seconds
disp(z(:,5));
disp(z(:,7));
