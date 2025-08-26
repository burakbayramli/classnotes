function f=normalize01(f);
% Normalize to the range of [0,1]

fmin  = min(f(:));
fmax  = max(f(:));
f = (f-fmin)/(fmax-fmin);  % Normalize f to the range [0,1]