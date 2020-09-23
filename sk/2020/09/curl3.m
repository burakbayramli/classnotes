function varargout = curl3 (x,y,z,u,v,w)

  dx = x(1,:,1)(:);
  dy = y(:,1,1)(:);
  dz = z(1,1,:)(:);

  [~, dFx_dy, dFx_dz] = gradient (u, dx, dy, dz);
  [dFy_dx, ~, dFy_dz] = gradient (v, dx, dy, dz);
  [dFz_dx, dFz_dy] = gradient (w, dx, dy, dz);
  rot_x = dFz_dy - dFy_dz;
  rot_y = dFx_dz - dFz_dx;
  rot_z = dFy_dx - dFx_dy;
  l = sqrt(u.^2 + v.^2 + w.^2);
  av = (rot_x .* u +
        rot_y .* v +
        rot_z .* w) ./ (2 * l);

  varargout{1} = rot_x;
  varargout{2} = rot_y;
  varargout{3} = rot_z;
  varargout{4} = av;

endfunction
