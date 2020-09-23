function [Up,Sp,Vp] = addblock_svd_update( U, S, V, A, force_orth )
% function [Up,Sp,Vp] = addblock_svd_update( U, S, V, A, force_orth )
%
% Given the SVD of
%
%   X = U*S*V'
%
% update it to be the SVD of
%
%   [X A] = Up*Sp*Vp'
%
% that is, add new columns (ie, data points).
%
% I have found that it is faster to add several (say, 200) data points
% at a time, rather than updating it incrementally with individual
% data points (for 500 dimensional vectors, the speedup was roughly
% 25x).  However, in the rank-one case there is structure that I have
% not exploited, so that may still be faster than a block method.
%
% The subspace rotations involved may not preserve orthogonality due
% to numerical round-off errors.  To compensate, you can set the
% "force_orth" flag, which will force orthogonality via a QR plus
% another SVD.  In a long loop, you may want to force orthogonality
% every so often.
%
% See Matthew Brand, "Fast low-rank modifications of the thin
% singular value decomposition".
%
% D. Wingate 8/17/2007
% http://pcc.byu.edu/resources.html
%

  current_rank = size( U, 2 );

  % P is an orthogonal basis of the column-space
  % of (I-UU')a, which is the component of "a" that is
  % orthogonal to U.
  m = U' * A;
  p = A - U*m;

  P = orth( p );
  % p may not have full rank.  If not, P will be too small.  Pad
  % with zeros.
  P = [ P zeros(size(P,1), size(p,2)-size(P,2)) ];

  Ra = P' * p;

  %  
  % Diagonalize K, maintaining rank
  %

  z = zeros( size(m) );

  K = [ S m ; z' Ra ];

  [tUp,tSp,tVp] = svds( K, current_rank );

  %
  % Now update our matrices!
  %

  Sp = tSp;

  Up = [ U P ] * tUp;   % this may not preserve orthogonality over
                        % many repetitions.  See below.

  % Exploit structure to compute this fast: Vp = [ V Q ] * tVp;
  if ( ~isempty(V) )
    Vp = V * tVp( 1:current_rank, : );
  else
    Vp = [];
  end;
  Vp = [ Vp ; tVp( current_rank+1:size(tVp,1), : ) ];

  % The above rotations may not preserve orthogonality, so we explicitly
  % deal with that via a QR plus another SVD.  In a long loop, you may
  % want to force orthogonality every so often.

  if ( force_orth )
    [UQ,UR] = qr( Up, 0 );
    [VQ,VR] = qr( Vp, 0 );
    [tUp,tSp,tVp] = svds( UR * Sp * VR', current_rank );
    Up = UQ * tUp;
    Vp = VQ * tVp;
    Sp = tSp;
  end;
  
return;
