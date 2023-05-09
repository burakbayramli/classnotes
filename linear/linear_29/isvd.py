# Wingate'in matlab kodlarindan tercume edilmistir, matlab dizini
# altinda orijinal kodlar var
# http://pcc.byu.edu/resources.html
import numpy as np
import scipy.linalg as lin

def  addblock_svd_update( Uarg, Sarg, Varg, Aarg, force_orth = False):
  U = Varg
  V = Uarg
  S = np.eye(len(Sarg),len(Sarg))*Sarg
  A = Aarg.T
  
  current_rank = U.shape[1]
  m = np.dot(U.T,A)
  p = A - np.dot(U,m)
  P = lin.orth(p)
  Ra = np.dot(P.T,p)
  z = np.zeros(m.shape)
  K = np.vstack(( np.hstack((S,m)), np.hstack((z.T,Ra)) ))
  tUp,tSp,tVp = lin.svd(K);
  tUp = tUp[:,:current_rank]
  tSp = np.diag(tSp[:current_rank])
  tVp = tVp[:,:current_rank]
  Sp = tSp
  Up = np.dot(np.hstack((U,P)),tUp)
  Vp = np.dot(V,tVp[:current_rank,:])
  Vp = np.vstack((Vp, tVp[current_rank:tVp.shape[0], :]))
  
  if force_orth:
    UQ,UR = lin.qr(Up,mode='economic')
    VQ,VR = lin.qr(Vp,mode='economic')
    tUp,tSp,tVp = lin.svd( np.dot(np.dot(UR,Sp),VR.T));
    tSp = np.diag(tSp)
    Up = np.dot(UQ,tUp)
    Vp = np.dot(VQ,tVp)
    Sp = tSp;

  Up1 = Vp;
  Vp1 = Up;
    
  return Up1,Sp,Vp1


