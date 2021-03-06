GRS.test <-
function(ret.mat,factor.mat){
  ret.mat=as.matrix(ret.mat); factor.mat=as.matrix(factor.mat)
  N=ncol(ret.mat); T=nrow(ret.mat); K = ncol(factor.mat)
  
  e.mat = matrix(NA,ncol=N,nrow=T)
  b.mat = matrix(NA,ncol=K+1,nrow=N)
  se.mat = matrix(NA,ncol=K+1,nrow=N)
  R2.mat = matrix(NA,ncol=1,nrow=N)
  t.mat = matrix(NA,ncol=K+1,nrow=N)
  p.mat = matrix(NA,ncol=K+1,nrow=N)
  
  one = matrix(1,nrow=T,ncol=1)
  dat = as.matrix(cbind( one,factor.mat))
  
  for(i in 1:N){
    ri = as.matrix(ret.mat[,i,drop=F])
    b = solve(t(dat) %*% dat) %*% t(dat) %*%  ri; 
    e = ri -  dat %*% b 
    b.mat[i,] =  b
    e.mat[,i] =  e
    R2.mat[i,] = 1-sum(e^2)/sum((ri-mean(ri))^2)
    
    s2=sum(e^2)/(T-K-1)
    covmat=s2*solve(t(dat) %*% dat)
    se.mat[i,] = sqrt(diag(covmat))
    t.mat[i,] = b/sqrt(diag(covmat))
    p.mat[i,]  = 2*pt(abs(b/sqrt(diag(covmat))),df=T-K-1,lower.tail=FALSE)
  }
  
  sigma = crossprod(e.mat)/(T-K-1); #sigma = crossprod(e.mat)/(T) Use Unbiased estimator as in GRS
  alpha = matrix(b.mat[,1],nrow=N)
  
  factor.mean = t(matrix(colMeans(factor.mat),nrow=K,ncol=T))
  omega = crossprod(factor.mat-factor.mean)/(T-1); #omega = crossprod(factor.mat-factor.mean)/(T) Use Unbiased estimator as in GRS
  
  tem1 = t(alpha) %*% solve(sigma) %*% alpha
  tem2 = 1 + factor.mean[1,, drop=FALSE] %*% solve(omega) %*% t(factor.mean[1,, drop=FALSE])
  tem3 = T/N; tem4 = (T-N-K)/(T-K-1)
  
  F = tem3*tem4 *(tem1/tem2)
  p.F = pf(F,df1=N,df2=T-N-K,lower.tail=FALSE)
  
  if(ncol(factor.mat) == 1) {factor.names = c("intercept","Singlefactor")} else {factor.names = c("intercept", colnames(factor.mat))}
  ret.names=colnames(ret.mat)
  colnames(F) = "GRS"; colnames(p.F) = "GRS"
  colnames(b.mat) = factor.names; rownames(b.mat) = ret.names; 
  colnames(e.mat) = ret.names; 
  colnames(t.mat) = factor.names; rownames(t.mat) = ret.names; 
  colnames(p.mat) = factor.names; rownames(p.mat) = ret.names; 
  colnames(se.mat) = factor.names; rownames(se.mat) = ret.names; 
  rownames(R2.mat) = ret.names; 
  
  
  return(list(GRS.stat=F,GRS.pval=p.F,coef=b.mat,resid=e.mat,tstat=t.mat,pval=p.mat,se=se.mat,R2=R2.mat))
}
