###This function is to do calculations for the posterior probability classification.
###input:
### Train--a matrix of n*p where the group label is out.
### group--a vector of n*1 which is group label for Train.
### Test--a matrix of k*p which contains k samples to classify.
###       attention:Test needs to be the form of matrix even though k=1.
### p--the proior probability for G1,G2,...,Gm.
### label--a vector of g contained the group name corresponding to p.
### population--the types of distributions of population G1,G2,...,Gm.
### var.equal--whether to consider the cov matrixs of G1,G2,...,Gm are equal or not.


posterior.probability = function(Train,group,Test,p,label,population='normal',var.equal){
  nx = nrow(Test)
  m = length(label)
  P = matrix(0, nrow=m, ncol=nx, dimnames = list(label,NULL))
  
  if (population=='normal'){
    
    mu=matrix(0, nrow=m, ncol=ncol(Train))   
    for (i in 1:m)
    {
      mu[i,]=colMeans(Train[group==label[i],])
    }
    
    if (var.equal==TRUE){
      for (i in 1:m){
        d2 = mahalanobis(Test,mu[i,],var(Train))
        P[i,] = log(p[i]) - 1/2*d2
        #P[i,] corresponding to data of label[i]
        #p[i] correspongding to label[i
        #P[,j] depend on the order of Test.
      }
    }
    if (var.equal==FALSE){
      for (i in 1:m){
        S = var(Train[group==label[i],])
        d2 = mahalanobis(Test,mu[i,],S)
        P[i,] = log(p[i])-1/2*log(det(S))-1/2*d2
      }
    }
  }
  
  return(P)
}


ECM = function(Train,group,Test,p,label,population='normal',var.equal){
  #to be continued.
}