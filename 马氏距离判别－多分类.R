# we need to share the variable 'label' out and in the function.
distance.mahalanobis = function(Train,Test,group,label,var.equal){
  nx = nrow(Test)
  n_group = length(label)
  mu = matrix(0,nrow = n_group,ncol=ncol(Train))
  for (i in 1:n_group){
    mu[i,] = colMeans(Train[group==label[i],])
  }
  
  D = matrix(0,nrow=n_group,ncol=nx)
  
  if (var.equal==TRUE){
    for (i in 1:n_group){
      D[i,] =  mahalanobis(Test,mu[i,],var(Train))
    } 
  }
  if (var.equal==FALSE){
    for (i in 1:n_group){
      D[i,] = mahalanobis(Test,mu[i,],var(Train[group==label[i],]))
    }
  }
  
  return(D)
}

### The function is to classify by mahalanobis distance.
### input:
### Train--a matrix of n*p where the group label is out.
### group--a vector of n*1 which is group label for Train.
### Test--a matrix of k*p which contains k samples to classify.
###       attention:Test needs to be the form of matrix even though k=1.
###       attention:if Test is null,the function is to classify the training data.
### var.equal--whether to consider the cov matrixs of G1,G2,...,Gk are equal or not.

### This function can also be applied in bi-classification.

discriminiant.distance.multi=function(Train, group, Test=NULL, var.equal=FALSE,distance.type='mahalanobis'){
  flag=0
  if (is.null(Test)==TRUE){
    flag=1
    Test = Train
  }
  nx = nrow(Test)
  label = levels(factor(group))
  n_group = length(label)

  if (distance.type=='mahalanobis'){
    D = distance.mahalanobis(Train,Test,group,label,var.equal)
  }
  
  classify = rep(0,nx)
  for (j in 1:nx){
    dmin = Inf
    for (i in 1:n_group){
      if (D[i,j]<dmin){
        dmin = D[i,j]
        classify[j] = label[i]
      }
    }
  }
  
  if (flag==1){
    output_test = data.frame(Test)
    output_test = cbind(output_test,group,classify)
    error_num = sum(output_test$group!=output_test$classify)
    error_rate = error_num/nx
    cat('error num:',error_num,' error rate:',error_rate,'\n')
    error_id = which(output_test$classify!=output_test$group)
    cat('The error samples are:\n')
    error_sample = output_test[error_id,]
    print(error_sample)
    
    error_matrix = matrix(0,nrow=n_group,ncol=n_group,dimnames =list(label,label))
    for (i in 1:n_group){
      for (j in 1:n_group){
        error_matrix[i,j] = sum((output_test$group==label[i])*(output_test$classify==label[j]))
      }
    }
    cat('\nThe error matrix is:\n')
    print(error_matrix)
  }
  if (flag==0){
    output_test = cbind(Test,classify)
    output_test = data.frame(output_test)
  }
  
  return(output_test)
  
  
  }
