source('/Users/zhangxiang/GitHub/判别分析/bayes.method.R')

### The function is to classify by bayes classification.
### input:
### Train--a matrix of n*p where the group label is out.
### group--a vector of n*1 which is group label for Train.
### Test--a matrix of k*p which contains k samples to classify.
###       attention:Test needs to be the form of matrix even though k=1.
###       attention:if Test is null,the function is to classify the training data.
### var.equal--whether to consider the cov matrixs of G1,G2,...,Gk are equal or not.


bayes.classification = function(Train,group,Test=NULL,var.equal=FALSE,method='posterior'){
  flag = 0
  if (is.null(Test)==TRUE){
    flag = 1
    Test = Train
  }
  nx = nrow(Test)
  label = levels(factor(group))
  m = length(label)
  
  p = rep(1,m)
  
  if (method=='posterior'){
    P = posterior.probability(Train,group,Test,p,label,'normal',var.equal)
  }
  
  classify = rep(0,nx)
  for (j in 1:nx){
    pmax = -Inf
    for (i in 1:m){
      if (P[i,j]>pmax){
        pmax = P[i,j]
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
    
    error_matrix = matrix(0,nrow=m,ncol=m,dimnames =list(label,label))
    for (i in 1:m){
      for (j in 1:m){
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




