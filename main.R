setwd('/Users/zhangxiang/GitHub/判别分析')
source('/Users/zhangxiang/GitHub/判别分析/马氏距离判别－多分类.R')

group = iris$Species
data = as.matrix(subset(iris,select=-Species))
discriminiant.distance.multi(data,group,var.equal = FALSE)

source('/Users/zhangxiang/GitHub/判别分析/bayes判别.R')
bayes.classification(data,group,var.equal = FALSE)
