#creat chart
thickness<-matrix(c(
  437.83,429.42,404.17,401.35,411.96,411.64,409.03,405.99,
  87.77,90.48,105.45,88.88,102.37,99.31,92.54,91.19,
  308.84,346.66,383.16,329.45,361.33,359.91,311.25,330.05,
  69.21,72.57,75.16,70.65,73.66,72.39,71.04,72.97
  
  
),byrow = T,ncol = 8)
dimnames(thickness)<- list(c("1","2","3","4"),c("Run1","Run2","Run3","Run4","Run5","Run6","Run7","Run8"))
I<- rep(1,4)
A<- rep(c(-1,1),2)
B<- c(rep(-1,2),rep(1,2))
AB <- A*B
Total<- apply(thickness,1,sum)
Average<- apply(thickness,1,mean)
Variance<- apply(thickness,1,var)
table<-cbind(I,A,B,AB,thickness,Total,Average,Variance)
table
#################
#Effect estimate_method1
n<-8
Aeff <-(Total %*% A)/(2*n)
Aeff
Beff <-(Total %*% B)/(2*n)
Beff
ABeff <-(Total %*% AB)/(2*n)
ABeff
############
#Standard error of effect
k<-2
IndexStart<-1
i<-seq(IndexStart,2^k)
i
Variance
sigma_square_hat<-sum(Variance[i]/(2^k))
sigma_square_hat

se_effect <-sqrt((sigma_square_hat)/(n*2^(k-2)))
se_effect
#########
#t ratio
A_t_ratio <- Aeff/se_effect
A_t_ratio 
B_t_ratio <- Beff/se_effect
B_t_ratio
AB_t_ratio <- ABeff/se_effect
AB_t_ratio


#######
#Convert chart in to matrix in longformat
install.packages("tidyverse")
library(tidyverse)
d1 <- table %>%
  as.tibble() %>% 
  select(I, A, B, AB, Run1, Run2, Run3, Run4, Run5, Run6, Run7, Run8) %>% 
  gather(key = "Run", value = "thickness", -I, -A, -B, -AB) %>%      mutate(A = factor(A, levels = c(1, -1)),                         
                                                                            B =factor(B, levels = c(1, -1)), 
                                                                            AB = factor(AB, levels = c(1, -1)))
options(contrasts=c("contr.sum","contr.poly"))
d2 <-d1[c(-5)]
d2
####################
#Linear regression 
thickness.lm <- lm(thickness ~ A * B, data = d1)
thickness.lm

plot(cars$speed, cars$dist)
abline(thickness.lm)
#############
t.test(subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (A==+1)),subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (A==-1)), mu=0)
t.test(subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (B==+1)),subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (B==-1)), mu=0)
t.test(subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (AB==+1)),subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (AB==-1)), mu=0)



#creat chart
thickness<-matrix(c(
  437.83,429.42,404.17,401.35,411.96,411.64,409.03,405.99,
  87.77,90.48,105.45,88.88,102.37,99.31,92.54,91.19,
  1133.57,1141.02,1129.27,1121.45,1110.35,1088.08,1085.89,1076.45,
  213.74,212.35,210.20,215.86,212.13,211.78,210.98,211.68
  
  
),byrow = T,ncol = 8)
dimnames(thickness)<- list(c("1","2","3","4"),c("Run1","Run2","Run3","Run4","Run5","Run6","Run7","Run8"))
I<- rep(1,4)
A<- rep(c(-1,1),2)
B<- c(rep(-1,2),rep(1,2))
AB <- A*B
Total<- apply(thickness,1,sum)
Average<- apply(thickness,1,mean)
Variance<- apply(thickness,1,var)
table<-cbind(I,A,B,AB,thickness,Total,Average,Variance)
table
#################
#Effect estimate_method1
n<-8
Aeff <-(Total %*% A)/(2*n)
Aeff
Beff <-(Total %*% B)/(2*n)
Beff
ABeff <-(Total %*% AB)/(2*n)
ABeff
############
#Standard error of effect
k<-2
IndexStart<-1
i<-seq(IndexStart,2^k)
i
Variance
sigma_square_hat<-sum(Variance[i]/(2^k))
sigma_square_hat

se_effect <-sqrt((sigma_square_hat)/(n*2^(k-2)))
se_effect
#########
#t ratio
A_t_ratio <- Aeff/se_effect
A_t_ratio 
B_t_ratio <- Beff/se_effect
B_t_ratio
AB_t_ratio <- ABeff/se_effect
AB_t_ratio


#######
#Convert chart in to matrix in longformat
install.packages("tidyverse")
library(tidyverse)
d1 <- table %>%
  as.tibble() %>% 
  select(I, A, B, AB, Run1, Run2, Run3, Run4, Run5, Run6, Run7, Run8) %>% 
  gather(key = "Run", value = "thickness", -I, -A, -B, -AB) %>%      mutate(A = factor(A, levels = c(1, -1)),                         
                                                                            B =factor(B, levels = c(1, -1)), 
                                                                            AB = factor(AB, levels = c(1, -1)))
options(contrasts=c("contr.sum","contr.poly"))
d2 <-d1[c(-5)]
d2
####################
#Linear regression 
thickness.lm <- lm(thickness ~ A * B, data = d1)
thickness.lm

plot(cars$speed, cars$dist)
abline(thickness.lm)
#############
t.test(subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (A==+1)),subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (A==-1)), mu=0)
t.test(subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (B==+1)),subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (B==-1)), mu=0)
t.test(subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (AB==+1)),subset(table,select=c(Run1,Run2,Run3,Run4,Run5,Run6,Run7,Run8),subset = (AB==-1)), mu=0)







