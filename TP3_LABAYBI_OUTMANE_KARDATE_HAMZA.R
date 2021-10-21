library(FactoMineR) 
A=na.omit(airquality) 
res=RegBest(A[,1],A[,-1],method = "adjr2")$best
summary(res) 
res1=lm(Ozone~.,data = A); res2=lm(Ozone~Solar.R+Wind+Temp+Month,data = A) 
anova(res2,res1)
summary(res2)
plot(res2)

predict.lm(res2,newdata=data.frame(Solar.R=190,Wind=7.4,Temp=67,Month=5),interval =
               "prediction",level = 0.99, type = "response")

library(FactoMineR) ;
children
A=children[1:14,1:5] 
colnames(A)=c("Unq","cep","bepc","HSD","Univ") ;
rownames(A)[1:14]=1:14
A[2,]/sum(A[2,])
res=CA(A)
LQBC=rbind(res$row$coord[,1:2],res$col$coord[,1:2]/sqrt(res$eig[1:2,1]))
plot(LQBC,col=c(rep(1,14),rep(2,5)),pch=c(rep(1,14),rep(2,5)))
text(LQBC,labels = rownames(LQBC),col=c(rep(1,14),rep(2,5)))
res$eig
