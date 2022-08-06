library(glmnet)
data("trees")
attach(trees)
summary(trees)
trees[1:5,]
Girth_Height=trees$Girth*trees$Height
Girth_sq=trees$Girth^2
Girth2_Height=trees$Girth^2*trees$Height
trees[,"Girth_Height"]=trees$Girth^2
trees[,"Girth2"]=Girth_sq
trees[,"Girth2_Height"]=Girth2_Height
names(trees)
trees2=trees[,c(1,2,4,5,6)]
trees2
X=data.matrix(trees2)

trees2=trees[,c(1,2,3,4,5,6)]
trees2
x=model.matrix(Volume~.,data=trees2)[,-1]
x
y=trees[,3]
REGfit = lm(y~x)  
summary(REGfit)  
#fit penalized regression using all predictors
library(glmnet)  # may need to download package glmnet
lambdalist = exp((1200:-1200)/100)  # order large to small
#fit ridge regression - need alpha = 0
RRfit = glmnet(x, y, alpha = 0,lambda=lambdalist)
coef(RRfit,s=0.1)
plot(RRfit,xvar="lambda",xlim=c(-12,12)); abline(v=log(0.1))
#fit LASSO - need alpha =1
LASSOfit = glmnet(x, y, alpha = 1,lambda=lambdalist)
coef(LASSOfit,s=0.5)
plot(LASSOfit,xvar="lambda"); abline(v=log(0.5))
#fit ENET
ENETfit = glmnet(x, y, alpha = 0.75,lambda=lambdalist)
coef(ENETfit,s=0.4)
plot(ENETfit,xvar="lambda"); abline(v=log(0.4))
cbind(coef(RRfit,s=.1)[,1],coef(LASSOfit,s=.5)[,1],coef(ENETfit,s=.4)[,1])
RRyhat = predict(RRfit,newx=x,s=.1); LASSOyhat = predict(LASSOfit,newx=x,s=.5); ENETyhat = predict(ENETfit,newx=x,s=.4)
round(cbind(RRyhat,LASSOyhat,ENETyhat,y),2)
# Step 2 Grid Selection
library(glmnet)

lambdalist = exp((1200:-1200)/100)  # order large to small
y
k=10
nfolds=ncv=k
n=dim(x)[1]
#using 10-fold cross-validation we need to create indexes for entire data set 
groups = c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))  #produces list of group labels
groups
set.seed(4)
cv_sample= sample(groups,n)  #orders randomly, with seed (2) to determine starting point
cv_sample

#RR cross-validation
model_rr_cv = cv.glmnet(x, y, lambda=lambdalist, alpha = 0, nfolds=ncv, foldid=cv_sample)
plot(model_rr_cv$lambda,model_rr_cv$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
     xlim=c(0,10),ylim = c(20,30))
order(model_rr_cv$cvm)
list=c(4,2,10,4,5)
min(list)
order(list)
whichlowestcvRR = order(model_rr_cv$cvm)[1]; min(model_rr_cv$cvm)
bestlambdaRR = lambdalist[whichlowestcvRR]; bestlambdaRR
abline(v=bestlambdaRR)
#LASSO cross-validation
cvLASSOglm = cv.glmnet(x, y, lambda=lambdalist, alpha = 1, nfolds=ncv, foldid=cvgroups)
plot(cvLASSOglm$lambda,cvLASSOglm$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
     xlim=c(0,3),ylim = c(20,30))
whichlowestcvLASSO = order(cvLASSOglm$cvm)[1]; min(cvLASSOglm$cvm)
bestlambdaLASSO = lambdalist[whichlowestcvLASSO]; bestlambdaLASSO
abline(v=bestlambdaLASSO)

#ENET alpha=0.95 cross-validation
cvENET95glm = cv.glmnet(x, y, lambda=lambdalist, alpha = 0.95, nfolds=ncv, foldid=cv_sample)
plot(cvENET95glm$lambda,cvENET95glm$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)")
whichlowestcvENET95 = order(cvENET95glm$cvm)[1]; min(cvENET95glm$cvm)
bestlambdaENET95 = lambdalist[whichlowestcvENET95]; bestlambdaENET95; abline(v=bestlambdaENET95)

#ENET alpha=0.5 cross-validation
cvENET50glm = cv.glmnet(x, y, lambda=lambdalist, alpha = 0.50, nfolds=ncv, foldid=cv_sample)
plot(cvENET50glm$lambda,cvENET50glm$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
     xlim=c(0,5),ylim = c(0,30))
whichlowestcvENET50 = order(cvENET50glm$cvm)[1]; min(cvENET50glm$cvm)
bestlambdaENET50 = lambdalist[whichlowestcvENET50]; bestlambdaENET50; abline(v=bestlambdaENET50)
bestlambdaENET50
#fit selected model




#Estimating standard deviations with bootstrap
library(boot)  
#define functions that output coefficients (parameters to be estimated)

beta.fn.Full = function(inputdata,index) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  lmfitboot = lm(yboot~xboot)
  return(lmfitboot$coef)
}
#run the boot function to simulate re-samples (with replacement)
#and obtain the coefficients for each re-sample
#partial model bootstrap
set.seed(5)
Fullbootoutput = boot(cbind(y,x),beta.fn.Full,R=10000)
print(Fullbootoutput)
#(Fullbootoutput$t)[,1] is all 1000 coefficient estimates for intercept (1st term)




Bestfit = glmnet(x, y, alpha = 0.50,lambda=lambdalist)
coef(Bestfit,s=bestlambdaENET50)
plot(Bestfit,xvar="lambda"); abline(v=log(bestlambdaENET50))
plot(Bestfit)
