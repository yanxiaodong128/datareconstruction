
#####################################################
#Theorem1
#############################################################
 library(nleqslv)  
 FindRoot = function(x,gamma1,gamma2,alpha) {
  return(pnorm(x-gamma2)-exp(2*gamma2*x)*pnorm(-x-gamma2)-1+alpha)
}

logf_ml=function(z,theta){
sum(-log(exp(-(abs(z-theta[2])+theta[1])^2/2)/sqrt(2*pi)+
theta[1]*exp(-2*theta[1]*abs(z-theta[2]))*pnorm(-abs(z-theta[2])+theta[1])))
}

 
MAX<-5000
max<-1000
Qn1<-rep(0,max)
Qn2<-rep(0,max)
Times1<-rep(0,max)
Times2<-rep(0,max)
n<-10
mu1<-1
mu2<--1
Z_alpha<-nleqslv(1, FindRoot, control=list(btol=.000001),jacobian=TRUE,method="Newton",gamma1=gamma1,gamma2=mu2,alpha= 0.05)$x
sigma<-1
dd<-n  



MM<-200
MU<-array(0,dim=c(MM,4))
for(jk in 1:MM){
x<-rnorm(n,mu1,sigma)
y<-rnorm(n,mu2,sigma)  
for(k in 1:max){
    
    mmu1<-mean(x)
    mmu2<-mean(y)
    sigmahat<-sqrt(mean((c(x-mean(x),y-mean(y)))^2))
    Tn1<-rep(0,MAX)
    Tn2<-rep(0,MAX)
    Tn3<-rep(0,MAX)
    Tn4<-rep(0,MAX)
    for(j in 1:MAX){
    if(((sum(Tn2[1:j]+Tn4[1:j]))/((MAX))+sum(Tn1[1:j]+Tn3[1:j])/(sqrt(MAX)*sigmahat))<=0){
    xx<-sample(x,1)
    #mmu1<-sum(Tn2[1:j])/j
    Tn1[j]<-(xx-mmu1)
    Tn2[j]<-xx
    }else{
     yy<-sample(y,1)
    #mmu2<-sum(Tn4[1:j])/j
    Tn3[j]<-(yy-mmu2)
    Tn4[j]<-yy}         
   }
   Qn2[k]<-sum(Tn2+Tn4)/((MAX))+sum(Tn1+Tn3)/(sqrt(MAX)*sigmahat)
   Qn1[k]<-sum(Tn1+Tn3)/(sqrt(MAX)*sigma*sqrt(1/n))
   Times1[k]<-(abs(Qn2[k])>Z_alpha)+0
   Times2[k]<-(abs(Qn1[k])>1.96)+0    
 }


theta0<-c(0,0)
theta2<-optim(theta0, logf_ml,z=c(Qn2))$par
bigmu<-theta2[1]+theta2[2]
smallmu<-theta2[2]-theta2[1]
MU[jk,1]<-mean(x)
MU[jk,2]<-mean(y)
MU[jk,3]<-bigmu
MU[jk,4]<-smallmu
print(jk)
}



bias<-MU-matrix(rep(c(mu1,mu2,mu1,mu2),MM),ncol=4,byrow=TRUE)
mean(sqrt(apply((bias^2)[,1:2],1,mean)))
mean(sqrt(apply((bias^2)[,3:4],1,mean)))
boxplot(bias)




