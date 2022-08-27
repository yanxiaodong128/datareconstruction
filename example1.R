
#####################################################
#####Data Reconstruction
#####Data Reconstruction
#####Data Reconstruction
#####Data Reconstruction
#####Data Reconstruction
#####Data Reconstruction
#####Data Reconstruction
#############################################################
 library(nleqslv)  
 FindRoot = function(x,gamma1,gamma2,alpha) {
  return(pnorm(x-gamma2)-exp(2*gamma2*x)*pnorm(-x-gamma2)-1+alpha)
}

 
MAX<-5000
max<-1000
Qn1<-rep(0,max)
Qn2<-rep(0,max)
Times1<-rep(0,max)
Times2<-rep(0,max)
n<-100
mu1<-0.8
mu2<--0.8
Z_alpha<-nleqslv(1, FindRoot, control=list(btol=.000001),jacobian=TRUE,method="Newton",gamma1=gamma1,gamma2=mu2,alpha= 0.05)$x
sigma<-1
dd<-n  
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

densityf<-function(y,alpha,c){
exp(-(y^2-2*alpha*(abs(y-c)-abs(c))+alpha^2)/2)/sqrt(2*pi)-
alpha*exp(2*alpha*abs(y-c))*(1-pnorm(abs(c)+abs(y-c)+alpha,0,1))
}   
YY<-seq(-5,5,length=100)
Alpha<-mu2
res<-array(0,dim=c(length(YY),length(Alpha)))
alpha<-minimean
c<-0
for(j in 1:length(YY)){
for(k in 1:length(Alpha))
res[j,k]<-densityf(YY[j],Alpha[k],0)
}

plot(YY,res,type="l",ylab="density",xlab="",col="blue")
lines(density(Qn2),ylab="density",col="red",xlab="")
mean(Times1)
mean(Times2)


