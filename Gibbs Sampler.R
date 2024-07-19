library(GA)

n=1000
mu1=2
mu2=3
sigma1=1
sigma2=1
rho=0.8

x1=c()
x2=c()

x1[1]=3
x2[1]=3

Mu1=function(x2){
  mu1+(sigma1/sigma2)*rho*(x2-mu2)
}

Sigma1=function(sigma1){
  sqrt(sigma1^2*(1-rho^2)) 
}

Mu2=function(x1){
  mu2+(sigma2/sigma1)*rho*(x1-mu1)
}

Sigma2=function(sigma2){
  sqrt(sigma2^2*(1-rho^2)) 
}

i=1
while(i<n){
  x1[i+1]=rnorm(1,Mu1(x2[i]),Sigma1(sigma1))
  x2[i+1]=rnorm(1,Mu2(x1[i]),Sigma2(sigma2))
  i=i+1
}
D=data.frame(x1,x2)
dim(D)

sigma12=rho

x1=sort(x1)
x2=sort(x2)

z=function(x1,x2){
  exp(-(sigma2*(x1-mu1)^2+sigma1*(x2-mu2)^2-2*sigma12*(x1-mu1)*(x2-mu2))/(2*(sigma1*sigma2-sigma12^2)))/(2*pi*sqrt(sigma1*sigma2-sigma12^2))
}
f=outer(x1,x2,z)
persp3D(x1, x2, f, theta = 60, phi = 0,expand=0.5,xlab="X1",ylab="X2",zlab="Joint Distribution")


# HISTOGRAM
f1=function(x){
  (1/sqrt(2*pi*sigma1))*exp(-(x-mu1)^2/(2*sigma1^2))
}

f2=function(x){
  (1/sqrt(2*pi*sigma2))*exp(-(x-mu2)^2/(2*sigma2^2))
}

x=x1
hist(x,freq = FALSE)
curve(f1(x),add = TRUE)

x=x2
hist(x,freq = FALSE)
curve(f2(x),add = TRUE)
