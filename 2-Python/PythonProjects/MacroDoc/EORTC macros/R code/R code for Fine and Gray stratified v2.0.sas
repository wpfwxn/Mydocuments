submit / R;
library(splines)
library(survival)
library(cmprsk)
library(crrSC)

attach(mydata)

# Fine and Gray model

source ("C:/temp/callCRR.txt")


coef=fitCR$coef
hr=exp(coef)
std1=fitCR$var^0.5
std=diag(std1)
var=diag(fitCR$var)
pvalue=round(1-pchisq(coef^2/var,1),25)
lowCI=exp(coef+qnorm(0.025,mean=0,sd=1)*std)
upCI=exp(coef+qnorm(0.975,mean=0,sd=1)*std)
resu=data.frame(variables=colnames(x),coef,hr,std,lowCI,upCI,pvalue)

endsubmit;



