install.packages("stargazer")
library("stargazer")
library("MASS")
install.packages("robustbase")
library("robustbase")


mydata=read.csv("data.txt",header=FALSE)
mydataS=mydata[,c(7,14,17,22,26,38,127,128)]
colnames(mydataS)=c("hhsize","young","urban","socialsecurity",
                   "income","unemployed","policebudget","crime")



reg1=lm(crime~hhsize,mydataS)
reg2=lm(crime~young,mydataS)
reg3=lm(crime~urban,mydataS)
reg4=lm(crime~socialsecurity,mydataS)
reg5=lm(crime~income,mydataS)
reg6=lm(crime~unemployed,mydataS)
new=mydataS[mydataS[,7]!="?",]
new[,7]=as.numeric(new[,7])
reg7=lm(crime~policebudget,new)
confint(reg1, 'hhsize', level=0.95)

stargazer(reg1,reg2,reg3,reg4,reg5,reg6,reg7,type="text")
