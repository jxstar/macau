library(quantmod)
library(leaps)
library(Hmisc) #describe
library(psych) #describe
library(GPArotation)
library(pastecs) #stat.desc
library(corrgram) # for corralation analysis
library(gvlma)
library(car)
library(relaimpo)

library(xlsx)
library(RSQLite)
library(RMySQL)

library(ggplot2) #add for ggplot
library(reshape2)
library(dplyr)

##deal with CSV file
##1. delete the "," in number
##2. change the date to 
par(mfrow=c(1,1))
mycolor=rainbow(20)

#============================================================
## Global constant
#============================================================

target.time=as.POSIXlt("2015-07-01")
target.mon=target.time$mon+1
target.year=target.time$year+1900
(gcol=paste("m", 1:target.mon, sep=""))
(tm=paste("m",target.mon,sep=""))
(um=paste("m",target.mon-1,sep=""))
gmethod=c("Direct LR","1st Order Diff", "Y2Y Diff12 LR","Y2Y Diff12 1st Diff",
          "M2M 1st Order Diff LR","1st Order Diff Mean","M2M 2nd Order Diff","Baidu Index LR")
gstatus=c("Perfect", "Good", "Conservative","Normal", "Poor", "Singular")
gadopted=c("Y", "N")

resplot=function (usedata,predict.result){
  usedata[which(usedata$year==target.year),tm]=predict.result
  msjp=melt(usedata[,c("year",tm,um)],id="year")
  p <- ggplot(msjp, aes(year, value))
  p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
    labs(title=paste(gpicname,"Result",sep=" : "),x="year", y="Tourist Number")
  print(p1)
}



#=================================================================================================
#1# read the data
#=================================================================================================

macau=read.xlsx("macau.tourists.number.xls",1)
#mc=select(macau,date,tot,cn,hk,tw)
mc=macau
mc$cn1 = c(NA, mc$cn[1:(nrow(mc)-1)]);
mc$cn12= c(rep(NA, 12), mc$cn[1:(nrow(mc)-12)]);
mc$cn2hk1=c(NA, mc$cn2hk[1:(nrow(mc)-1)]);
mc$cn2hk12=c(rep(NA, 12), mc$cn2hk[1:(nrow(mc)-12)]);
mc$diff=mc$cn-mc$cn1
mc$diff12=mc$cn-mc$cn12                                                       
mc$diff2nd=mc$diff-c(NA, mc$diff[1:(nrow(mc)-1)])
mc$mm=mc$cn/mc$cn1
mc$yy=mc$cn/mc$cn12
mc=mc[order(mc$date),]
mc$year=as.integer(format(as.Date(mc$date),"%Y"))
mc$month=as.integer(format(as.Date(mc$date),"%m"))
#head(mc)

#=================================================================================================
#2# analysis the datap <- ggplot(jp, aes(month, arrival))
#=================================================================================================
#2.1 overall prosect---------------------------------------------------------

p <- ggplot(mc, aes(month, cn))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+
  geom_point(aes(colour = factor(year)))+labs(title="Macau History Tourist Number Groupby Year")
print(p1)
p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)+
  labs(title="Macau History Tourist Number")
print(p2)

# Check if we can use the Y2Y method to predict----------------------------
p<- ggplot(filter(mc,year %in% c(2002:target.year),month %in% c(4:target.mon)), aes(year, cn))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC History Tourist Number Groupby Month")
print(p1)

p<- ggplot(filter(mc,year %in% c(2002:target.year),month %in% c(5:target.mon)), aes(year, diff12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC Y2Y Diff12 Groupby Month")
print(p1)

p<- ggplot(mc, aes(year, diff12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC History Y2Y Diff12 Groupby Month")
print(p1)

if (F) {
  p<- ggplot(filter(mc,year %in% c(2002:target.year),month %in% c(1:target.mon)), aes(month, diff12))
  p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
    labs(title="MC History Tourist Number Groupby Month")
  print(p1)
  
  p<- ggplot(mc, aes(month, diff12))
  p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
    labs(title="MC History Tourist Number Groupby Month")
  print(p1)
  
  p <- ggplot(mc, aes(date, cn))
  p1 <- p + geom_line(aes(date, cn))+geom_line(aes(date, tot))+geom_line(aes(date, hk))
  print(p1)
}



# Check the  1st Order Diff M2M
# Conclusion: month 6.7.8.9.10.11 can use the 1st Order M2M Diff method
p<- ggplot(mc, aes(month, diff))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="MC History Diff Groupby Month")
print(p1)

p<- ggplot(mc, aes(year, diff))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC History Tourist Number Groupby Year")
print(p1)

p<- ggplot(filter(mc,year %in% c(2002:target.year),month %in% c(1:target.mon)), aes(year, diff))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC History Tourist Number Groupby Year")
print(p1)



#=================================================================================================
#3 Y2Y Diff12 M6 to Predict M7
#combination
#=================================================================================================

#===========================================================
# Check if we can use the Y2Y method to predict
p<- ggplot(filter(mc,year %in% c(2002:target.year),month %in% c(6:target.mon)), aes(year, cn))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC History Tourist Number Groupby Month")
print(p1)

p<- ggplot(filter(mc,year %in% c(2002:target.year),month %in% c(6:target.mon)), aes(year, diff12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC Y2Y Diff12 Groupby Month")
print(p1)


#===========================================================
# prepare the data ready to use
#2003 year is quite special
cmbjp=select(filter(mc,year %in% c(2008:target.year),month %in% c(1:target.mon)),year,month,cn)
meltjp=melt(cmbjp,id=c("year","month"))
#cast the data as year id and produce m1a2,m34
dmjp=dcast(meltjp,year~month+variable)
names(dmjp)=c("year",gcol)

for (i in 2:(target.mon-1)){
  if (i<=3) {
    dmjp$m1a2=(dmjp$m1+dmjp$m2)/2
    next
  }
  dmjp[,paste("m", (i-1), i, sep="")]=apply(dmjp[, gcol[c((i-1), i)]], 1, mean, na.rm=T)
}

#calculate the Y2Y percentage
dmpjp=dmjp
dmpjp[2:nrow(dmpjp),2:ncol(dmpjp)]=dmjp[2:nrow(dmjp),2:ncol(dmjp)]-dmjp[1:nrow(dmjp)-1,2:ncol(dmjp)]
dmpjp[1,2:ncol(dmpjp)]=NA
#dmpjp
#apply(dmpjp[,c(6,7)],1,order)


#===========================================================
#the method we select
#[*]3.1 direct LR : M6-->M7
#[*]3.2 1st order diff: M2015.7-M2014.7=M2015.6-M2014.6
#[ ]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7)
#[ ]3.4 Diff12 1st order diff(2nd order diff):Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2015.6)


#-----------------------------------------------------------
#[*]3.1 direct LR : M6-->M7: m6 is the best: 0.9958167
(hcor.LR=cor(dmjp[,which(names(dmjp)==gcol[target.mon])],dmjp[,-c(1,which(names(dmjp)==tm))],use="na.or.complete"))
#[ ]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7):  direct LR is much better, hcor.diff<hcor.LR
(hcor.Diff=cor(dmpjp[-nrow(dmpjp),which(names(dmpjp)==gcol[target.mon])],dmpjp[-nrow(dmpjp),-1],use="na.or.complete"))
(hcor.LR-hcor.Diff)
knitr::kable(as.data.frame(hcor.LR),caption="Y2Y M7 Coefficient Correlation")
knitr::kable(as.data.frame(hcor.Diff),caption="Y2Y Diff12（M7）Coefficient Correlation")
#vcor no year is quite correlated
(vcor=cor(t(dmpjp[-which(names(dmpjp)==gcol[target.mon])]),t(dmpjp[,-which(names(dmpjp)==gcol[target.mon])])))



#===========================================================
#[*]3.1 direct LR : M6-->M7
#[ ]3.2 1st order diff: M2015.7-M2014.7=M2015.6-M2014.6
#[ ]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7)
#[ ]3.4 Diff12 1st order diff(2nd order diff):Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2015.6)
methodology=gmethod[1]
(usedata=dmjp)
(tm)
(um="m6")
(useformula=as.formula(paste(tm, "~", um, sep="")))
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))


ufit=lm(useformula,data=na.omit(usedata))
(smfit=summary(ufit))
(predict.result=predict(ufit,newdata=usedata[which(usedata$year==target.year),]))
usedata[which(usedata$year==target.year),tm]=predict(ufit,newdata=usedata[which(usedata$year==target.year),])
usedata$prediction=predict(ufit,newdata=usedata)
usedata$error=(usedata$prediction-usedata[,tm])/usedata[,tm]
usedata

msjp=melt(usedata[,c(tm,um,"prediction")],id=um)
p <- ggplot(msjp, aes(msjp[,um], value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x=um, y=tm)
print(p1)

msjp=melt(usedata[,c("year",tm,um,"prediction")],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x="year", y="Tourist Number")
print(p1)

(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ":LR ERROR",sep=" "), x="YEAR", y="ERROR"))

er=range((usedata[,tm]-usedata$prediction),na.rm = T)
benchmark=data.frame(date=as.Date(target.time), id="1.1",methdology=methodology,target=tm, dependent=um,
                    floor=predict.result+er[1], cap=predict.result+er[2],bestguess=predict.result,
                    correlation=max(hcor.LR,na.rm = T), adjR2=smfit$adj.r.squared,variance=NA,maxError=max(abs(usedata$error),na.rm=T),
                    status="Perfect", adopted="Y",stringsAsFactors = F)
(benchmark)
knitr::kable(benchmark,caption="月度直接线性回归预测")






#===========================================================
#[ ]3.1 direct LR : M6-->M7
#[*]3.2 1st order diff: M2015.7-M2014.7=M2015.6-M2014.6
#[ ]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7)
#[ ]3.4 Diff12 1st order diff(2nd order diff):Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2015.6)

(methodology=gmethod[2])
(usedata=dmjp)
(tm)
(um="m6")
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))

#sjp.data=(dmjp[,-1]-dmjp[,which(names(dmpjp)==tm)])/dmjp[,which(names(dmpjp)==tm)]
sjp.data=(dmjp[,-1]-dmjp[,which(names(dmjp)==tm)])/dmjp[,which(names(dmjp)==tm)]

sigma=apply(sjp.data,2,function (x) {return(var(x,na.rm=T))})
(sigma=sigma[order(sigma)])
(p <- ggplot(data=melt(as.data.frame(t(sigma)),id=tm),aes(x=factor(variable),weight=value)) + 
  geom_bar(position='dodge',fill="grey")+labs(title=paste(gpicname," : (Mx-M7) variance",sep=" ")))
(um="m6")

# m6: M6 Diff 12
#mean(m7)(without2015)-mean(m6)(without2015)=m7(2015)-m6(2015)
#m7(2015)=mean(m7)(without2015)-mean(m6)(without2015)+m6(2015)
#predict.result=mean(usedata[-which(usedata$year==target.year),tm]-
#                    usedata[-which(usedata$year==target.year),um])+usedata[which(usedata$year==target.year),um]
#m7(2015)-m7(2014)=m6(2015)-m6(2014)
#m7(2015)=m7(2014)-m6(2014)+m6(2015)
predict.result=usedata[which(usedata$year==(target.year-1)),tm]-
               usedata[which(usedata$year==(target.year-1)),um]+usedata[which(usedata$year==target.year),um]

prange=range(usedata[-which(usedata$year==(target.year)),tm]-
       usedata[-which(usedata$year==(target.year)),um]+usedata[which(usedata$year==target.year),um],na.rm = T)

usedata$prediction=c(NA,usedata[-which(usedata$year==(target.year)),tm])-
  c(NA,usedata[-which(usedata$year==(target.year)),um])+usedata[,um]
usedata$error=(usedata$prediction-usedata[,tm])/usedata[,tm]
(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ": ERROR",sep=" "), x="YEAR", y="ERROR"))


benchmark[which(gmethod==methodology),]=
          data.frame(date=as.Date(target.time),id="1.2",methdology=methodology,target=tm, dependent=um,
          floor=prange[1], cap=prange[2], bestguess=predict.result,
          correlation=NA, adjR2=NA,variance=min(sigma[-1],na.rm = T),maxError=max(abs(usedata$error),na.rm=T),
          status="Good", adopted="Y",stringsAsFactors = F)
(benchmark)
knitr::kable(benchmark,caption="月度一阶差分")
resplot(dmjp,predict.result)


#===========================================================
#[ ]3.1 direct LR : M6-->M7
#[ ]3.2 1st order diff: M2015.7-M2014.7=M2015.6-M2014.6
#[*]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7)
#[ ]3.4 Diff12 1st order diff(2nd order diff):Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2015.6)

(methodology=gmethod[3])
(usedata=dmpjp)
(tm)
(um="m56")
(useformula=as.formula(paste(tm, "~", um, sep="")))
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))
(hcor.Diff=cor(dmpjp[,which(names(dmpjp)==gcol[target.mon])],dmpjp[,-c(1, which(names(dmpjp)==tm))],use="na.or.complete"))
#select M56

ufit=lm(useformula,data=na.omit(usedata))
smfit=summary(ufit)
(predict.result=predict(ufit,newdata=usedata[which(usedata$year==target.year),])+dmjp[which(usedata$year==(target.year-1)),tm])
usedata[which(usedata$year==target.year),tm]=predict(ufit,newdata=usedata[which(usedata$year==target.year),])
usedata$prediction=predict(ufit,newdata=usedata)
usedata$error=(usedata$prediction-usedata[,tm])/dmjp[,tm]
#usedata

msjp=melt(usedata[,c(tm,um,"prediction")],id=um)
p <- ggplot(msjp, aes(msjp[,um], value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x=um, y=tm)
print(p1)

msjp=melt(usedata[,c("year",tm,um,"prediction")],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x="year", y="Y2Y Diff12")
print(p1)

(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ":LR ERROR",sep=" "), x="YEAR", y="ERROR"))

er=range((usedata[,tm]-usedata$prediction),na.rm = T)
benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="2.1",methdology=methodology, target=paste(tm,"(2015-2014)",sep = ""), dependent=um,
             floor=predict.result+er[1],cap=predict.result+er[2], bestguess=predict.result,
             correlation=max(hcor.Diff,na.rm = T), adjR2=smfit$adj.r.squared,variance=NA,maxError=max(abs(usedata$error),na.rm=T),
             status="Normal", adopted="Y",stringsAsFactors = F)
(benchmark)
knitr::kable(benchmark,caption="Y2Y Diff12 线性回归预测")
resplot(dmjp,predict.result)



#===========================================================
#[ ]3.1 direct LR : M6-->M7
#[ ]3.2 1st order diff: M2015.7-M2014.7=M2015.6-M2014.6
#[ ]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7)
#[*]3.4 Diff12 1st order diff(2nd order diff):Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2015.6)
#Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2014.6)
(methodology=gmethod[4])
(usedata=dmpjp)
(tm)
(um="m56")
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))

sjp.data=(dmpjp[,-1]-dmpjp[,which(names(dmpjp)==tm)])/dmpjp[,which(names(dmpjp)==tm)]
sigma=apply(sjp.data,2,function (x) {return(var(x,na.rm=T))})
(sigma=sigma[order(sigma)])
(p <- ggplot(data=melt(as.data.frame(t(sigma)),id=tm),aes(x=factor(variable),weight=value)) + 
  geom_bar(position='dodge',fill="grey")+labs(title=paste(gpicname," : (Diff12 Mx- Diff12 M7) variance",sep=" ")))

# m6: M6 Diff 12
#mean(m7)(without2015)-mean(m6)(without2015)=m7(2015)-m6(2015)
#m7(2015)=mean(m7)(without2015)-mean(m6)(without2015)+m6(2015)
#predict.result=mean(usedata[-which(usedata$year==target.year),tm]-
#                    usedata[-which(usedata$year==target.year),um])+usedata[which(usedata$year==target.year),um]
usedata
predict.result=usedata[which(usedata$year==(target.year-1)),tm]-usedata[which(usedata$year==(target.year-1)),um]+
               usedata[which(usedata$year==target.year),um]+dmjp[which(usedata$year==(target.year-1)),tm]

prange=range(usedata[-which(usedata$year==(target.year)),tm]-usedata[-which(usedata$year==(target.year)),um]+
             usedata[which(usedata$year==target.year),um],na.rm=T)+dmjp[which(usedata$year==(target.year-1)),tm]

#Diff12(2015.7)=Diff12(2014.7)-Diff12(2014.6)+Diff12(2015.6)

usedata$prediction=c(NA,usedata[-which(usedata$year==(target.year)),tm])-
  c(NA,usedata[-which(usedata$year==(target.year)),um])+usedata[,um]
usedata$error=(usedata$prediction-usedata[,tm])/dmjp[,tm]
(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ": ERROR",sep=" "), x="YEAR", y="ERROR"))

benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="2.2",methdology=methodology, target=paste(tm,"(2015-2014)",sep = ""), dependent=um,
             floor=prange[1], cap=prange[2], bestguess=predict.result,
             correlation=NA, adjR2=NA,variance=min(sigma[-1],na.rm = T),maxError=max(abs(usedata$error),na.rm=T),
             status="Normal", adopted="Y",stringsAsFactors = F)
(benchmark)

knitr::kable(benchmark,caption="基于Y2Y增长绝对数值Diff12一阶差分")

usedata[which(usedata$year==target.year),tm]=usedata[which(usedata$year==(target.year-1)),tm]-usedata[which(usedata$year==(target.year-1)),um]+
  usedata[which(usedata$year==target.year),um]
msjp=melt(usedata[,c("year",tm,um)],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x="year", y="Y2Y Diff12")
print(p1)

resplot(dmjp,predict.result)

















#=================================================================================================
#4 M2M Diff1 M6 to Predict M7
#combination
#=================================================================================================

#===========================================================
# Overview 
p<- ggplot(filter(mc,year %in% c(2012:target.year),month %in% c(4:target.mon)), aes(month, cn))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="MC Number Groupby year")
print(p1)

p<- ggplot(filter(mc,year %in% c(2008:target.year),month %in% c(4:target.mon)), aes(month, diff))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="MC Diff1 Groupby Year")
print(p1)

p<- ggplot(filter(mc,year %in% c(2008:target.year),month %in% c(4:target.mon)), aes(year, diff))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC Diff1 Groupby Month")
print(p1)

#===========================================================
# the method we select:  Diff1
#[ ]4.1 Diff1 LR : Mx-->D7
#[ ]4.2 1st order diff: m2015.7-M2014.7=M2015.6-M2014.6
#[*]4.3 2nd order diff: D2015.7-D2014.7=D2015.6-D2014.6


#===========================================================
# Prepare the data
diff1=select(filter(mc,year %in% c(2008:target.year),month %in% c(1:target.mon)),year,month,diff)
diff1=melt(diff1,id=c("year","month"))
diff1=dcast(diff1,year~month+variable)
names(diff1)=c("year",gcol)





#===========================================================
#[*]4.1 Diff1 LR : Mx-->D7
#[ ]4.2 1st order diff: m2015.7-M2014.7=M2015.6-M2014.6
#[ ]4.3 2nd order diff: D2015.7-D2014.7=D2015.6-D2014.6
# First Order with Linear Regression : M7=approximated diff1 + M6
# approximated diff1(M7-M6)=LR(M6)

methodology=gmethod[5]
(tm)
(um="m3")
(useformula=as.formula(paste(tm, "~", um, sep="")))
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))

#(hcor=cor(diff1[,which(names(diff1)==tm)],
#          cbind(diff1[,-c(1,which(names(diff1)==tm))],dmjp[,which(names(dmjp)==gcol[target.mon-1])]),use="na.or.complete"))
(hcor.diff=cor(diff1[,which(names(diff1)==tm)],diff1[,-c(1,which(names(diff1)==tm))],use="na.or.complete"))
(hcor.dmjp=cor(diff1[,which(names(diff1)==tm)],dmjp[,-c(1,which(names(dmjp)==tm))],use="na.or.complete"))
#The cor is bad betweent diffs and the tourist numbers
knitr::kable(as.data.frame(hcor.diff),caption="基于M2M月度一阶差分的线性回归预测")
knitr::kable(as.data.frame(hcor.dmjp),caption="基于M2M月度一阶差分的线性回归预测")

# m6 tourist number, m7(m7-m6)
usedata=as.data.frame(cbind(diff1[,c("year",tm)], dmjp[,um]))
names(usedata)=c("year",tm, um)

ufit=lm(useformula,data=na.exclude(usedata))
(smfit=summary(ufit))
(predict.result=predict(ufit,newdata=usedata[which(usedata$year==target.year),])+usedata[which(usedata$year==(target.year)),um])
usedata[which(usedata$year==target.year),tm]=predict(ufit,newdata=usedata[which(usedata$year==target.year),])
usedata$prediction=predict(ufit,newdata=usedata)
usedata$error=(usedata$prediction-usedata[,tm])/dmjp[,tm]
#usedata

msjp=melt(usedata[,c(tm,um,"prediction")],id=um)
p <- ggplot(msjp, aes(msjp[,um], value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x=um, y=tm)
print(p1)

msjp=melt(usedata[,c("year",tm,um,"prediction")],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x="year", y="M2M Diff1")
print(p1)

msjp=melt(usedata[,c("year",tm,"prediction")],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x="year", y="M2M Diff1")
print(p1)

(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ":LR ERROR",sep=" "), x="YEAR", y="ERROR"))

er=range((usedata[,tm]-usedata$prediction),na.rm = T)
benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="3.1",methdology=methodology, target=paste(tm,um,sep = "-"), dependent=um,
             floor=predict.result+er[1],cap=predict.result+er[2], bestguess=predict.result,
             correlation=max(hcor.dmjp,na.rm = T), adjR2=smfit$adj.r.squared,variance=NA,maxError=max(abs(usedata$error),na.rm=T),
             status="Normal", adopted="Y",stringsAsFactors = F)
(benchmark)
knitr::kable(benchmark,caption="基于M2M月度一阶差分的线性回归预测")
resplot(dmjp,predict.result)


#===========================================================
#[ ]4.1 Diff1 LR : Mx-->D7
#[*]4.2 1st order diff: m2015.7-M2014.7=M2015.6-M2014.6
#[ ]4.3 2nd order diff: D2015.7-D2014.7=D2015.6-D2014.6
# First Order without Linear Regression : M7(2015)-M6(2015)=M7(2014)-M6(2014)
#mean(m7)(without2015)-mean(m6)(without2015)=m7(2015)-m6(2015)
#m7(2015)=mean(m7)(without2015)-mean(m6)(without2015)+m6(2015)
(methodology=gmethod[6])
(usedata=diff1)
(tm)
(um="m6")
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))

#mean(m7)(without2015)-mean(m6)(without2015)=m7(2015)-m6(2015)
#m7(2015)=mean(m7)(without2015)-mean(m6)(without2015)+m6(2015)
predict.result=mean(usedata[-which(usedata$year==target.year),tm],na.rm=T)+
               dmjp[which(dmjp$year==target.year),um]
prange=range(usedata[,tm]-mean(usedata[-which(usedata$year==target.year),tm],na.rm=T),na.rm=T)+predict.result

usedata$prediction=mean(usedata[-which(usedata$year==target.year),tm],na.rm=T)
usedata$error=(usedata$prediction-usedata[,tm])/dmjp[,tm]
(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ": ERROR",sep=" "), x="YEAR", y="ERROR"))

benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="3.2",methdology=methodology,target=paste(tm,um,sep = "-"), dependent="mean",
             floor=prange[1], cap=prange[2], bestguess=predict.result,
             correlation=NA, adjR2=NA,variance=NA,maxError=max(abs(usedata$error),na.rm=T),
             status="Conservative", adopted="Y",stringsAsFactors = F)
(benchmark)
knitr::kable(benchmark,caption="基于M2M月度一阶差分的保守预测")
resplot(dmjp,predict.result)






#===========================================================
#[ ]4.1 Diff1 LR : Mx-->D7
#[ ]4.2 1st order diff: m2015.7-M2014.7=M2015.6-M2014.6
#[*]4.3 2nd order diff: D2015.7-D2014.7=D2015.6-D2014.6
#=================================================================================
# Second Order without Linear Regression : Diff M7(2015) - Diff M6(2015)=Diff M7(2014) - Diff M6(2014)
# D7(2015)=D6(2015)+D7(2014)-D6(2014)

diff2nd=select(filter(mc,year %in% c(2008:target.year),month %in% c(1:target.mon)),year,month,diff2nd)
diff2nd=melt(diff2nd,id=c("year","month"))
diff2nd=dcast(diff2nd,year~month+variable)
names(diff2nd)=c("year",gcol)

(methodology=gmethod[7])
(tm)
(um="m6")
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))
(usedata=diff2nd)

#mean(m7)(without2015)-mean(m6)(without2015)=m7(2015)-m6(2015)
#D7(2015)=mean(D7)(without2015)-mean(D6)(without2015)+D6(2015)

if (T) {
  predict.result=usedata[which(usedata$year==(target.year-1)),tm]+
    diff1[which(dmjp$year==target.year),um]+dmjp[which(dmjp$year==target.year),um]
  prange=range(usedata[,tm]-c(NA,usedata[-which(usedata$year==target.year),tm]),na.rm=T)+predict.result
  usedata$prediction=c(NA,usedata[-which(usedata$year==target.year),tm])
} else {
  predict.result=mean(usedata[-which(usedata$year==target.year),tm],na.rm=T)+
    diff1[which(dmjp$year==target.year),um]+dmjp[which(dmjp$year==target.year),um]
  prange=range(usedata[,tm]-mean(usedata[-which(usedata$year==target.year),tm],na.rm=T),na.rm=T)+predict.result
  usedata$prediction=mean(usedata[-which(usedata$year==target.year),tm],na.rm=T)
}
usedata$error=(usedata$prediction-usedata[,tm])/dmjp[,tm]
(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ": ERROR",sep=" "), x="YEAR", y="ERROR"))

benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="3.3",methdology=methodology,target=paste("delta","(",paste(tm,um,sep = "-"),")",sep=""), dependent=paste(tm,um,sep = "-"),
             floor=prange[1], cap=prange[2], bestguess=predict.result,
             correlation=NA, adjR2=NA,variance=NA,maxError=max(abs(usedata$error),na.rm=T),
             status="Singular", adopted="Y",stringsAsFactors = F)
(benchmark)
knitr::kable(benchmark,caption="基于M2M月度二阶差分预测")
resplot(dmjp,predict.result)








#=================================================================================================
#6 Linear Regression
#=================================================================================================
(methodology=gmethod[8])
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", "Baidu", "-->", tm, sep=" "))

#6.1 read and check the hotword of baidu index----------------------------------------
hotword=read.xlsx("macau.search.index.xlsx",1)
hwname=c("date","mc","mcdt","mcdb","mcdc","mcgwgl","mcjc","mcjd","mcjdyd","mcly","mclydt","mclygl",
                 "mclyjd","mcms","mcsn","mctq","mcy","mczyx","gatxz","gay","gaybj","hk","hkmc","hkmcly","hkmclygl")
names(hotword)=hwname
hw=hotword[,1:length(hwname)]
#turn off the data to numeric
#wcgwgl hkmclygl
for (i in 2:ncol(hw)){
  for (j in 1:nrow(hw)){
    if (is.na(hw[j,i])){
      cat(i,j,hw[j,i],mean(hw[,i],na.rm=T),hw[j-1,i],"\n")
      hw[j,i]=hw[j-1,i]
    }
    if (hw[j,i]<(mean(hw[,i],na.rm=T)/100)){
      cat(i,j,hw[j,i],mean(hw[,i],na.rm=T),hw[j-1,i],"\n")
      hw[j,i]=hw[j-1,i]
    }
  }
}

head(hw)



#6.2 gather the date to fhw.all----------------------------------------
fhw=hw
fhw=transform(fhw,month=format(date,"%Y-%m"))

fhw.melt=melt(fhw,id=c("date","month"))
fhw.gb=group_by(fhw.melt,month,variable)
fhw.month=summarize(fhw.gb,date=first(date),value=sum(value))
fhw.month=dcast(fhw.month,date+month~variable)

fhw.data.lag=fhw.month[,3:ncol(fhw.month)]
head(fhw.data.lag)
fhw.data.lag[2:nrow(fhw.data.lag),]=fhw.data.lag[1:(nrow(fhw.data.lag)-1),]
fhw.data.lag[1,]=NA
names(fhw.data.lag)=paste(names(fhw.data.lag),"1",sep="")
fhw.month=cbind(fhw.month,fhw.data.lag)
fhw.month=filter(fhw.month,date>as.Date("2011-12-01"))
fhw.all=merge(select(mc,date,cn,diff, diff12,cn1,cn12,holiday,newyear,nbmonth,cn2hk,cn2hk1,cn2hk12),select(fhw.month,-month),by = intersect("date", "date"))


#6.3 determine the time and select the var to Liear Regression----------------------------------------
date.start=c(as.Date("2012-01-01"),as.Date("2013-01-01"),as.Date("2014-01-01"),
             as.Date("2014-06-01"),as.Date("2015-01-01"))
for(i in 1:length(date.start)){
  all.cor=as.data.frame(cor(fhw.all[which(fhw.all$date>=date.start[i]),"cn"],
                            fhw.all[which(fhw.all$date>=date.start[i]),5:ncol(fhw.all)],use="na.or.complete"))
  print(date.start[i])
  print(all.cor[order(desc(all.cor))])
}

#==================================================================================================
#hwm=select(fhw.all,date,cn,holiday,newyear,nbmonth,cn2hk,mcjdyd,cn12,mcjd,mcjd1,mcdc1,hkmcly1,hkmclygl1)
hwm=select(fhw.all,date,cn,holiday,newyear,nbmonth,cn2hk,mcjd1,hkmclygl1,hkmcly1,mcdb1,mcdc1)
           
picdata=hwm
picdata[,6:ncol(picdata)]=apply(picdata[,6:ncol(picdata)],2,function (x){
  x/mean(x,na.rm=T)*mean(picdata$cn,na.rm=T)})
mpic=melt(picdata,id="date")
p<- ggplot(mpic,aes(date, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title="MC vs index Groupby variable")+facet_wrap(~variable)
print(p1)
for (i in 6:ncol(picdata)){
  mpic=melt(picdata,id="date")
  p<- ggplot(melt(picdata[,c(1,2,i)],id="date"),aes(date, value))
  p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
    labs(title=paste("MC vs",names(picdata)[i]))
  print(p1)
}

#time: 2015-01-201507
#var : cn2hk, 
#       mcjd1
#factor: holiday newyear nbmonth
#---------------------------------------------------------------------------
#************determine the interval ****************************************
#---------------------------------------------------------------------------
#time.range=c(which(hwm$date==as.Date("2014-06-01")):which(hwm$date==as.Date("2015-05-01"))) #terrible
time.range=c(which(hwm$date==as.Date("2015-02-01")):which(hwm$date==as.Date("2015-06-01"))) #terrible
cat("The Date We Use in this algorithm\n", as.character(hwm[time.range,"date"]),"\n")
hwm.p=hwm[which(hwm$date==as.Date("2015-07-01")),]
hwm.t=hwm[time.range,]

cat("The correlation of the hotwords we may select:\n")
hwm.cor=as.data.frame(cor(hwm.t$cn,hwm.t[,6:ncol(hwm.t)]))
hwm.cor[order(-hwm.cor)]
corrgram(hwm.t[,6:ncol(hwm.t)],order=T,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)


#5.4 pick up the best variables -----------------------------------------------------------
#leaps=regsubsets(cn~cn2hk+mcjdyd+cn12+mcjd+mcjd1+mcdc1+hkmcly1+hkmclygl1+newyear+holiday,data=hwm.t,nbest=4)
#plot(leaps,scale="adjr2",main="Variables Selection")

leaps=regsubsets(cn~mcjd1+hkmclygl1+mcdc1+holiday+newyear,data=hwm.t,nbest=4)
plot(leaps,scale="adjr2",main="Variables Selection")


#5.5 fit--------------------------------------------------------------fit

#------------------------------------------------------------------------------
#******************determine the varaible and make linear regression***********
#------------------------------------------------------------------------------
(um="hkmclygl1")

#hwfit=lm(cn~mcdc1+holiday,hwm.t)#holiday
#hwfit=lm(cn~mcdc1,hwm.t)#165 # this prediction must be short interval:less than 
#hwfit=lm(cn~hkmclygl1+holiday,hwm.t)
hwfit=lm(cn~hkmclygl1,hwm.t)
cat("The predic result:",predict(hwfit,newdata=hwm.p),"\n")
smfit=summary(hwfit)

error=(fitted(hwfit)-hwm.t$cn)/hwm.t$cn
hwm.t$predict=fitted(hwfit)
hwm.t$error=error
(p <- ggplot(data=hwm.t,aes(x=factor(date),weight=error,fill=factor(date))) + 
  geom_bar(position='dodge')+labs(title="Linear Regression Error"))
gvmodel=gvlma(hwfit)
summary(gvmodel)
#multicollinearily
#sqrt(vif(hwfit))

#3.2 predict-----------------------------------------------------------predict
msjp=hwm
msjp$prediction=predict(hwfit,newdata=msjp)
msjp=melt(msjp[,c("date","cn","prediction")],id="date")
p <- ggplot(msjp, aes(date, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title="LR Prediction Vs Official")
print(p1)


#3.3 evalation--------------------------------------------------------evaluation
predict.result=predict(hwfit,newdata=hwm.p)
prange=range( (fitted(hwfit)-hwm.t$cn),na.rm=T)+predict.result
hwm.t[,c("date","cn","predict","error")]
hwm[,c("date","cn")]

benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="4",methdology=methodology,target=tm, dependent=um,
             floor=prange[1], cap=prange[2], bestguess=predict.result,
             correlation=hwm.cor[,um], adjR2=smfit$adj.r.squared,variance=NA,maxError=max(abs(hwm.t$error),na.rm=T),
             status="Normal", adopted="Y",stringsAsFactors = F)

knitr::kable(benchmark,caption="基于百度指数的线性回归预测")







#======================================================================================
# Final Result Analysis
#======================================================================================
#(final.result=mean(benchmark[1:8,"bestguess"],na.rm=T))
(final.result=median(benchmark[1:8,"bestguess"],na.rm=T))
benchmark
n=length(gmethod)+1
benchmark[n,]=NA
benchmark[n,"date"]=as.Date(target.time)
benchmark[n,"bestguess"]=final.result
benchmark[n,"methdology"]="Median"
(benchmark)

knitr::kable(benchmark,caption=paste("最终预测结果汇总表", as.Date(target.time),sep=""))

mc[which(mc$date==as.Date(target.time)),"cn"]=final.result
mc[which(mc$date==as.Date(target.time)),]=transform(mc[which(mc$date==as.Date(target.time)),],
                                                    diff=cn-cn1,diff12=cn-cn12,
                                                    mm=cn/cn1,yy=cn/cn12)
tmc=select(mc,date,thismon=cn,lastmon=cn1,lastyear=cn12,M2M=diff,Y2Y=diff12,mm,yy)
names(tmc)=c("date","Tourists","lastMonth","lastYear", "M2M", "Y2Y","mmratio","yyratio")
tmc=filter(tmc, date>=as.Date("2014-07-01") & date<=as.Date("2015-07-01"))
tmc

knitr::kable(tmc,caption=paste(as.Date(target.time),"MC Result Analysis"))
save(file=paste("Macau-Prediction-result-", as.Date(target.time), ".Rdata",sep=""), benchmark, tmc, mc )

#---------------------------------------overview--------------------------------------------
p <- ggplot(mc, aes(month, cn))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6) + 
  geom_point(aes(colour = factor(year)))+labs(title="Macau Tourist Prediction Groupby Year")
print(p1)
p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)+
  labs(title="Macau Tourist Prediction Groupby Year")
print(p2)

#---------------------------------------Y2Y group by month--------------------------------------------
p<- ggplot(filter(mc,year %in% c(2002:target.year),month %in% c(4:target.mon)), aes(year, cn))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="Macau Tourist Prediction Groupby Month")
print(p1)

p<- ggplot(filter(mc,year %in% c(2002:target.year),month %in% c(4:target.mon)), aes(year, diff12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="Macau Y2Y Diff Groupby Month")
print(p1)



#---------------------------------------M2M group by year--------------------------------------------
p <- ggplot(filter(mc,year %in% c(2008:target.year),month %in% c(4:target.mon)), aes(month, cn))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="Macau Tourist Number Groupby Year")
print(p1)

p <- ggplot(filter(mc,year %in% c(2008:target.year),month %in% c(4:target.mon)), aes(month, diff))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="Macau M2M Groupby Year")
print(p1)










###################################################################################
# 7 Getting data in to MySQL
###################################################################################

if (F){
  
  
  conn <- dbConnect(MySQL(), dbname = "thcresult", 
                    username="root", password="123456",host="101.200.189.155",port=3306)
  #dbGetQuery(conn,"set names utf8")  #for macos
  dbGetQuery(conn,"set names gbk") # for win
  
  benchmark$prediction=benchmark[nrow(benchmark),"bestguess"]
  benchmark$acturalreslult=0
  benchmark$updatetime=as.numeric(Sys.time())
  benchmark$unixdate=as.numeric(as.POSIXct(benchmark$date))
  benchmark
  #rownames(benchmark)
  
  dbListTables(conn)
  #dbRemoveTable(conn,"japanresult")
  
  if (!dbExistsTable(conn, "japanresult"))  dbWriteTable(conn, "japanresult", benchmark)
  if (dbExistsTable(conn, "japanresult"))  dbWriteTable(conn, "japanresult", benchmark,append=T)
  
  
  
  a=dbGetQuery(conn,"select * from japanresult")
  class(a$unixdate) = c('POSIXt','POSIXct')
  class(a$updatetime) = c('POSIXt','POSIXct')
  a
  str(a)
  #dbGetQuery(conn,"delete from japanresult where date='2015-07-01'")
  
  dbDisconnect(conn)
  
  
}