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

## Global constant
target.time=as.POSIXlt("2015-06-01")
target.mon=target.time$mon+1
target.year=target.time$year+1900

#----------------------------------------------------------------------------------------------
#1# read the data

#1.1 official JP arrival data and the currency---------------------------macau

macau=read.xlsx("macau.tourists.number.xls",1)
#mc=select(macau,date,tot,cn,hk,tw)
mc=macau
mc$cn1 = c(NA, mc$cn[1:(nrow(mc)-1)]);
mc$cn12= c(rep(NA, 12), mc$cn[1:(nrow(mc)-12)]);
mc$cn2hk1=c(NA, mc$cn2hk[1:(nrow(mc)-1)]);
mc$cn2hk12=c(rep(NA, 12), mc$cn2hk[1:(nrow(mc)-12)]);
mc$diff=mc$cn-mc$cn1
mc$diff12=mc$cn-mc$cn12                                                       
mc$mm=mc$cn/mc$cn1
mc$yy=mc$cn/mc$cn12
mc=mc[order(mc$date),]
mc$year=as.integer(format(as.Date(mc$date),"%Y"))
mc$month=as.integer(format(as.Date(mc$date),"%m"))


#=================================================================================================
#2# analysis the datap <- ggplot(jp, aes(month, arrival))

#2.1 overall prosect---------------------------------------------------------

p <- ggplot(mc, aes(month, cn))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+
  geom_point(aes(colour = factor(year)))+labs(title="Macau History Tourist Number Groupby Year")
print(p1)
p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)+
  labs(title="Macau History Tourist Number")
print(p2)

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


# Check if we can use the Y2Y method to predict----------------------------
p<- ggplot(filter(mc,year %in% c(2002:target.year),month %in% c(1:target.mon)), aes(year, cn))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC History Tourist Number Groupby Month")
print(p1)

p<- ggplot(filter(mc,year %in% c(2002:target.year),month %in% c(1:target.mon)), aes(year, diff12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC History Tourist Number Groupby Month")
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



#=================================================================================================
#3 Y2Y Diff12 M5 to Predict M6
#combination

#2003 year is quite special
cmbjp=select(filter(mc,year %in% c(2008:target.year),month %in% c(1:target.mon)),year,month,cn)
meltjp=melt(cmbjp,id=c("year","month"))

#cast the data as year id and produce m1a2,m34
dmjp=dcast(meltjp,year~month+variable)
names(dmjp)=c("year","m1","m2","m3","m4","m5","m6")
dmjp$m1a2=(dmjp$m1+dmjp$m2)/2
dmjp$m34=(dmjp$m3+dmjp$m4)/2
dmjp$m45=(dmjp$m5+dmjp$m4)/2

#calculate the Y2Y percentage
dmpjp=dmjp
dmpjp[2:nrow(dmpjp),2:ncol(dmpjp)]=dmjp[2:nrow(dmjp),2:ncol(dmjp)]-dmjp[1:nrow(dmjp)-1,2:ncol(dmjp)]
dmpjp[1,2:ncol(dmpjp)]=0
dmpjp
apply(dmpjp[,c(6,7)],1,order)

#m5 is the best
hcor=cor(dmpjp[-nrow(dmpjp),which(names(dmpjp)=="m6")],dmpjp[-nrow(dmpjp),-1]) 
#vcor no year is quite correlated
vcor=cor(t(dmpjp[-which(names(dmpjp)=="m6")]),t(dmpjp[,-which(names(dmpjp)=="m6")]))

# predict the year 2 year diff
hcor=cor(dmpjp[-nrow(dmpjp),which(names(dmpjp)=="m6")],dmpjp[-nrow(dmpjp),-1]) 

dmpjp.t=dmpjp[-which(dmpjp$year==2015 | dmpjp$year==2008),]
dmpjp.p=dmpjp[which(dmpjp$year==2015),]
diff12.fit=lm(m6~m5,dmpjp.t)
summary(diff12.fit)
dmpjp[which(dmpjp$year==2015),"m6"]=predict(diff12.fit,newdata=dmpjp.p)
dmpjp
p=ggplot(dmpjp,aes(x=year,y=m6))+geom_line()
plot(dmpjp$year,dmpjp$m6,type="b",lty=1,xlab="date",col=mycolor[1],main=("Y2Y Diff12 M5 to Predict M6"))
lines(dmpjp$year,predict(diff12.fit,newdata=dmpjp),type="b",lty=1,xlab="date",col=mycolor[4])
lines(dmpjp$year,dmpjp$m5,type="b",lty=1,xlab="date",col=mycolor[8])
legend("bottomright",legend=c("real m6 diff12", "predict m6 diff12","real m5 diff12"),lty=c(1,1,1,1),col=mycolor[c(1,4,8)])

dmjp[which(dmjp$year==2015),"m6"]=dmpjp[which(dmpjp$year==2015),"m6"]+dmjp[which(dmjp$year==2014),"m6"]
predict.result=dmjp[which(dmjp$year==2015),"m6"]

range(dmpjp.t$m6-fitted(diff12.fit))
barplot((predict(diff12.fit,newdata=dmpjp)-dmpjp$m6)/dmjp$m6,names.arg=dmjp$year,ylab="ERROR")
benchmark=data.frame(date=as.Date(target.time),methdology="Y2Y Diff LR with M5",
                     floor=predict.result+range(dmpjp.t$m6-fitted(diff12.fit))[1],
                     cap=predict.result+range(dmpjp.t$m6-fitted(diff12.fit))[2],
                     bestguess=predict.result,stringsAsFactors = F)







#=================================================================================================
#4 Y2Y Diff Mean M45
#-------------------------------------------------------------------------------------------
sjp.data=(dmpjp[,-1]-dmpjp[,which(names(dmpjp)=="m6")])/dmpjp[,which(names(dmpjp)=="m6")]
sjp.data[1,]=0
#select the best KPI with lowest volatility whichh is sigma--------------------------
sigma=apply(sjp.data,2,function (x) {
  return(var(x,na.rm=T))
})

#here we select m45 with the lowest volatility
sigma=sigma[order(sigma)]
barplot(sigma,main="1st Diff12 variance with some variables")

sjp.data=sjp.data[,names(sigma)]
sjp=cbind(year=dmjp$year,sjp.data)

msjp=melt(dmpjp[,c("year","m6","m5","m45")],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title="Macau Diff12 Groupby Month")
print(p1)

# m6: M6 Diff 12
#mean(m6)(without2015)-mean(m45)(without2015)=m6(2015)-m45(2015)
#m6(2015)=mean(m6)(without2015)-mean(m45)(without2015)+m45(2015)
m6diff12=mean(dmpjp[-c(1,nrow(dmpjp)),"m6"]-dmpjp[-c(1,nrow(dmpjp)),"m45"])+dmpjp[nrow(dmpjp),"m45"]

predict.result=m6diff12+dmjp[which(dmjp$year==2014),"m6"]
prange=range(dmpjp[-c(1,nrow(dmpjp)),"m6"]-dmpjp[-c(1,nrow(dmpjp)),"m45"])+dmpjp[nrow(dmpjp),"m45"]+dmjp[which(dmjp$year==2014),"m6"]
benchmark[2,]=data.frame(date=as.Date(target.time),methdology="Y2Y Diff Mean M45",
                     floor=prange[1],
                     cap=prange[2],
                     bestguess=predict.result,stringsAsFactors = F)
(benchmark)








#=================================================================================================
#5 predict with the month data 1st order difference
#4.1 cast the data as year id and produce m1a2,m34-------------------
current=target.year
recent=target.year-1

vjp=dcast(meltjp,year~month+variable)
names(vjp)=c("year","m1","m2","m3","m4","m5","m6")
vjp$m1a2=(vjp$m1+vjp$m2)/2
vjp$m34=(vjp$m3+vjp$m4)/2
vjp$m45=(vjp$m4+vjp$m5)/2
vjp$m1234=(vjp$m1+vjp$m2+vjp$m3+vjp$m4)/4
vjp$m123=(vjp$m1+vjp$m2+vjp$m3)/3
vjp$m124=(vjp$m1+vjp$m2+vjp$m4)/3


#4.2 predict with the month data differential method-------------------------------------
#define all of the KPI share:sjp
sjp.data=select(vjp,m1234,m34,m45,m1a2,m123,m124,m3,m4,m5)
sjp.data=(vjp$m6-sjp.data)

#select the best KPI with lowest volatility whichh is sigma--------------------------
sigma=apply(sjp.data,2,function (x) {
  return(var(x,na.rm=T))
})
sigma=sigma[order(sigma)]
barplot(sigma,main="1st order difference variance with some variables")


sjp.data=sjp.data[,names(sigma)]
sjp=cbind(year=vjp$year,sjp.data)

msjp=melt(sjp,id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title="Macau 1st-Order Difference Groupby Variance")
print(p1)
(p <- ggplot(filter(msjp,variable=="m45"), aes(year, value))+geom_line())

#here we select: m45
#mean(m6)(without2015)-mean(m45)(without2015)=m6(2015)-m45(2015)
#m6(2015)=mean(m6)(without2015)-mean(m45)(without2015)+m45(2015)
#2009---2014 mean (m6-m45)
predict.result=mean(vjp[-c(1,nrow(vjp)),"m6"]-vjp[-c(1,nrow(vjp)),"m45"])+vjp[nrow(vjp),"m45"]
prange=range(vjp[-c(1,nrow(vjp)),"m6"]-vjp[-c(1,nrow(vjp)),"m45"]+vjp[nrow(vjp),"m45"])

benchmark[3,]=data.frame(date=target.time,methdology="1st order Diff with M45",
                         floor=prange[1],
                         cap=prange[2],
                         bestguess=predict.result,stringsAsFactors = F)

benchmark












#=================================================================================================
#6 Linear Regression
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
hwm=select(fhw.all,date,cn,holiday,newyear,nbmonth,cn2hk,mcjdyd,cn12,mcjd,mcjd1,mcdc1,hkmcly1,hkmclygl1)

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

#time: 2014-09-201506
#var : cn2hk, 
#       mcdc1:less than 6 month: delete the qiyizhi
#       mcjd1
#factor: holiday newyear nbmonth
#---------------------------------------------------------------------------
#************determine the interval ****************************************
#---------------------------------------------------------------------------
#time.range=c(which(hwm$date==as.Date("2014-06-01")):which(hwm$date==as.Date("2015-05-01"))) #terrible
time.range=c(which(hwm$date==as.Date("2015-02-01")):which(hwm$date==as.Date("2015-05-01"))) #terrible
cat("The Date We Use in this algorithm\n", as.character(hwm[time.range,"date"]),"\n")
hwm.p=hwm[which(hwm$date==as.Date("2015-06-01")),]
hwm.t=hwm[time.range,]

cat("The correlation of the hotwords we may select:\n")
hwm.cor=cor(hwm.t$cn,hwm.t[,6:ncol(hwm.t)])
hwm.cor[order(-hwm.cor)]
names(hwm.t[,6:ncol(hwm.t)])[order(-hwm.cor)]
corrgram(hwm.t[,6:ncol(hwm.t)],order=T,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)


#5.4 pick up the best variables -----------------------------------------------------------
leaps=regsubsets(cn~cn2hk+mcjdyd+cn12+mcjd+mcjd1+mcdc1+hkmcly1+hkmclygl1+newyear+holiday,data=hwm.t,nbest=4)
plot(leaps,scale="adjr2",main="Variables Selection")

leaps=regsubsets(cn~mcdc1+hkmcly1+hkmclygl1+holiday,data=hwm.t,nbest=4)
plot(leaps,scale="adjr2",main="Variables Selection")


#5.5 fit--------------------------------------------------------------fit

#------------------------------------------------------------------------------
#******************determine the varaible and make linear regression***********
#------------------------------------------------------------------------------
# multilienar:lygl1 sumall1 sp1 lyqz1

hwfit=lm(cn~mcdc1+holiday,hwm.t)#holiday
hwfit=lm(cn~mcdc1,hwm.t)#165 # this prediction must be short interval:less than 

cat("The predic result:",predict(hwfit,newdata=hwm.p),"\n")
summary(hwfit)
#sqrt(vif(hwfit))

mse=sum(((fitted(hwfit)-hwm.t$cn)/hwm.t$cn)^2)^(1/2)
cat("lag=",30," mse=",mse,"\n")
error=(fitted(hwfit)-hwm.t$cn)/hwm.t$cn
hwm.t$predict=fitted(hwfit)
hwm.t$error=error
barplot(error,names.arg=hwm.t$date,ylab="ERROR")


gvmodel=gvlma(hwfit)
summary(gvmodel)
#multicollinearily
#sqrt(vif(hwfit))



#3.2 predict-----------------------------------------------------------predict
hwm.p$cn=predict(hwfit,newdata=hwm.p)
hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwm.p
hwm[which(hwm$date==hwm.p$date),]=hwm.p
cat("the predicted final value: ",as.character(hwm.p$date), hwm.p$cn,"\n")

#plot(hwm$date,hwm$cn,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(0,400000),main=("Prediction Vs Official"))
plot(hwm$date,hwm$cn,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(1000000,2000000),main=("Prediction Vs Official")) 
legend("topleft",legend=c("official", "predict"),lty=c(1,1,1,1),col=mycolor[c(1,3,5,7)])
lines(hwm[c(time.range,max(time.range+1)),"date"],c(fitted(hwfit),hwm.p$cn),type="p",pch=2,col=mycolor[3])
#lines(hwm$date,hwm$jg*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])

#3.3 evalation--------------------------------------------------------evaluation


hwm.t[,c("date","cn","predict","error")]
hwm[,c("date","cn")]


benchmark[4,]=data.frame(date=target.time,methdology="Linear Regression",
                         floor=1700000,
                         cap=1500000,
                         bestguess=hwm.p$cn,stringsAsFactors = F)

benchmark

MeanBestGuess=mean(benchmark$bestguess)
benchmark[5,]=data.frame(date=target.time,methdology="Mean",
                         floor=NA,
                         cap=NA,
                         bestguess=MeanBestGuess,stringsAsFactors = F)


