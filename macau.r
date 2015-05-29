library(quantmod)
library(xlsx)
library(ggplot2) #add for ggplot
library(leaps)
library(Hmisc) #describe
library(psych) #describe
library(GPArotation)
library(pastecs) #stat.desc
library(corrgram) # for corralation analysis
library(gvlma)
library(relaimpo)
library(reshape2)

##deal with CSV file
##1. delete the "," in number
##2. change the date to 
par(mfrow=c(1,1),mar=c(4,4,0,0))
mycolor=rainbow(20)
#----------------------------------------------------------------------------------------------
#1# read the data

#1.1 official JP arrival data and the currency---------------------------macau

macau=read.xlsx("macau.tourists.number.xls",1)
names(macau)=c("date","year","month", "tot","cn","hk","tw","au","br","ca","fr","ger","in","id","it","jp","my","hl","nz","flp","pty","kr","rs","sp","sa","spain","sw","tl","el","usa","vn","other")

mc=macau[,c("date","year","month","tot","cn","hk","tw")]
mc$cn1 = c(NA, mc$cn[1:(nrow(mc)-1)]);
mc$cn12= c(rep(NA, 12), mc$cn[1:(nrow(mc)-12)]);
mc$mm=mc$cn/mc$cn1
mc$yy=mc$cn/mc$cn12
mc=mc[order(mc$date),]

mc$month=NA
for(i in 1:nrow(mc)){
  ymd=strsplit(as.character(mc[i,"date"]),"-")
  ymd=ymd[[1]]
  mc[i,2:3]=ymd[1:2]
}
mc

#1.2 first look of the macau tourist number -----------------------------

p <- ggplot(mc, aes(as.numeric(month), cn))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))
print(p1)
p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)
print(p2)

p <- ggplot(mc, aes(date, cn))
p1 <- p + geom_line(aes(date, cn))+geom_line(aes(date, tot))+geom_line(aes(date, hk))
print(p1)

if (F){
  p <- ggplot(mc, aes(as.numeric(month), tot))
  p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))
  print(p1)
  p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)
  print(p2)
  p <- ggplot(mc, aes(as.numeric(month), hk))
  p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))
  print(p1)
  p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)
  print(p2)
}



#1.3 read the hotword of baidu index----------------------------------------
hotword=read.xlsx("macau.search.index.xlsx",1)
hwname=c("date","year","month","day","mc","mcdt","mcdb","mcdc","mcgwgl","mcjc","mcjd","mcjdyd","mcly","mclydt","mclygl",
                 "mclyjd","mcms","mcsn","mctq","mcy","mczyx","gatxz","gay","gaybj","hk","hkmc","hkmcly","hkmclygl")
names(hotword)=hwname

hw=hotword[,1:length(hwname)]
tail(hw)

for(i in 1:nrow(hw)){
  ymd=strsplit(as.character(hw[i,"date"]),"-")
  hw[i,2:4]=ymd[[1]]
}

#turn off the data to numeric
#wcgwgl hkmclygl
for (i in 5:ncol(hw)){
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

#change the daily date to month
hwd2m=aggregate(hw[,5:ncol(hw)],by=list(hw$year,hw$month),FUN=sum,na.rm=T)
names(hwd2m)[1:2]=c("year","month")
hwd2m=data.frame(date=hwd2m$month,hwd2m)
hwd2m$date=as.Date(paste(hwd2m$year,hwd2m$month,"01",sep="-"),"%Y-%m-%d")
hwd2m=hwd2m[order(hwd2m$date),]


#1.4 read the holiday and super days----------------------------------------
superday=read.xlsx("superday.xlsx",1)

#1.5 read HK data                   ----------------------------------------
hongkang=read.xlsx("hk.tourists.number.xlsx",1)
hk=hongkang[,c("Year","of.China")]
names(hk)=c("date","cn2hk")
hk
hk$cn2hk1 = c(NA, hk$cn2hk[1:(nrow(hk)-1)]);
hk$cn2hk12= c(rep(NA, 12), hk$cn2hk[1:(nrow(hk)-12)]);

#----------------------------------------------------------------------------------------------
#2# prepare the taining and prediction data to hwm
#2.1 #merge mc hwd2m superday to alldata--------------------------------------------------alldata

alldata=merge(hwd2m[which(hwd2m$date==as.Date("2011-01-01")):which(hwd2m$date==as.Date("2015-04-01")),],superday,by = intersect("date", "date"))
alldata=merge(alldata,hk,by = intersect("date", "date"))
alldata=merge(alldata,mc[,-which(names(mc)=="year"|names(mc)=="month"|names(mc)=="hk"|names(mc)=="tw")],by = intersect("date", "date"))
#order the all data
alldata=data.frame(alldata[,c("date","year","month","dpm","tot","cn","mm","yy")],
                   alldata[,which(names(alldata)=="mc"):which(names(alldata)=="hkmclygl")],
                   alldata[,which(names(alldata)=="holiday"):which(names(alldata)=="cn2hk12")],
                   alldata[,which(names(alldata)=="cn1"):which(names(alldata)=="cn12")])

objdata=alldata[,1:8]
cordata=alldata[,9:ncol(alldata)]

cormc=cor(alldata[1:nrow(alldata)-1,"cn"],cordata[1:nrow(alldata)-1,])
cordata=cordata[,order(-cormc)]
alldata=data.frame(objdata,cordata)

alldata1=cordata
alldata1[2:nrow(alldata1),]=alldata1[1:(nrow(alldata1)-1),]
alldata1[1,]=NA
alldata1$date=alldata$date
names(alldata)




if (F){
  # add some percent value
  hwm$mm=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"mm"]
  hwm$yy=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"yy"]
  hwm$nd12=NA;hwm$jgnd12=NA;hwm$lynd12=NA;hwm$m12=NA;hwm$m1234=NA;hwm$nd1234=NA
  strdate=as.character(hwm$date)
  hwm[grep("2013",strdate),"m12"]=(hwm[which(hwm$date==as.Date("2013-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2013-02-01")),"arrival"])
  hwm[grep("2014",strdate),"m12"]=(hwm[which(hwm$date==as.Date("2014-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2014-02-01")),"arrival"])
  hwm[grep("2015",strdate),"m12"]=(hwm[which(hwm$date==as.Date("2015-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2015-02-01")),"arrival"])
  hwm[grep("2013",strdate),"m1234"]=(hwm[which(hwm$date==as.Date("2013-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2013-02-01")),"arrival"]+hwm[which(hwm$date==as.Date("2013-03-01")),"arrival"]+hwm[which(hwm$date==as.Date("2013-04-01")),"arrival"])
  hwm[grep("2014",strdate),"m1234"]=(hwm[which(hwm$date==as.Date("2014-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2014-02-01")),"arrival"]+hwm[which(hwm$date==as.Date("2014-03-01")),"arrival"]+hwm[which(hwm$date==as.Date("2014-04-01")),"arrival"])
  hwm[grep("2015",strdate),"m1234"]=(hwm[which(hwm$date==as.Date("2015-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2015-02-01")),"arrival"]+hwm[which(hwm$date==as.Date("2015-03-01")),"arrival"]+hwm[which(hwm$date==as.Date("2015-04-01")),"arrival"])
  hwm$nd12=hwm$arrival/hwm$m12
  hwm$nd1234=hwm$arrival/hwm$m1234
  hwm[grep("2013",strdate),"jgnd12"]=hwm[grep("2013",strdate),"jg"]/(hwm[which(hwm$date==as.Date("2013-01-01")),"jg"]+hwm[which(hwm$date==as.Date("2013-02-01")),"jg"])
  hwm[grep("2014",strdate),"jgnd12"]=hwm[grep("2014",strdate),"jg"]/(hwm[which(hwm$date==as.Date("2014-01-01")),"jg"]+hwm[which(hwm$date==as.Date("2014-02-01")),"jg"])
  hwm[grep("2015",strdate),"jgnd12"]=hwm[grep("2015",strdate),"jg"]/(hwm[which(hwm$date==as.Date("2015-01-01")),"jg"]+hwm[which(hwm$date==as.Date("2015-02-01")),"jg"])
  hwm[grep("2013",strdate),"lynd12"]=hwm[grep("2013",strdate),"ly"]/(hwm[which(hwm$date==as.Date("2013-03-01")),"ly"]+hwm[which(hwm$date==as.Date("2013-02-01")),"ly"])
  hwm[grep("2014",strdate),"lynd12"]=hwm[grep("2014",strdate),"ly"]/(hwm[which(hwm$date==as.Date("2014-03-01")),"ly"]+hwm[which(hwm$date==as.Date("2014-02-01")),"ly"])
  hwm[grep("2015",strdate),"lynd12"]=hwm[grep("2015",strdate),"ly"]/(hwm[which(hwm$date==as.Date("2015-03-01")),"ly"]+hwm[which(hwm$date==as.Date("2015-02-01")),"ly"])
  
}

#2.2 hwm------------------------------------------------------------------hwm

inputflag=(which(names(alldata)=="yy")+1):ncol(alldata)#------MACRO Input location
objectflag=c(which(names(alldata)=="date"):which(names(alldata)=="yy"))
varname=names(alldata)

bdate=as.Date("2011-01-01")   #-----------MACRO DATE
tdate=as.Date("2015-03-01")
edate=as.Date("2015-04-01")

hwm=alldata
predict.no=1

usefuldate=hwm[which(hwm$date==bdate):which(hwm$date==edate),"date"]
cat("The Date We Use in this algorithm :  ", paste(bdate,edate,sep=" - "),"\n")
hwmt=hwm[which(hwm$date==bdate):which(hwm$date==tdate),]
hwmp=hwm[which(hwm$date==edate):which(hwm$date==edate),]


#----------------------------------------------------------------------------------------------
#3# select the best index

#3.1 correlation method-----------------------------------------------
cat("The correlation of the hotwords we may select:\n")
cor(hwmt$cn,hwmt[,inputflag])
corrgram(hwmt[,c(which(names(hwmt)=="cn"),inputflag)],order=T,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)
#  cn2hk   cn2hk12    mcjdyd      cn12      mcms      mcjd     gatxz    cn2hk1      mcjc       cn1      mcdb      hkmc      mctq
#0.8944697 0.8306905 0.8102572 0.7895684 0.7886437 0.7439273 0.7023497 0.6517773 0.6300267 0.6166936 0.6082949 0.6062449 0.5737031
#    mcsn   mcgwgl    mclyjd    mclydt      mcdc        mc    mclygl     mczyx  hkmclygl       hk    hkmcly       mcy   newyear
#0.572775 0.555073 0.5413292 0.5082319 0.5033097 0.4945047 0.4738809 0.4551576 0.4345739 0.384854 0.3595856 0.3477999 0.2297341
#  nbmonth          gay        mcly     holiday      mcdt     gaybj
#0.1655945 -0.006112162 -0.02387346 -0.09065025 -0.236874 -0.352684

#3.2 ploting method---------------------------------------------------
w=data.frame(name=names(hwmt)[inputflag])
w$cor=c(cor(hwmt$cn,hwmt[,inputflag]))
w$mean=c(apply(hwmt[,inputflag],2,mean))
w$w=mean(hwmt$cn)/w$mean
w
p=ggplot(hwmt, aes(date, cn),show_guide = TRUE) + geom_line(aes(date, cn),show_guide = TRUE)+geom_point()
mycolor=rainbow(40)
for (i in length(inputflag):1){
  n=inputflag[i]
  p1=p+geom_line(aes(date, hwmt[,n]*w[i,"w"]),color=mycolor[i])+labs(x="DATE", y=varname[n],title=paste("Visiter VS ",varname[n]))  
  p1=p1+geom_point(aes(date, hwmt[,n]*w[i,"w"]),color=mycolor[i])
  print(p1)  
}

#p2=p+geom_line(aes(date, cn2hk/2),color=mycolor[1])+labs(x="DATE", y="cn2hk")+
# scale_colour_manual("PID",values = c("419096_1006" = "red","419253_1006" = "blue"))
#print(p2)
if (F) {
plot(hwmt$date,hwmt$cn,type="b",lty=1,xlab="date",ylim=c(0,2400000))
legend("topleft",legend=c("official", "cn2hk", "cn2hk12"),lty=c(1,1,1,1),col=mycolor[c(1,3,4,4)])
lines(hwmt$date,hwmt$cn2hk/2,type="b",lty=1,xlab="date",col=mycolor[3])
lines(hwmt$date,hwmt$cn2hk12/3,type="b",lty=1,xlab="date",col=mycolor[4])
lines(hwmt$date,hwmt$mcjdyd*200,type="b",lty=1,xlab="date",col=mycolor[5])
lines(hwmt$date,hwmt$cn12,type="b",lty=1,xlab="date",col=mycolor[6])
lines(hwmt$date,hwmt$mcms*200,type="b",lty=1,xlab="date",col=mycolor[7])
lines(hwmt$date,hwmt$mcjd*150,type="b",lty=1,xlab="date",col=mycolor[8])
}


#3.3 modify the data  -----------------------------------------------

hwm=alldata[,1:8]
hwm=data.frame(hwm,alldata[,c("cn2hk","cn2hk12","mcjdyd","cn12","mcms","mcjd","mclyjd","mclygl")],
               gay1=alldata1$gay,mcy1=alldata1$mcy,mcly1=alldata1$mcly,hkmc1=alldata1$hkmc,
               hkmcly1=alldata1$hkmcly,hkmclygl1=alldata1$hkmclygl,gatxz1=alldata1$gatxz)
hwm
inputflag=(which(names(alldata)=="yy")+1):ncol(hwm)#------MACRO Input location
objectflag=c(which(names(alldata)=="date"):which(names(alldata)=="yy"))
varname=names(hwm)

bdate=as.Date("2011-01-01")   #-----------MACRO DATE
tdate=as.Date("2015-03-01")
edate=as.Date("2015-04-01")
predict.no=1

usefuldate=hwm[which(hwm$date==bdate):which(hwm$date==edate),"date"]
cat("The Date We Use in this algorithm :  ", paste(bdate,edate,sep=" - "),"\n")
hwmt=hwm[which(hwm$date==bdate):which(hwm$date==tdate),]
hwmp=hwm[which(hwm$date==edate):which(hwm$date==edate),]
cor(hwmt[-1,"cn"],hwmt[-1,inputflag])


#3.3 regsubset method  -----------------------------------------------
#namelink=NA
#for (i in 1:ncol(hwmt)){namelink=paste(namelink,names(hwmt)[i],sep="+")}
#namelink
       
#leaps=regsubsets(cn~cn2hk+cn2hk12+mcjdyd+cn12+mcms+mcjd+gatxz+cn2hk1+mcjc+cn1+mcdb+hkmc+mctq+mcsn+mcgwgl+
#                  mclyjd+mclydt+mcdc+mc+mclygl+mczyx+hkmclygl+hk+hkmcly+mcy+newyear+nbmonth+gay+mcly+
#                  holiday+mcdt+gaybj,data=hwmt,nbest=4)
leaps=regsubsets(cn~cn2hk+cn2hk12+mcjdyd+cn12+mcms+mcjd+gatxz+cn2hk1+mcjc+mcdb+hkmc+mctq+mcgwgl+
                  mclyjd+mclydt+mcdc+mclygl+mczyx+hkmcly+mcy+newyear+nbmonth+gay+mcly+holiday+mcdt,data=hwmt,nbest=4)
leaps=regsubsets(cn~cn2hk12+mcjdyd+cn12+mcms+mcjd+gatxz+cn2hk1+mcjc+mcdb+hkmc+mctq+mcgwgl+
                   mclyjd+mclydt+mcdc+mclygl+mczyx+hkmcly+mcy+newyear+nbmonth+gay+mcly+holiday+mcdt,data=hwmt,nbest=4)
plot(leaps,scale="adjr2")

#3.1 fit--------------------------------------------------------------fit
hwfit=lm(cn~cn2hk+mcjdyd+mcms+mcjc+mclyjd+mclygl+mcy,hwmt)
hwfit=lm(cn~mcjdyd+mcms+mcjc+mclyjd+mclygl+mcy,hwmt)#mcjc
summary(hwfit)
#cat("The hotwords we use to predict: jiage lvyou newyear\n")
#cat("The predic result:",predict(hwfit,newdata=hwmp),"\n")

#3.2 predict-----------------------------------------------------------predict

hwmp$cn=predict(hwfit,newdata=hwmp)
hwmp$mm=hwmp$cn/hwmp$cn1;
hwmp$yy=hwmp$cn/hwmp$cn12
hwmp
hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwmp
cat("the predicted final value: ",as.character(hwmp$date), hwmp$cn,"\n")


plot(hwm$date,hwm$cn,type="b",lty=1,xlab="date",ylim=c(1000000,2400000))
legend("topleft",legend=c("official", "predict"),lty=c(1,1,1,1),col=mycolor[c(1,3,5,7)])
lines(usefuldate,c(fitted(hwfit),hwmp$cn),type="b",pty=2,col=mycolor[3])
#lines(usefuldate,c(fitted(percentfit)*(585400),hwmp$nd12*585400),type="b",pty=2,col=mycolor[9])
lines(hwm$date,hwm$jg*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])
lines(hwm$date,hwm$ly*2,type="b",cex=1,lty=1,xlab="date",col=mycolor[7])

p=ggplot(hwm, aes(date, cn),show_guide = TRUE) + geom_line(aes(date, cn),show_guide = TRUE)+geom_point()
p1=p+geom_line(aes(usefuldate, c(fitted(hwfit),hwmp$cn)),color=mycolor[1])+labs(x="DATE", y="predict",title=paste("Official VS Predict"))  
p1=p1+geom_point(aes(usefuldate, c(fitted(hwfit),hwmp$cn)),color=mycolor[1])
print(p1) 

for (i in length(inputflag):1){
  n=inputflag[i]
  p1=p+geom_line(aes(date, hwmt[,n]*w[i,"w"]),color=mycolor[i])+labs(x="DATE", y=varname[n],title=paste("Visiter VS ",varname[n]))  
  p1=p1+geom_point(aes(date, hwmt[,n]*w[i,"w"]),color=mycolor[i])
  print(p1)  
}

if (F){
  lines(hwm$date,hwm$gwgl*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])
  lines(hwm$date,hwm$ctriphotel*30,type="b",cex=1,lty=1,xlab="date",col=mycolor[7])
  lines(hwm$date,hwm$jg*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[4])
  #lines(hwm$date,hwm$yh*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])
  #lines(hwm$date,hwm$jd*20,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])
  lines(hwm$date,hwm$gwgl*20,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])
  lines(hwm$date,hwm$sp/3,type="b",cex=1,lty=1,xlab="date",col=mycolor[6])
  
  legend("topleft",legend=c("official", "ctripreview","ctripvisa","ctriphotel","mfwguide","visa"),lty=c(1,1,1),col=mycolor[c(1,11,12,13,14,15)])
  lines(ctripreview$date,ctripreview$tot*40,type="b",cex=1,lty=1,xlab="date",col=mycolor[11])
  lines(ctripvisa$date,ctripvisa$cmtid*2000,type="b",cex=1,lty=1,xlab="date",col=mycolor[12])
  lines(ctriphotel$date,ctriphotel$comment*100,type="b",cex=1,lty=1,xlab="date",col=mycolor[13])
  lines(mfwguide$date,mfwguide$review*4000,type="b",cex=1,lty=1,xlab="date",col=mycolor[14])
  lines(visa$date,visa$user*10000,type="b",cex=1,lty=1,xlab="date",col=mycolor[15])
}

plot(hwm$date,hwm$mm,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(0,3))
legend("topleft",legend=c("arrival mm", "arrival yy","jgmm"),lty=c(1,1,1),col=mycolor[c(1,3,5)])
lines(hwm$date,hwm$yy,type="b",lty=1,xlab="date",col=mycolor[3],ylim=c(0,2))
jg=data.frame(date=hwm$date,jg=hwm$jg,jg1=c(NA, hwm$jg[1:(nrow(hwm)-1)]))
jg$mm=jg$jg/jg$jg1
lines(jg$date,jg$mm,type="b",lty=1,xlab="date",col=mycolor[5],ylim=c(0,2))


d20140102=hwm[which(hwm$date==as.Date("2014-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2014-02-01")),"arrival"]
d20150102=hwm[which(hwm$date==as.Date("2015-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2015-02-01")),"arrival"]
d201403=hwm[which(hwm$date==as.Date("2014-03-01")),"arrival"]
d201503=hwm[which(hwm$date==as.Date("2015-03-01")),"arrival"]
d201404=hwm[which(hwm$date==as.Date("2014-04-01")),"arrival"]
d201504=hwm[which(hwm$date==as.Date("2015-04-01")),"arrival"]
resmm=data.frame(year=c("2014","2015","delta"),m3dm1am2=c(d201403/d20140102,d201503/d20150102,d201503/(d20150102)-d201403/d20140102),
                 m4dm1am2=c(d201404/d20140102,d201504/d20150102,d201504/d20150102-d201404/d20140102))
resyy=data.frame(year=c("20150102","201503","201504"),delta=c(d20150102/d20140102-1,d201503/d201403-1,d201504/d201404-1))
resmm
resyy


#3.3 evalation--------------------------------------------------------evaluation
mse=sum(((fitted(hwfit)-hwmt$arrival)/hwmt$arrival)^2)^(1/2)
cat("lag=",lag," mse=",mse,"\n")
error=(fitted(hwfit)-hwmt$arrival)/hwmt$arrival
hwmt$predict=fitted(hwfit)
hwmt$error=error
barplot(error,names.arg=hwmt$date,ylab="ERROR")


jp.p=jp
jp.p[which(jp.p$date==hwmp$date),"arrival"]=hwmp$arrival
jp.p=jp.p[40:162,]

plot(jp.p$date,jp.p$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(50000,400000))
legend("topleft",legend=c("japan tourist number"),lty=c(1),col=mycolor[c(1)])

jp.p[,1:2]
hwmt[,c("date","arrival","predict","error")]
hwm[,c("date","arrival")]



gvmodel=gvlma(hwfit)
summary(gvmodel)

#multicollinearily
sqrt(vif(hwfit))


cor(hwmt$arrival,hwmt[,2:ncol(hwm)])
corrgram(hwmt,order=T,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)

par(mfrow=c(1,1),mar=c(4,4,0,0))

leaps=regsubsets(arrival~qz+jg+jd+arrival1+arrival12+lyqz+yh+gw+sp,data=hwmt,nbest=5)
leaps=regsubsets(arrival~jg+yh+arrival12+jd+gwgl,data=hwmt,nbest=2)
leaps=regsubsets(arrival~qz+ly+y+tq+jd+hl+lyqz+jg+dt+lvgl+yh+zyx+gwgl+gw+lyjd+sp+arrival1+arrival12+jpy,data=hwmt,nbest=3)

leaps=regsubsets(arrival~qz+ly+y+tq+jd+ryfy+hl+lyqz+jg+dt+lvgl+yh+zyx+gwgl+gw+lyjd+sp+arrival1+arrival12+holiday+nbmonth,data=hwmt,nbest=4)
leaps=regsubsets(arrival~ly+tq+jd+lyqz+jg+lvgl+yh+zyx+gwgl+sp+arrival1+arrival12+holiday+nbmonth+newyear,data=hwmt,nbest=4)
plot(leaps,scale="adjr2")

save(file="japan.Rdata",japan,jp,hotword,hwm,currency)
load("japan.Rdata")
