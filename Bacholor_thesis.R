### cryprocurrency data 

# import bitcoin price dataset
BTC=read.csv("btcPrice.csv")
BTC[,1]=as.Date(BTC[,1],format="%Y/%m/%d") # change the class of first row from factor into date
# dickey-fuller test for unit root for BCT price
library(tseries)
adf.test(BTC[,2],k=7)  # shows unit root -> use log return
BTC[2:2539,3]=diff(log(BTC[,2]))*100 # ln(P_t) - ln(P_t-1)
colnames(BTC)=c("Date", "BTC price","BTC log return")
adf.test(na.omit(BTC[,3]),k=7) # alt_h: stationary
plot(BTC$Date,BTC$`BTC price`,type="l",xlab="Date", ylab="BTC price", sub="Figure 1: Price of Bitcion")　# Figure1
plot(BTC$Date,BTC$`BTC log return`,type="l", xlab="Date", ylab="BTC return", sub="Figure 3: log return of Bitcoin") # Figure3  

# ethereum
ETH=read.table("etherprice",header = F, sep = ";")  # data from 30/7/2015 to 17/7/2017, daily
ETH=ETH[-c(1:12,703:719),] # data from 11/8/2015 to 30/6/2017, daily. data till 11/8/2015 contains 0.00
ETH[,3]=seq(from=as.Date("2015/8/11"), to=as.Date("2017/6/30"), by="day")
ETH=ETH[,c(3,2)]
colnames(ETH)=c("Date","ethPrice")
plot(ETH$Date,ETH$ethPrice,type="l", xlab="Date",ylab="ETH price",sub="Figure 2: Price of Ether") # Figure 2
adf.test(ETH[,2],k=7)    # unit root
ETH[2:690,3]=diff(log(ETH$ethPrice))*100
colnames(ETH)=c("Date","ethPrice","log return eth")
plot(ETH$Date,ETH$`log return eth`,type="l", xlab="Date",ylab="ETH return",sub="Figure 4: log return of Ether") #Figure4
adf.test(na.omit(ETH$`log return eth`),k=7)  # stationarity



###  STOCK Price  ###

# import SP500 dataset
SP500 = read.csv("SP500.csv")
SP500[,1]=as.Date(SP500[,1],format="%Y/%m/%d")
SP500[,2]=as.numeric(as.character(SP500[,2]))
adf.test(na.omit(SP500[,2]),k=5)  # unit root
SP500[2:1815,3]=diff(log(SP500[,2]))*100
colnames(SP500)[3]="SP500 log return"
adf.test(na.omit(SP500[,3]),k=5)  # stationarity
plot(SP500$Date,SP500$SP500,type="l")
plot(SP500$Date,SP500$`SP500 log return`,type="l")

# import NIKKEI
NIKKEI=read.csv("NIKKEI225.csv")
NIKKEI[,1]=as.Date(NIKKEI[,1])
NIKKEI[,2]=as.numeric(as.character(NIKKEI[,2]))
adf.test(na.omit(NIKKEI[,2]),k=5)  # unit root
NIKKEI[-1,3]=diff(log(NIKKEI[,2]))*100
colnames(NIKKEI)[c(1,3)]=c("Date","NIKKEI log return")
adf.test(na.omit(NIKKEI$`NIKKEI log return`),k=5) # stationary
plot(NIKKEI$Date,NIKKEI$NIKKEI225,type="l")
plot(NIKKEI$Date,NIKKEI$`NIKKEI log return`,type="l")

# STOXX50
STOXX= read.csv("STOXX50E.csv")
STOXX=STOXX[c(1,5)]
STOXX[,1]=as.Date(STOXX[,1])
STOXX[,2]=as.numeric(as.character(STOXX[,2]))
adf.test(na.omit(STOXX[,2]),k=5) #unit root
STOXX[-1,3]=diff(log(STOXX[,2]))*100
colnames(STOXX)[c(2,3)]=c("STOXX50","STOXX log return")
adf.test(na.omit(STOXX$`STOXX log return`),k=5)  # stationary
plot(STOXX$Date,STOXX$STOXX50,type="l")
plot(STOXX$Date,STOXX$`STOXX log return`,type="l")


###  Gold  ###
Gold=read.csv("GOLD.csv") # GOLD, price in london market
Gold[,1]=as.Date(Gold[,1])
Gold[,2]=as.numeric(as.character(Gold[,2]))
adf.test(na.omit(G[,2]),k=5)  # unit root
Gold[-1,3]= diff(log(Gold[,2]))*100
colnames(Gold)=c("Date","GOLD", "GOLD log return")
adf.test(na.omit(Gold[,3]),k=5) # stationary
plot(Gold$Date,Gold$GOLD,type="l")
plot(Gold$Date,Gold$`GOLD log return`,type="l")

###  Exchange rate  ###
EXC=read.csv("ExcRate.csv")
EXC=EXC[1:3]
EXC[,1]=as.Date(EXC[,1])
EXC[,2]=as.numeric(as.character(EXC[,2]))
EXC[,2]=1/EXC[,2]
EXC[,3]=as.numeric(as.character(EXC[,3]))
adf.test(na.omit(EXC[,2]),k=5)  # unit root
adf.test(na.omit(EXC[,3]),k=5)  # unit root
EXC[-1,4]=diff(log(EXC[,2]))*100 
EXC[-1,5]=diff(log(EXC[,3]))*100
colnames(EXC)=c("Date","USD/EUR","USD/JPY","ln(USD/EUR)","ln(USD/JPY)")
adf.test(na.omit(EXC[,4]),k=5) #stationary
adf.test(na.omit(EXC[,5]),k=5) #stationary
plot(EXC$Date,EXC$`USD/EUR`,type="l")
plot(EXC$Date,EXC$`USD/JPY`,type="l")
plot(EXC$Date,EXC$`ln(USD/EUR)`,type="l")
plot(EXC$Date,EXC$`ln(USD/JPY)`,type="l")


###  Federal Fund Rate  ###
FFrate=read.csv("FFrate.csv") # effective FF rate, daily
FFrate=FFrate[c(1,3)]
FFrate[,1]=as.Date(FFrate[,1])
FFrate[,2]= as.numeric(as.character(FFrate[,2]))
adf.test(na.omit(FFrate[,2]),k=5)  # stationary
colnames(FFrate)=c("Date","FFrate")
plot(FFrate$Date,FFrate$FFrate,type="l")


## merge multiple datasets
library(plyr)
DS=join_all(list(BTC,ETH,SP500,STOXX,NIKKEI, Gold, EXC, FFrate),by="Date",type="left")
# get summary stats
summary(DS)
library(timeSeries)
# colSds(DS) ... doesnt compute well?

# dataset for garch (Pt~P_t-1 + Ct-1, C:covatiates)
colnames(DS)
#Dataset=data.frame(DS[-2539,3],DS[-1,3],DS[-1,7],DS[-1,9],DS[-1,11],DS[-1,13],DS[-1,15],DS[-1,17],DS[-1,18])
Dataset2=data.frame(DS[-2539,3],DS[-1,3],DS[-2539,7],DS[-2539,9],DS[-2539,11],DS[-2539,13],DS[-2539,15],DS[-2539,17],DS[-2539,18])
library(stats)
#Dataset_complete=subset(Dataset,complete.cases(Dataset)==T)  
Dataset_complete2=subset(Dataset2,complete.cases(Dataset2)==T)
#colnames(Dataset_complete)=c("BTC return","BTC return_lag1", "SP500 return_lag1", "STOXX50 return_lag1","NIKKEI return_lag1","GOLD return_lag1","ln(USD/EUR)_lag1","ln(USD/JPY)_lag1","FFrate_lag1")
colnames(Dataset_complete2)=c("BTC return","BTC return_lag1", "SP500 return", "STOXX50 return","NIKKEI return","GOLD return","ln(USD/EUR)","ln(USD/JPY)","FFrate")
#mean= lm(`BTC return`~.,data = Dataset_complete); summary(mean)
mean2= lm(`BTC return`~.,data = Dataset_complete2); summary(mean2)
plot(mean2)  # qqnorm to visialise distribution of resid
Date.btc=subset(DS,complete.cases(DS[,-c(4:5)])==T)[,1]
plot(x=Date.btc,y = mean2$resid,xlab="Date",ylab="resid",sub="Figure 5: Residuals of Eqn (19)")   # figure5
# chech the existence of heteroscedasticity in residual of mean eqn
library(FinTS)
ArchTest(mean2$resid,lags=1) # ARCH effect in resid --> ready to use garch

# GARCH model; univaridate
library(fGarch)
garch.btc=garchFit(~arma(1,0)+garch(1,1),data = DS$`BTC log return`[-1],trace = F); garch.btc
# garchFit(~garch(1,1),data = DS$`BTC log return`[-1],trace = F)
plot(x=DS$Date[-1],y=garch.btc@residuals,xlab="Date",ylab="resid",sub="residuals of eqn (19)")
summary(lm(garch.btc@residuals^2~DS$Date[-1]))
#plot(DS$Date[-1],garch.btc@residuals^2,type="l")
#abline(lm(garch.btc@residuals^2~DS$Date[-1]))

# garch for eth
colnames(DS)
Date.eth=subset(DS,complete.cases(DS)==T)[,1] # use when plot resid
#Dataset.eth=data.frame(DS[-2539,5],DS[-1,5],DS[-1,7],DS[-1,9],DS[-1,11],DS[-1,13],DS[-1,15],DS[-1,17],DS[-1,18])
Dataset.eth2=data.frame(DS[-2539,5],DS[-1,5],DS[-2539,7],DS[-2539,9],DS[-2539,11],DS[-2539,13],DS[-2539,15],DS[-2539,17],DS[-2539,18])
str(DS)
View(DS)
library(stats)
#Dataset_complete.eth=subset(Dataset.eth,complete.cases(Dataset.eth)==T)
Dataset_complete.eth2=subset(Dataset.eth2,complete.cases(Dataset.eth2)==T)
#colnames(Dataset_complete.eth)=c("ETH return","ETH return_lag1", "SP500 return_lag1", "STOXX50 return_lag1","NIKKEI return_lag1","GOLD return_lag1","ln(USD/EUR)_lag1","ln(USD/JPY)_lag1","FFrate_lag1")
colnames(Dataset_complete.eth2)=c("ETH return","ETH return_lag1", "SP500 return", "STOXX50 return","NIKKEI return","GOLD return","ln(USD/EUR)","ln(USD/JPY)","FFrate")
# mean.eth= lm(`ETH return`~.,data = Dataset_complete.eth); summary(mean.eth) # multiARIMA
mean.eth2=lm(`ETH return`~.,data = Dataset_complete.eth2); summary(mean.eth2)
# chech the existence of heteroscedasticity in residual of mean eqn
plot(mean.eth2)
plot(x=Date.eth,y=mean.eth2$resid,xlab="Date",ylab="resid",sub="Figure 6: Residuals of Eqn (20)") # Figure6
library(FinTS)
ArchTest(mean.eth2$resid,lags=1) # ARCH effect in resid --> ready to use garch
ArchTest(lm(`ETH return`~`ETH return_lag1`,data=Dataset_complete.eth2)$residuals,lag=1)
garchFit(~arma(1,0)+garch(1,1),data=ETH$`log return eth`[-1],trace=F)
garch.eth=garchFit(~garch(1,1),data=ETH$`log return eth`[-1],include.mean = F,trace=F); garch.eth
plot(x=DS$Date[c(1851:2539)],y=garch.eth@residuals,xlab="Date",ylab="resid",sub="residuals of eqn (24)")


### volatility comparison w/fiat currency
sdbtc=sd(na.omit(DS$`BTC log return`))
sdeth=sd(na.omit(DS$`log return eth`))
sdgold=sd(na.omit(DS$`GOLD log return`))
sdeur=sd(na.omit(DS$`ln(USD/JPY)`))
sdjpy=sd(na.omit(DS$`ln(USD/JPY)`))
sdbtc365=sd(DS$`BTC log return`[c(2175:2539)])
sdeth365=sd(DS$`log return eth`[c(2175:2539)])

barplot(c(sdbtc,sdeth,sdgold,sdeur,sdjpy,sdbtc365,sdeth365),names.arg = c("BTC","ETH","Gold","EUR","JPY","BTC365","ETH365"),ylab="volatility of daily return in USD prices",sub="Volatility of bitcoin and ether compared to EUR, JPY and gold")

plot(DS$Date[-1], garch.btc@sigma.t,type="l",xlab="Date",ylab="conditional SDs, BTC",sub="Figure 7: Historical volatility of bitcoin")

plot(DS$Date[c(1851:2539)],garch.eth@sigma.t,type="l",xlab="Date",ylab="conditional SDs, ETH",sub="Figure 8: Historical volatility of ether")

garch.eur=garchFit(~garch(1,1),data=na.omit(DS$`ln(USD/EUR)`),trace=F)
dateexc=subset(EXC,complete.cases(EXC)==T)$Date
plot(dateexc,garch.eur@sigma.t,type="l", xlab="Date",ylab="conditional SDs, EUR",sub="Figure 9: Historical volatility of Euro")

garch.jpy=garchFit(~garch(1,1),data=na.omit(DS$`ln(USD/JPY)`),trace=F)
plot(dateexc,garch.jpy@sigma.t,type="l",xlab="Date",ylab="conditional SDs, JPY",sub="Figure 10: Historical volatility of Yen")

garch.gold=garchFit(~garch(1,1),data = na.omit(DS$`GOLD log return`),trace=F)
dategold=subset(Gold,complete.cases(Gold)==T)$Date
plot(dategold[-1],garch.gold@sigma.t[-1],type="l",xlab="Date",ylab="conditional SDs, GOLD",sub="Figure 11: Historical volatility of gold")


## Vitamin shoppe
Vitamin = read.csv("VSI.csv")
Vitamin = Vitamin[c(1,5)]
Vitamin[,1] = as.Date(Vitamin[,1], format="%Y-%m-%d")
adf.test(Vitamin[,2],k=1)
library(plyr)
Vitamin = join(Vitamin,BTC,by="Date")
summary(lm(Close~`BTC log return`,data = Vitamin))




"""#### redundant codes below
library(fGarch)
garchFit(~arma(1,0)+garch(1,1),data = DS$`BTC log return`[-1],trace = F)
garchFit(GARCH11~arma(1,0)+garch(1,1),data = Dataset_complete,trace = F)
X.zoo=zoo(Dataset_complete)
garchFit(~garch(1,1),data = X.zoo)

colnames(dataframe_variance)

var(predict(object = mean))
head(mean$resid)

dccspec(VAR=T, enternal.regressor=Dataset_complete)

library(fGarch)
#install.packages("TSA")
library(TSA)
xreg=DS[c(5,7,9,11,14,15,16,17)]
arima.fit=arimax(x=DS$`BTC log return`, order = c(1,0,0),xreg = xreg[-1]);arima.fit
summary(arima.fit)
arima.fit$residuals

plot(x=DS$Date,y = arima.fit$resid)   # showing the heteroscedasticity of ARMA
ArchTest(na.exclude(arima.fit$resid),lags=1)   # test rejects the null hypothesis of no heteroscedasticity in resid
apply()
library(rmgarch)
install.packages("aTSA")
library(aTSA)

# ArchTest{FinTS}
install.packages("FinTS")
library(FinTS)
ArchTest()

library(forecast)
auto.arima(DS$`BTC log return`, trace = T)
rmgarch::.__C__mGARCHfit

arima.lm=lm(DS[-2539,3]~DS[-1,3]+xreg[-1,1]+xreg[-1,2]+xreg[-1,3]+xreg[-1,4]+xreg[-1,5]+xreg[-1,6]+xreg[-1,7]+xreg[-1,8])
summary(arima.lm)
ArchTest(arima.lm$resid)

# ARCH(1)
dataframe_variance=data.frame(mean$resid^2,Dataset_complete[,-c(1,2)])
mean.resid.2.lag1=dataframe_variance$mean.resid.2[-1]
dataframe_variance=dataframe_variance[1:1325,]
dataframe_variance=data.frame(dataframe_variance,mean.resid.2.lag1)  
summary(lm(mean.resid.2~. ,data= dataframe_variance))

FinTS.stats(DS$SP500)
library(rugarch)

library(stats)
predict(mean,n.ahead=10)

#install.packages("MASS")
library(MASS)
rlm(`ETH return`~.,data = Dataset_complete.eth)
summary(glm(`ETH return`~.,data = Dataset_complete.eth))

sink("eth.txt")
summary(mean.eth)
sink()

glm()

length(Date.btc)
length(DS$Date)

plot(density(garch.btc@residuals/garch.btc@sigma.t))
lines(-300:300/100,dnorm(-300:300/100,0,1))

library(e1071)
kurtosis(garch.btc@residuals/garch.btc@sigma.t)

# v_i follows skewed dist
qqnorm(garch.btc@residuals/garch.btc@sigma.t, sub="qqplot, bitcoin garch resid")
abline(0,1)
shapiro.test(garch.btc@residuals/garch.btc@sigma.t)
# resid follows skew normal dist
garchskew.btc= garchFit(~arma(1,0)+garch(1,1),data = DS$`BTC log return`[-1],trace = F); garchskew.btc
library(lattice)
qqmath(~garchskew.btc@residuals/garch.btc@sigma.t,distribution = function(p){qsnorm(p,xi=0.925575)},
       abline=c(0,1),xlab = "theoretical quantiles",ylab = "sample quantiles") 
ks.test(garchskew.btc@residuals/garchskew.btc@sigma.t,"psnorm",xi=0.925575)

qqnorm(garch.eth@residuals/garch.eth@sigma.t,sub="qqplot, ether garch resid")
abline(0,1)
shapiro.test(garch.eth@residuals/garch.eth@sigma.t)
length(na.exclude(DS[,5]))

install.packages("fNonlinear")
library(fNonlinear)
bdsTest(garch.btc@residuals)

####"""

