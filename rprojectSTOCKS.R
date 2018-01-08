#install.packages("quantmod")
#install.packages("tseries")
#install.packages("lubridate")
#install.packages("PerformanceAnalytics")
library(lubridate)
library(tseries)
library(quantmod)
#install.packages("data.table")
library(data.table)
library(ggplot2)
library(PerformanceAnalytics)
listofsymbols=c("GE","RGLD","VZ","AAPL")
allprice=NULL
i=1
for(tkr in listofsymbols)
{
  
  tkrprice=get.hist.quote(instrument=tkr,start="2008-01-03",end="2016-12-03",quote="AdjClose",provider="yahoo",origin="1970-01-01",compression="m",retclass="zoo")
  if(i==1)
  {
    allprice=tkrprice
    
  }  
  
  else
  {
    allprice=merge(allprice,tkrprice)
    
  }
  
  i=i+1
  
}

getSymbols("AAPL")
barChart(AAPL,bar.type="hlc")
colnames(allprice)=listofsymbols
allprice.xts=allprice

str(allprice)
allcontcompreturns=diff(log(allprice))

colnames(allcontcompreturns)=paste0(rep("ccret_",length(listofsymbols)),listofsymbols)
allcontcompreturns.xts=allcontcompreturns
allprice$year_price=year(index(allprice))
allprice$month_price=month(index(allprice))
allcontcompreturns$year_price=year(index(allcontcompreturns))
allcontcompreturns$month_price=month(index(allcontcompreturns))

allprice
coredata(allprice)
allprice=data.table(as.data.frame(coredata(allprice)))
returnanalysis=data.table(as.data.frame(coredata(allcontcompreturns)))
allprice
allprice_if=data.table(melt(allprice,id.vars=c("year_price","month_price")))
returnanalysis_if=data.table(melt(returnanalysis,id.vars=c("year_price","month_price")))
colnames(allprice_if)=c("year_price","month_price","stock","price")
colnames(returnanalysis_if)=c("year_price","month_price","stock","cc_return")
allprice_if

#########################################################################
prices_mean_by_year=allprice_if[,list(avg_price=mean(price),med_price=median(price),sd_price=sd(price)),by=list(year_price)]
prices_mean_by_year_stock=allprice_if[,list(avg_price=mean(price),med_price=median(price),sd_price=sd(price)),by=list(year_price,stock)]

###################################################################################
##############graphs#################################################################

##############How is average stock price moving over the years################
ggplot(prices_mean_by_year_stock,aes(x=year_price,y=avg_price,color=stock,group=stock))+geom_line(size=2)+
labs(x="year",y="Average Price",title="Average Price of the stock over the years")
#############Showing variability of the stocks across the months across the years
ggplot(allprice_if,aes(x=as.factor(month_price),y=price,color=stock,group=stock))+geom_line(size=1)+facet_wrap(~year_price,scales="free_y")+
labs(x="Month",y="Price",title="Price distribution of the stock across the years")
##########################################################################################
###############median prices of stocks across all years..... AGAIN A MEASURE OF VARIABILITY   ######################
ggplot(allprice_if,aes(x=stock,y=price,color=stock,group=stock))+geom_boxplot()+
  labs(x="Stocks",y="Price",title="Price distribution of every stock across all years ")

###########remind me to show you a cool graph but not relevant to our study############
###################CORRELATION BETWEEN VARIOUS STOCKS
cor(allprice[,-c(6,5),with=FALSE])

pairs(allprice[,-c(6,5),with=FALSE])
###############BOXPLOT OF PRICE AND RETURNS DISTRIBUTION
chart.Boxplot(allprice.xts,legend.loc="bottom",main="Price Distribution")
chart.Boxplot(allcontcompreturns.xts,legend.loc="bottom",main="Return Distribution")

#####################future value of 1$ invested
chart.CumReturns(diff(allprice.xts)/lag.xts(allprice.xts,k=1),legend.loc="topleft",wealth.index=TRUE,main="Future value of $ 1 invested")
