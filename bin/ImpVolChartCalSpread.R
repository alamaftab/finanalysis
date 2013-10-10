#!/usr/bin/Rscript

source("c:/aftab/R/finanalysis/config/finanalysis.cfg")
source(paste(Home, "/data/parm/ImpVolChartCalSpread.in", sep = ''))

source(paste(Home, "/lib/impliedVolLib.R", sep = ''))

dataPathHist=histFile
dataPathOpt1=optFileIn1
dataPathOpt2=optFileIn2

histData <- read.table(dataPathHist,skip=1, header=F, sep="|")
OptData1 <- read.table(dataPathOpt1,skip=1, header=F, sep="|")
OptData2 <- read.table(dataPathOpt2,skip=1, header=F, sep="|")

#tt[k]=strptime(tab$autos_data.Date[k], "%Y-%m-%d", tz="")

sharePrice = OptData1[1,6]
downloadDate =strptime( OptData1[1,2], "%Y%m%d", tz="")

StrikeDate1 = strptime(OptData1[1,4], "%y%m%d", tz="")
StrikeDate2 = strptime(OptData2[1,4], "%y%m%d", tz="")

# get the range of price closer to strike price so band would be celling(shareprice) and floor(shareprice)

lowerRange = floor(sharePrice -15)
upperRange = ceiling(sharePrice + 10)

daysToExpire1 = as.numeric((StrikeDate1  - downloadDate)*5/7)
daysToExpire2 = as.numeric((StrikeDate2  - downloadDate)*5/7)


tt1 <- OptData1[OptData1$V7 >= lowerRange & OptData1$V7 <= upperRange & OptData1$V5 == "P" & OptData1$V14 > 3 & (grepl("MSFT131019",OptData1[,8])),]
tt1
tt2 <- OptData2[OptData2$V7 >= lowerRange & OptData2$V7 <= upperRange & OptData2$V5 == "P" & OptData2$V14 > 3 & (grepl("MSFT131116",OptData2[,8])),]
tt2

ttRowCount1 = as.numeric(nrow(tt1))
dfForGraph1 = data.frame()
for (i in 1:ttRowCount1)
{
  strikePrice = tt1[i,7]
  OptPrice1 = as.numeric(tt2[i,11])
  
  impvol1 =implied.vol(sharePrice,strikePrice,.012,(daysToExpire1/252),OptPrice1, "P")
  print(strikePrice) 
  print(impvol1)
  dfForGraph1 <- rbind(dfForGraph1,c(strikePrice,impvol1))
  
 }


ttRowCount2 = as.numeric(nrow(tt2))
dfForGraph2 = data.frame()
for (i in 1:ttRowCount2)
{
  strikePrice = tt2[i,7]
  OptPrice2 = as.numeric(tt2[i,11])
  
  impvol2 =implied.vol(sharePrice,strikePrice,.012,(daysToExpire2/252),OptPrice2, "P")
  print(strikePrice) 
  print(impvol2)
  dfForGraph2 <- rbind(dfForGraph2,c(strikePrice,impvol2))
  
}

histrowcount = as.numeric(nrow(histData))

lnArray <- c(1.1)

for ( i in 2:histrowcount)
{
  lnArray[(i-1)]=log(histData[(i-1),8]/histData[(i),8])
}

noOfDaysForVolCalculation = 20

histvolArray = c(1.1)
for ( j in 1:( histrowcount -1 - noOfDaysForVolCalculation))
{
  histvolArray[j]=sd(lnArray[j:(j + noOfDaysForVolCalculation -1)])*sqrt(252)
}

histvolmax =  max(histvolArray)
histvol =  histvolArray[1]

par( mfrow = c( 1, 2 ) )

plot(dfForGraph1[,1],dfForGraph1[,2], ylim= range(0,5), col="green", lty=1,lwd = 2)
#plot(dfForGraph2[,1],dfForGraph2[,2])
abline(histvol,0)
abline(histvolmax,0)
points(dfForGraph2[,1],dfForGraph2[,2], col="red", lty=1,lwd = 2)
abline(v=55)

plot(tt1[,7],tt1[,11])
points(tt2[,7],tt2[,12])

merge_tt1_tt2 <- merge(tt1,tt2,by.x = "V7", by.y = "V7")

points(merge_tt1_tt2$V7,(merge_tt1_tt2$V12.y - merge_tt1_tt2$V11.x), col="red", lty=1,lwd = 2)

abline(h=.2)
abline(v=32)


par( mfrow = c( 1, 1 ) )

