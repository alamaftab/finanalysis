#!/usr/bin/Rscript


source("c:/aftab/R/finanalysis/config/finanalysis.cfg")
source(paste(Home, "/data/parm/ImpVolChartCalSpread.in", sep = ''))

source(paste(Home, "/lib/impliedVolLib.R", sep = ''))


dataPathHist= histFile
dataPathOpt= optFileIn1

histData <- read.table(dataPathHist,skip=1, header=F, sep="|")
OptData <- read.table(dataPathOpt,skip=1, header=F, sep="|")

data.entry(dataPathOpt, dataPathHist)

#tt[k]=strptime(tab$autos_data.Date[k], "%Y-%m-%d", tz="")

sharePrice = OptData[1,6]
downloadDate =strptime( OptData[1,2], "%Y%m%d", tz="")

StrikeDate = strptime(OptData[1,4], "%y%m%d", tz="")

# get the range of price closer to strike price so band would be celling(shareprice) and floor(shareprice)

lowerRange = floor(sharePrice -10)
upperRange = ceiling(sharePrice + 10)

daysToExpire = as.numeric((StrikeDate  - downloadDate)*5/7)
daysToExpire 

tt <- OptData[OptData$V7 >= lowerRange & OptData$V7 <= upperRange & OptData$V5 == putOrCall & OptData$V14 > 30 & (grepl(otherFilter,OptData[,8])),]
tt

ttRowCount = as.numeric(nrow(tt))
dfForGraph = data.frame()
for (i in 1:ttRowCount)
{
  strikePrice = tt[i,7]
  OptPrice = as.numeric(tt[i,11])
  
  impvol =implied.vol(sharePrice,strikePrice,.012,(daysToExpire/252),OptPrice, putOrCall)
  print(strikePrice) 
  print(impvol)
  dfForGraph <- rbind(dfForGraph,c(strikePrice,impvol))
  
  
}

# plot(x,y)



## hIST vol calculation

#last20DaysHistPrice = histData[1:21,8]

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

plot(dfForGraph[,1],dfForGraph[,2], ylim= range(0,8))
abline(histvol,0)
abline(histvolmax,0)



