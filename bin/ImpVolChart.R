#!/usr/bin/Rscript

source("C:/aftab/R/finanalysis/lib/impliedVolLib.R")

dataPathHist="C:/aftab/R/finanalysis/data/in/^VIX/HIST/^ViX_Hist.txt"
dataPathOpt="C:/aftab/R/finanalysis/data/in/SPY/OPT/SPY_2013-04_20130411122355.txt"
histData <- read.table(dataPathHist,skip=1, header=F, sep="|")
OptData <- read.table(dataPathOpt,skip=1, header=F, sep="|")

#tt[k]=strptime(tab$autos_data.Date[k], "%Y-%m-%d", tz="")

sharePrice = OptData[1,6]
downloadDate =strptime( OptData[1,2], "%Y%m%d", tz="")

StrikeDate = strptime(OptData[1,4], "%y%m%d", tz="")

# get the range of price closer to strike price so band would be celling(shareprice) and floor(shareprice)

lowerRange = floor(sharePrice -10)
upperRange = ceiling(sharePrice + 20)

daysToExpire = as.numeric((StrikeDate  - downloadDate)*5/7)
daysToExpire 

tt <- OptData[OptData$V7 >= lowerRange & OptData$V7 <= upperRange & OptData$V5 == "P" & OptData$V14 > 3000 & (grepl("SPY130420",OptData[,8])),]
tt

ttRowCount = as.numeric(nrow(tt))
dfForGraph = data.frame()
for (i in 1:ttRowCount)
{
  strikePrice = tt[i,7]
  OptPrice = as.numeric(tt[i,11])
  
  impvol =implied.vol(sharePrice,strikePrice,.012,(daysToExpire/252),OptPrice, "P")
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

plot(dfForGraph[,1],dfForGraph[,2], ylim= range(0,10))
abline(histvol,0)
abline(histvolmax,0)



