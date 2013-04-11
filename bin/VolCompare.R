source("C:/aftab/R/finanalysis/lib/impliedVolLib.R")

dataPathHist="C:/aftab/R/finanalysis/data/in/SPY/HIST/SPY_Hist.txt"
dataPathOpt="C:/aftab/R/finanalysis/data/in/SPY/OPT/SPY_2013-04_20130328143755.txt"
histData <- read.table(dataPathHist,skip=3, header=F, sep="|")
OptData <- read.table(dataPathOpt,skip=3, header=F, sep="|")

#tt[k]=strptime(tab$autos_data.Date[k], "%Y-%m-%d", tz="")

sharePrice = OptData[1,6]
downloadDate =strptime( OptData[1,2], "%Y%m%d", tz="")

StrikeDate = strptime(OptData[1,4], "%y%m%d", tz="")

# get the range of price closer to strike price so band would be celling(shareprice) and floor(shareprice)
lowerRange = floor(sharePrice)
upperRange = ceiling(sharePrice)

daysToExpire = as.numeric((StrikeDate  - downloadDate)*5/7)
daysToExpire 

tt <- OptData[OptData$V7 >= lowerRange & OptData$V7 <= upperRange & OptData$V5 == "C",]
tt
strikePrice = tt[1,7]
tmp = tt[1,"V11"]
tmp
OptPrice = as.numeric(tt[1,11])
OptPrice


last20DaysHistPrice = histData[1:21,8]

lnArray <- c(1.1)

for ( i in 2:21)
{
	lnArray[(i-1)]=log(last20DaysHistPrice[i-1]/last20DaysHistPrice[(i)])
}

beta=sd(lnArray)*sqrt(252)
beta

#Read option file



BS(sharePrice,strikePrice,.012,(daysToExpire/252),beta, "C")
implied.vol(sharePrice,strikePrice,.012,(daysToExpire/252),OptPrice, "C")



