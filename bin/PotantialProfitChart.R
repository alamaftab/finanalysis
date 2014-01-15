#Purpose: PotantialProfitChart R Will be used to sell other end of covered call or put to take some profit. 
#This will explore possible profits.


#Direction to Run: You need next month or current month option price. Change line 30 
#SPY_2014-02.txt to SPY_2014-01.txt if if options are expiring current month. 
# I have given .05 buyback back by default. You may want to change it if needed.

## Input:  
stockSym = "MSFT"
ownedOptStrPr = 36
totOptOwned = 1000

#Parameter

data.entry(stockSym,ownedOptStrPr,totOptOwned ) 
#ata.entry()

coveredSellCnt1 = totOptOwned*.5
coveredSellCnt2 = totOptOwned*1
coveredSellCnt3 = totOptOwned
coveredSellPrc1 = ownedOptStrPr - 1
coveredSellPrc2 = ownedOptStrPr - 2
coveredSellPrc3 = ownedOptStrPr - 3
data.entry(coveredSellCnt1,coveredSellCnt2, coveredSellCnt3 ,coveredSellPrc1,coveredSellPrc2,coveredSellPrc3)



source("c:/aftab/R/finanalysis/config/finanalysis.cfg")
source(paste(Home, "/data/parm/ImpVolChartCalSpread.in", sep = ''))
source(paste(Home, "/lib/impliedVolLib.R", sep = ''))
DefaultFilePath ="C:/aftab/python/crawler/data/out/SPY/OPT/SPY_2014-01.txt"
dataPathOpt1=gsub("SPY",stockSym,DefaultFilePath)
OptData1 <- read.table(dataPathOpt1,skip=1, header=F, sep="|")


sharePrice = OptData1[1,6]

buyBackcoveredPutorCallPr = ( ownedOptStrPr - sharePrice )*totOptOwned
#Normally buyback is never zero so we can default it to .05.
if ( buyBackcoveredPutorCallPr < 0 ) 
{
  buyBackcoveredPutorCallPr = totOptOwned*.05
}

downloadDate =strptime( OptData1[1,2], "%Y%m%d", tz="")
StrikeDate1 = strptime(OptData1[1,4], "%y%m%d", tz="")

# get the range of price closer to strike price so band would be celling(shareprice) and floor(shareprice)

lowerRange = floor(sharePrice -6)
upperRange = ceiling(sharePrice + 2)

tt1 <- OptData1[OptData1$V7 >= lowerRange & OptData1$V7 <= upperRange & OptData1$V5 == "P" &
                  OptData1$V14 > 3 & (grepl("140222",OptData1[,8])),]

prFrm <- cbind(tt1[,7],tt1[,12],0)



coveredSellCost1 = (prFrm[prFrm[,1] == coveredSellPrc1,2])*coveredSellCnt1
coveredSellCost2 = (prFrm[prFrm[,1] == coveredSellPrc2,2])*coveredSellCnt2
coveredSellCost3 = (prFrm[prFrm[,1] == coveredSellPrc3,2])*coveredSellCnt3



tmpFrm <- data.frame(endPr =0, pr1=0,pr2 =0 , pr3 =0)

# here prFrm[,1] is final closing price for stock

for (i  in 1:nrow(prFrm))
{
  if ( prFrm[i,1] >= ownedOptStrPr ) 
  {
    tmpPrc1 =  coveredSellCost1 
    tmpPrc2 =  coveredSellCost2 
    tmpPrc3 =  coveredSellCost3 
  }
  else if ( prFrm[i,1] < ownedOptStrPr & prFrm[i,1] >= coveredSellPrc1 ) 
  {
    tmpPrc1 =  coveredSellCost1 + (totOptOwned - coveredSellCnt1)*(ownedOptStrPr - prFrm[i,1] ) + 
                    coveredSellCnt1*(ownedOptStrPr - prFrm[i,1])
    tmpPrc2 =  coveredSellCost2 + (totOptOwned - coveredSellCnt2)*(ownedOptStrPr - prFrm[i,1] ) + 
                    coveredSellCnt2*(ownedOptStrPr - prFrm[i,1])
    tmpPrc3 =  coveredSellCost3 + (totOptOwned - coveredSellCnt3)*(ownedOptStrPr - prFrm[i,1] ) + 
                    coveredSellCnt3*(ownedOptStrPr - prFrm[i,1])
  }
  else if ( prFrm[i,1] < coveredSellPrc1 & prFrm[i,1] >= coveredSellPrc2 ) 
  {
    tmpPrc1 =  coveredSellCost1 + (totOptOwned - coveredSellCnt1)*(ownedOptStrPr - prFrm[i,1] ) + 
      coveredSellCnt1*(ownedOptStrPr - coveredSellPrc1)
    tmpPrc2 =  coveredSellCost2 + (totOptOwned - coveredSellCnt2)*(ownedOptStrPr - prFrm[i,1] ) + 
      coveredSellCnt2*(ownedOptStrPr - prFrm[i,1])
    tmpPrc3 =  coveredSellCost3 + (totOptOwned - coveredSellCnt3)*(ownedOptStrPr - prFrm[i,1] ) + 
      coveredSellCnt3*(ownedOptStrPr - prFrm[i,1])
  }
  if ( prFrm[i,1] < coveredSellPrc2 & prFrm[i,1] >= coveredSellPrc3 ) 
  {
    tmpPrc1 =  coveredSellCost1 + (totOptOwned - coveredSellCnt1)*(ownedOptStrPr - prFrm[i,1] ) + 
      coveredSellCnt1*(ownedOptStrPr - coveredSellPrc1)
    tmpPrc2 =  coveredSellCost2 + (totOptOwned - coveredSellCnt2)*(ownedOptStrPr - prFrm[i,1] ) + 
      coveredSellCnt2*(ownedOptStrPr - coveredSellPrc2)
    tmpPrc3 =  coveredSellCost3 + (totOptOwned - coveredSellCnt3)*(ownedOptStrPr - prFrm[i,1] ) + 
      coveredSellCnt3*(ownedOptStrPr - prFrm[i,1])
  }

  else if (prFrm[i,1] < coveredSellPrc3 )
  {
    tmpPrc1 =  coveredSellCost1 + (totOptOwned - coveredSellCnt1)*(ownedOptStrPr - prFrm[i,1] ) + 
      coveredSellCnt1*(ownedOptStrPr - coveredSellPrc1)
    tmpPrc2 =  coveredSellCost2 + (totOptOwned - coveredSellCnt2)*(ownedOptStrPr - prFrm[i,1] ) + 
      coveredSellCnt2*(ownedOptStrPr - coveredSellPrc2)
    tmpPrc3 =  coveredSellCost3 + (totOptOwned - coveredSellCnt3)*(ownedOptStrPr - prFrm[i,1] ) + 
      coveredSellCnt3*(ownedOptStrPr - coveredSellPrc3)
  }
  tmpFrm <- rbind(tmpFrm,c(prFrm[i,1],tmpPrc1 - buyBackcoveredPutorCallPr,
                           tmpPrc2 - buyBackcoveredPutorCallPr ,tmpPrc3 - buyBackcoveredPutorCallPr ))
}

attach(tmpFrm)

#plot(pr1,endPr, ylim= range(31,38), col="green", lty=1,lwd = 2)
plot(endPr,pr1 , xlim= range(30,39),col="green", type = "l",lty=1,lwd = 2)
points(endPr,pr2, col="red", type = "l",lty=1,lwd = 2)
points(endPr, pr3, col="black",type = "l", lty=1,lwd = 2)
abline(h=500)
abline(h=1000)

detach(tmpFrm)
#implied.vol(38.15, 36,.02,(55/252),1.05, "P")
#BS(36,34,.02,30/252,.957, "P")
