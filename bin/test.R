source("C:/aftab/R/finanalysis/bin/test1.R")
dp
dataPath="C:/aftab/R/finanalysis/data/in/SHLD/SHLD_Hist.txt"
histData <- read.table(dataPath,skip=3, header=F, sep="|")
#sd(histData$V7)
#tt=summary(histData$V8)
#tt[1]
#sd(histData[nrow(histData)-20:nrow(histData),]$V8)
#nrow(histData)

tt = histData[((nrow(histData)-20):nrow(histData)),]
tt
last20DaysHistPrice = histData[1:21,8]

lnArray <- c(1.1)

for ( i in 2:21)
{
	lnArray[(i-1)]=log(last20DaysHistPrice[i-1]/last20DaysHistPrice[(i)])
}

sd(lnArray)*sqrt(252)

# Black-Scholes Option Value
# Call value is returned in values[1], put in values[2]
#input:Price, exercise price, risk-free rate, time to maturity volatility ( yearly: calculate for 30 days and extrapulate)
blackscholes <- function(S, X, rf, T, sigma) {
    values <- c(2)

    d1 <- (log(S/X)+(rf+sigma^2/2)*T)/sigma*sqrt(T)
    d2 <- d1 - sigma * sqrt(T)

    values[1] <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
    values[2] <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)

    values
}

blackscholes(42.34,55,.012,29/252,.95)

