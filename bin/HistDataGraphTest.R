#!/usr/bin/Rscript


source("c:/aftab/R/finanalysis/config/finanalysis.cfg")
source(paste(Home, "/data/parm/ImpVolChart.in", sep = ''))

source(paste(Home, "/lib/impliedVolLib.R", sep = ''))


dataPathHist= "C:/aftab/python/crawler/data/out/^VIX/HIST/^VIX_Hist.txt"
dataPathExpireDate = "C:/aftab/python/crawler/data/out/^VIX/OTH/Expire_Date.txt"

histData <- read.table(dataPathHist,skip=1, header=F, sep="|")
dataExpireDate  <- read.table(dataPathExpireDate ,skip=1, header=F, sep="|")

histData
dataExpireDate

#plot(as.Date( histData$V2[1:10], format="%Y%m%d"),histData$V8[1:10] )

plot(as.Date(histData$V2[1:86]),histData$V8[1:86] )

abline(v=as.Date( "20130304",format="%Y%m%d"))
abline(v=as.Date("20130505",format="%Y%m%d"))

abline(v=c(20130116,20130213,20130320,20130417,20130522))

