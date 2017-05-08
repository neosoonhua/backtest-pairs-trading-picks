rm(list=ls(all='true')) #Remove all objects

firstDate = "2017-04-02"
#install.packages(c("tseries", "zoo"))
library(tseries)
library(zoo)

#Columns
shortNameCol = 1
shortSymbolCol = 2
longNameCol = 3
longSymbolCol = 4
ratioCol = 5

par(ask=TRUE)

calls = read.csv(paste(firstDate, " dummy picks.csv", sep=""), header=TRUE)
head(calls)

count = 0 #To count the number of profitable calls
lastRow = dim(calls)[1]
for(row in 1:lastRow) {
	symbol1 = paste(calls[row, longSymbolCol], ".SI", sep="")
	symbol2 = paste(calls[row, shortSymbolCol], ".SI", sep="")

	name1 = calls[row, longNameCol]
	name2 = calls[row, shortNameCol]

	# Load security data from Yahoo! Finance 
	#"Adj" for adjusted prices
	price1 <- get.hist.quote(symbol1, quote="Open", start=firstDate, retclass="zoo") 
	#print(tail(price1))
	price1 <- na.locf(price1)               # Copy last non-NA price when NA   
	price2 <- get.hist.quote(symbol2, quote="Open", start=firstDate, retclass="zoo")   
	#print(tail(price2))
	price2 <- na.locf(price2)               # Copy last non-NA price when NA

	ratio = calls[row, ratioCol]
	wt = price1 - ratio*price2

	commission = (price1[1]+price2[1])*0.014 
	#Estimated commission for 4 trades. Needs to be finetuned.
	
	criticalValue = as.numeric(wt[1]+commission)
	if(max(wt) > criticalValue) count=count+1

	plot(wt, type='l', main=paste("Long 1", name1, "and short", ratio, name2, sep=" "))
	abline(criticalValue, 0, lty="dashed")
}

print(paste("Proportion of profitable calls =", round(count/lastRow, 2), sep=" "))