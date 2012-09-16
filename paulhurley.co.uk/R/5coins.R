# What Amounts can't you make using up to 5 coins 1p to 50p
# 
# Author: Paul Hurley
###############################################################################
library(ggplot2)
library(plyr)

#Define our coins
coins<-as.factor(c(0,1,2,5,10,20,50))

#build a list of all the possibilities
possibilities<-expand.grid(coin1=coins, coin2=coins, coin3=coins, coin4=coins, coin5=coins)

#calculate the result
possibilities$total<-as.numeric(as.character(possibilities$coin1))+
		as.numeric(as.character(possibilities$coin2))+
		as.numeric(as.character(possibilities$coin3))+
		as.numeric(as.character(possibilities$coin4))+
		as.numeric(as.character(possibilities$coin5))


targets<-1:250

#what amounts aren't possible
targets[!targets %in% possibilities$total]

tableofpossibilities<-ddply(.data=possibilities, .(total), nrow)

ggplot(data=possibilities, aes(x=total))+geom_histogram(binwidth=1)

#How about 4 coins
#build a list of all the possibilities
fourpossibilities<-expand.grid(coin1=coins, coin2=coins, 
		coin3=coins, coin4=coins)

#calculate the result
fourpossibilities$total<-as.numeric(as.character(fourpossibilities$coin1))+
		as.numeric(as.character(fourpossibilities$coin2))+
		as.numeric(as.character(fourpossibilities$coin3))+
		as.numeric(as.character(fourpossibilities$coin4))

#what values can't be made ?
targets[!targets %in% fourpossibilities$total]

tableofpossibilities<-ddply(.data=fourpossibilities, .(total), nrow)

ggplot(data=fourpossibilities, aes(x=total))+geom_histogram(binwidth=1)