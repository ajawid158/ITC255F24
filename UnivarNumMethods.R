#Fall 25/ITC 255
#Descriptive methods
#Univar case 
#Numerical methods
#Center of distribution (mean, median, mode)

dfTips=read.csv(url('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv'))
head(dfTips)


#check the distribution
plot(density(dfTips$tip))

#Locate the center....why is it important to locate the center
#different approaches
mean(dfTips$tip)  #
median(dfTips$tip)   #as a midpoint

mymode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mymode(dfTips$tip)


#Other locations (quantiles)
quantile(dfTips$tip)    #quartiles
quantile(dfTips$tip, 0.4)
#write a fun that returns any location in the dist

myQnt=function(x,q){
  pr=quantile(x, q)
  return(pr)
}

myQnt(dfTips$tip, 0.80)

# 80% paid 4 or less as tip, 20% paid more than 4 USD


boxplot(dfTips$tip,
        horizontal = T, 
        col='#0033FF')

#outliers affect the location of the center dispropotionaly 
boxplot.stats(dfTips$tip)

outs=boxplot.stats(dfTips$tip)[4] #outliers
typeof(outs)

outs=data.frame(outs)
head(outs)
min(outs$out)

View(dfTips)
#remove the outliers

tipNew=dfTips$tip[dfTips$tip<6]


boxplot(tipNew, horizontal = T)
mean(tipNew)
median(tipNew)
mymode(tipNew)


plot(density(tipNew))
#Variation
range(dfTips$tip)
sd(dfTips$tip)
var(dfTips$tip)   #center means the mean
mad(dfTips$tip)

plot(density(dfTips$tip))
##ECDF Emperical Cummulative Distribution Function

plot(ecdf(dfTips$tip), 
     col='blue', 
     main='ECDF of Tip', 
     xlab='tip')
abline(v=3.9, col='red', lty=3)
abline(h=0.8, col='darkgreen', lty=3)


ecdf(dfTips$tip)(4)
#83% paid 4 d or less as tip

#quantile and ecdf are inverse of one another
quantile(dfTips$tip, 0.8) #we have the percentage...look for the value

ecdf(dfTips$tip)(6)    #we have the value ...look for the percentage


#Next:Data Manipulation dplyr package