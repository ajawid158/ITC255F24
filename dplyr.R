#data manipulation with dplyr package
library(dplyr)
##a set of functions that perform certain actions

dfTips=read.csv('tips.csv')
View(dfTips)
names(dfTips)
dim(dfTips)


#filter rows
fcust=filter(dfTips, sex=='Female')
dim(fcust)
View(fcust)

#filter(dfTips, sex !='Female')
head(fcust)
dim(fcust)

fNonS=filter(dfTips, sex=="Female", smoker=='No')
head(fNonS)
dim(fNonS)

#logical operators &, |, ! AND OR NOT
unique(dfTips$day)

weekend=filter(dfTips, day=='Sun'| day=='Sat')  #day==Sat | Sun

head(weekend)
View(weekend)
dim(weekend)
nrow(dfTips)

filter(dfTips, day=='Sun' & day=='Sat')

#weekend and female
wkEndF=filter(dfTips, (day=='Sun'|day=='Sat') & sex=='Female')
head(wkEndF)
dim(wkEndF)

#Weekdays 

unique(dfTips$day)

wkDays=filter(dfTips, day!='Sun' &  day !='Sat')
head(wkDays)
View(wkDays)

#use the function %in%
unique(dfTips$size)

filter(dfTips, size %in% c(5,6))
filter(dfTips, day %in% c('Sun','Sat'))

#for numerical >, <, ==
filter(dfTips, size<4)
x1=filter(dfTips, tip>=6)
x1
#Arrange
names(dfTips)
View(dfTips)
head(dfTips, 10)

head(arrange(dfTips, -desc(tip)))
head(arrange(dfTips, desc(tip)))
head(arrange(dfTips, sex))   #decode the values F=1 Male 0

View(dfTips)
#select a subset based on column
names(dfTips)
x0=select(dfTips, c(total_bill, tip, size))
head(x0)
head(dfTips)

x1=select(dfTips, size, day, everything())  #var size is put at the beginning
head(x1)

head(dfTips)
dfTips=select(dfTips, total_bill:size)
head(dfTips)

head(dfTips)


head(select(dfTips, tip:smoker))
head(select(dfTips, -(tip:smoker)))

x2=select(dfTips, tip:smoker)
head(x2)
View(dfTips)


#rename
names(dfTips)

dfTips1=rename(dfTips, Bill=total_bill)
head(dfTips1)


#mutate
head(dfTips)
#total spending=total_bill+tip
x3=mutate(dfTips, total_cost=total_bill+tip)
View(x3)
x4=mutate(dfTips, tax=total_bill*0.2)
View(x4)
#summarize
summarise(dfTips, mean(total_bill), sd(total_bill),  mean(tip), sd(tip))

#based on another variable
gender=group_by(dfTips, sex)

summarise(gender, mean(total_bill), sd(total_bill))
smoker=group_by(dfTips, smoker)
summarise(smoker, mean(total_bill), sd(total_bill))
names(dfTips)

#pull a column as a vector 
smk = pull(dfTips, smoker)

head(smk)


#sample_n works with rows
dim(dfTips)
sampledfTips=sample_n(dfTips, 100)
head(sampledfTips)
View(sampledfTips)