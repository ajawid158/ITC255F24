#ITC 255/Statitical DATA Anlaysis
###++BivarMethods

dtTips=read.csv('tips.csv')
head(dtTips)
dim(dtTips)
#CASE of 2 QL Variables
  #Joint FDT
    #Gender and smoker associated?

jtSG=table(dtTips$sex, dtTips$smoker)
jtSG

barplot(jtSG, beside = T, 
        col=rainbow(2))

chisq.test(jtSG)

##Weak/No Association 
jtSG
table(dtTips$sex)

tf=jtSomkGender[1,]/87
tm=jtSomkGender[2,]/157


tb=rbind(tf, tm)
tb

barplot(tb, 
        beside = T, 
        col=rainbow(2))
abline(h=0)


#p-value = 1 meaning that there is NO Association

#Note:if the p-value<0.05 we have very Strong association
# if p-value<0.1 we have a moderate association 
#if p-value>0.1 there is not association
#who give more tip, male or female

#1 Binary (QL) and 1 QNT
#whether Gender has to do anything with the amount of tip?
library(dplyr)
names(dtTips)

#Gender vs Tip

genGroup=group_by(dtTips, sex)
summarise(genGroup, mean(tip), sd(tip), min(tip), max(tip))

ftip=dfTips$tip[dfTips$sex=="Female"]
mtip=dfTips$tip[dfTips$sex=="Male"]

#Joint Density

plot(density(ftip), 
     xlim=c(0,12), 
     col="red")

lines(density(mtip),
      col="blue")


t.test(dtTips$tip~dtTips$sex)
#p-value = 0.1378>0.1 no association  

#ql is not binary and a QNT 
#
#which day the customers pay more tips

dayGroup=group_by(dtTips, day)
summarise(dayGroup, mean(tip), sd(tip), min(tip), max(tip))

summary(aov(dtTips$tip~dtTips$day))
#pvalue=0.174=17.4%>0.1 no significant association

#2 QNT vars
#how tip changes as total_bill changes 

scatter.smooth(dtTips$total_bill, dtTips$tip)
abline(v=25, col='red')

cor(dtTips$total_bill, dtTips$tip)
cov(dtTips$total_bill, dtTips$tip)

##New dataset
dte=read.csv("employee.csv")
head(dte)

#Biuld a model of spending and salary 
scatter.smooth(dte$Salary, dte$Spending)
cor(dte$Salary, dte$Spending)

LM=lm(Spending~Salary, data=dte)
summary(LM)
