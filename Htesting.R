##Hypothesis testing about pop mean


dt=read.csv("tips.csv")

names(dt)
dim(dt)

#What is pop? what is the mean of the pop? 
#Var of interest is Toal_Bill
mean(dt$total_bill)

#Problem 1:

#H0: mu=25
#H1: mu !=25

t.test(x=dt$total_bill, mu=25, alternative = "two.sided")


#p-value=0.00<0.05 hence Reject H0 in favor of H1

# based on sample data you Reject the cliam that mu=25 und 5% p-value.
#p-valaue  of the test ~ 0

##Problem 2

#p-value=0.00<0.05 hence reject H0
#H0: mu<=25
#H1: mu > 25

t.test(x=dt$total_bill, mu=25, alternative = "greater")
#p-value=1>0.05 hence we do not reject H0

#Still we can H0, but if we do so how much our chance of making 
#type 1 ERROR= 100%


#Problem 3


#H0: mu=>25
#H1: mu <25
t.test(x=dt$total_bill, mu=25, alternative = "less")

# p-value = 0.000<0.05, hence REJECT H0 in favor in H1
# we approved the cliam that the pop mean is smaller than 25$

#mean of the AGE is not beyond 22.
#H0:mu>=22
#H1:mu<22


##Bivariate methods
dim(dt)
##CASE N0. 1: 2 QL vars
#Gender and time

#H0: No Association/ Independence
#H1: Association between Gender and Time

#Alpha is the max p-value (Pr(Type 1 Error)) that you accept 
#Decision: specify Alpha=5%

#Chi SQ test
chisq.test(table(dt$sex, dt$time))

#p-value=0.002  <   0.05 
#Decision: Reject H0 in favor of H1
#Hence: there is strong statistical evidence that the two variables are 
#associated IN the population. 
#Note: still there is a chance that you have Rejected a True H0::which 0.2%



##CASE No. 2: 1 QL 1 QNT vars
#1 Binary and 1 QNT :::
names(dt)
#Gender, tip

#H0: No Association/ Independence
#H1: Association between Gender and Time

#Decision: specify Alpha=5%

t.test(dt$tip~dt$sex)

#p-value = 0.1378 > 0.05
#Decision: Cannot Reject H0. 

#Note: If we had reject H0, the chance of making type error then was 14% that is why we dont do it.

#1 Non Binary and 1 QNT :::
names(dt)

#Day, tip

#H0: No Association/ Independence
#H1: Association between Gender and Time

#Decision: specify Alpha=5%

summary(aov(dt$tip~dt$day))

#p-value=0.174  > 0.05
#Decision: we cannot Reject H0. 



##CASE No. 3: 2 QNT vars
#Linear association:::correlation test
names(dt)

#total bill and tip

#H0: No LINEAR Association/ Independence
#H1: LIENAR Association between Total bill and tip

#Decision: specify Alpha=5%

cor.test(dt$total_bill, dt$tip)
cor(dt$total_bill, dt$tip)

# p-value < 2.2e-16~0 < 0.05
#We Reject H0 in favor of H1 




##SLR
#x total bill
#y tip

summary(lm(dt$tip~dt$total_bill))


#H0: b=0
#H1: b>0

#with p-value<0.05 we reject H0.