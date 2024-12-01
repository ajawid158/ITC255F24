dt=read.csv('tips.csv')
head(dt)


##SLR Model
#total bill and tip
#Y=tip    X=tota_bill

#Y=a+b*X  population model

#when X does not have any effect on Y? b=0
#When X has direct effect on Y? b>0
#When X has indirect effect on Y? b<0
#When X has an effect on Y? b!=0

#H0: b=0
#H1: b!=0

#alpha=0.05 (Max of p-value)

cor(dt$total_bill, dt$tip)


#run the SLR model

lmodel=lm(dt$tip~dt$total_bill)
summary(lmodel)

#Sample model
#tip=0.92+0.11*total_bill 

#p-value for estimated b~0 < 0.05
#Decision: Reject H0 
#b>0 hence we have a strong evidence that in the pop total_bill has a direct effect on tip.
names(dt)
##time, total_bill, tip
#population model
#tip=a+b1*total_bill+b2*time

#H0:b2=0
#H1: b2 !=0
#alpha=0.05 (Max of p-value)

lmodel2=lm(dt$tip~dt$total_bill+dt$time)
summary(lmodel2)

#we do not reject H0. No significant effect of time on tip. 
#use your own dataset 
#run at least on Bivar inf method
#Run an SLR model for two qnts
#submit your work as a single pdf file


