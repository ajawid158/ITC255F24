####Discrete distributions

#++++++++++++++++++++++++++++++++++++++++++#

#dbinomial ::: returns probs
#dbinomail(X=x, n, p)     Pr(X=x; n, p)   x<=n

dbinom(1, 2, 0.5)   #the chance of getting one head/1 in two trails

#Ex: 30% of households in a village depend entirely on agriculture.
#12 household is randomly chosen. What is the chance
#that x of these households depend on Agriculture.

p=0.3
n=12
x=c(0:12)
y=dbinom(x, n, p)
plot(x, y, 
     type = "b",
     col="red", 
     pch=19)

#++++++++++++++++++++++++++++++++++++++++++#

#Poisson Distribution

#Consider an online shop. On average the shop has 4 visits every hour. 
#what is the chance 0, 1, or 4 visit(s) in the next hour.

dpois(0, 4)
dpois(1,4)

#Same Porblem, estimate the chance of 0-12 visits next hour
x=c(0:12)
y=dpois(x, 4)
plot(x,y, 
     type = "b", 
     col="darkgreen",
     pch=19)


#++++++++++++++++++++++++++++++++++++++++++#
#Geometric Distributon

#The chance of stock trader to make a profit on certain day is 35%. 
#what is the chance the trade makes a profit on the first, second, and fifth day.
#Pr(X=k)=p*(1-p)^k
dgeom(0, 0.35)  #S
dgeom(1, 0.35)   #FS
dgeom(2, 0.35)   ##FFS

x=c(0:14)
y=dgeom(x, 0.35)
plot(x,y, 
     col="red", 
     type="b", 
     pch=19, 
     xlab="day", 
     ylab="prob")

#++++++++++++++++++++++++++++++++++++++++++#
##Uniform Distribution
#Chance of getting 8 
help("dunif")
dunif(8, 0,10)

x=c(0:10)
y=dunif(x, 0, 10)

plot(x,y, 
     pch=19, 
     type="b", 
     col="blue")



#+++++++++++++++++++++++++++++++++++++++#
#Normal Distribution

#X~N(10, 4)
#Generate X
x=rnorm(10000, 10, 4)
plot(density(x))

#Pr(X<12)= 0.69
pnorm(12, 10 , 4)

#Pr(X>11)= 1- Pr(x<11)=0.31
1-pnorm(12, 10, 4)

#Pr(8<X<12)=Pr(X<12)-Pr(X<8) = 0.38
pnorm(12, 10, 4)- pnorm(8, 10, 4)

#Simulation in R 

#Y and X 
#Y=b0+b1X+e    sLR Model of X and Y
#b0=0.5   b1= 2    X~N(0, 1)   e~N(0, 2)
#Simulate Y::Generate data on Y

b0=0.5
b1=2
x=rnorm(1000, 0, 1)    #generate data on X
e=rnorm(1000, 0, 2)   #generating random data on error term

#ObJECTIVE: Generate simulated data on Y() 




Y=b0+b1*x+e
head(Y)

summary(Y)

cor(x,Y)
plot(x, Y)

#Ex: 
#x: the income of the employees of a company 
#y: the spending of these employees

#y= b0+b1x+e    where b0=3,   b1=0.8    x~N(50, 4)    e~N(0, 3)
#spending= b0 + b1*Income + e     b1=1   b1>1   b1<1

#ObJECTIVE: Generate simulated data on Y(Spending of employees)

b0=3
b1=0.8
x=rnorm(1000,40, 6)
e=rnorm(1000, 0, 3)

y=b0+b1*x+e
head(y)
summary(y)

plot(density(y))

cor(x, y)
plot(x,y, 
     pch=3)

