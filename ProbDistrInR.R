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

