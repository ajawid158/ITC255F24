
x=read.csv("grades1.csv")    
View(x)

#x is our pop data

#draw a random sample

rs=sample(nrow(x), 50)

s1=x[rs, ]
View(s1)

#Suppose we work with MT score

plot(density(x$MT), col="red",
     ylim=c(0, 0.08))
lines(density(s1$MT), col="blue")

#PE
mean(s1$MT)
mean(x$MT)

median(s1$Q)
median(x$Q)


#CI for mean

CI=function(xbar, s, n, k){
  LB=xbar-k*(s/sqrt(n))
  UB=xbar+k*(s/sqrt(n))
  CI=c(LB, UB)
  return(CI)
}

xbar=mean(s1$MT)
s=sd(s1$MT)
n=50

##90%
CI(xbar, s, n, 2.576)
