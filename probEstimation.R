###Continuous Random Variable

t=read.csv("timeToOffice.csv")
View(t)

plot(ecdf(t$T))

#Pr(T<13)=14%
ecdf(t$T)(13)

#Pr(T>16)
1-ecdf(t$T)(16)

#Pr(13<T<16)
ecdf(t$T)(16)-ecdf(t$T)(13)
