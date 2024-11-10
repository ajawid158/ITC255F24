###Continuous Random Variable

t=read.csv("timeToOffice.csv")
View(t)

plot(ecdf(t$T))

#Pr(T<12)
ecdf(t$T)(13)
ecdf(t$T)(16)

#Pr(T>13)=1-Pr(T<13)
1-ecdf(t$T)(13)

#Pr(13<T<16)=Pr(T<16)-Pr(T<13)
ecdf(t$T)(16)-ecdf(t$T)(13)
