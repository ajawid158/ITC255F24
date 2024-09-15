#Fall 25/ITC 255
#Intro to R Programming

#Installation, R, R Studio, Github
#Data types and variables in R
x=12
class(x)
y="Name"
class(y)

#Data structures in R

dt=data.frame(gen=c("M", "f"),
              age=c(22, 24),
              height=c(170, 171))
dt

#uploading datasets to R
dt1=read.csv("tips.csv")
View(dt1)
dim(dt1)
names(dt1)
head(dt1)
dt1[,2]   # the second column
dt1[2,]    # the second row
dt1[2,3]   #second row third column


#Control Structures in R
#Sequencing
#Selection
#Loop
x=4


if(x>0){
    print("P")
  } else if (x==0) {
    print("Z")
  } else {
    print("N")
  }

#LOOP

a=c(3,2,6,8)

b=c()

for(i in 1:4){
  b[i]=a[i]+1
}                   

b

###Writing your OWN functions in R

sm=function(x){
  s=0
  for (i in 1:length(x)){
    s=s+x[i]
  }
  return(s)
}

a=c(1,2,3)
sm(a)


mn=function(x){
  s=0
  for (i in 1:length(x)) {
    s=s+x[i]
  }
  n=length(x)
  return(cat("the Mean is:", s/n))
}
mn(a)



