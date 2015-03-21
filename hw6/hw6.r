# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  # Set up the output variable, define it as a matrix then use initial.doctors
m<-matrix(nrow=n.doctors,ncol=n.days)
  # to set the first column (day)
m[,1]<-initial.doctors
  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p
for (i in 2:n.days){
two_random_doctors<-sample(1:n.doctors,2,replace=FALSE)
if (m[two_random_doctors[1],i-1]==m[two_random_doctors[2],i-1]){
m[,i]<-m[,i-1]
} else if (m[two_random_doctors[1],i-1]==1 & m[two_random_doctors[2],i-1]==0){
    m[,i]<-m[,i-1]
    m[two_random_doctors[2],i]<-sample(c(1,0),1,prob=c(p,1-p),replace=FALSE)
} else {
    m[,i]<-m[,i-1]
    m[two_random_doctors[1],i]<-sample(c(1,0),1,prob=c(p,1-p),replace=FALSE) 
}
}
# return the output
return(m)
}





# When you test your function you have to generate <initial.doctors> and
# pick values for th other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
initial.doctors<-sample(c(1,0),n.doctors,prob=c(0.1,0.9),replace=T)
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
n.doctors<-100
n.days<-1000

p<-0.9
mat1<-sim.doctors(initial.doctors, n.doctors, n.days, p)
a<-c()
for (j in 1:n.days){
  a[j]=sum(mat1[,j])
}
plot(x=1:n.days,y=a,type="l",col="red",xlab="days",ylab="number of adopters",main="Graph of Adopters")

p<-0.8
mat2<-sim.doctors(initial.doctors, n.doctors, n.days, p)
b<-c()
for (j in 1:n.days){
  b[j]=sum(mat2[,j])
}
lines(x=1:n.days,y=b,type="l",col="blue")

p<-0.7
mat3<-sim.doctors(initial.doctors, n.doctors, n.days, p)
d<-c()
for (j in 1:n.days){
  d[j]=sum(mat3[,j])
}
lines(x=1:n.days,y=d,type="l",col="green")

p<-0.4
mat4<-sim.doctors(initial.doctors, n.doctors, n.days, p)
f<-c()
for (j in 1:n.days){
  f[j]=sum(mat4[,j])
}
lines(x=1:n.days,y=f,type="l",col="grey")

p<-0.2
mat5<-sim.doctors(initial.doctors, n.doctors, n.days, p)
h<-c()
for (j in 1:n.days){
  h[j]=sum(mat5[,j])
}
lines(x=1:n.days,y=h,type="l",col="violet")

legend("topleft",legend=c("p=0.9","p=0.8","p=0.7","p=0.4","p=0.2"),lty=c(1,1,1,1,1),col=c("red","blue","green","grey","violet"),cex=0.6)
