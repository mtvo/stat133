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

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  has.adopted <- matrix(rep(0,n.doctors*2),n.doctors,n.days)
    has.adopted[,1] <- initial.doctors
  for (j in 2:n.days){  
    for (i in 1:n.doctors) {
    if (has.adopted[i,j-1]==1) {has.adopted[i,j] <- 1}
    }
    random <- sample(1:n.doctors,2, replace = FALSE)
    if (has.adopted[random[1],j]== 1 & has.adopted[random[2],j]==0) {
      has.adopted[random[2],j] <- sample(c(rep(1,p*100),rep(0,(1-p)*100)),1)}
    if (has.adopted[random[2],j]== 1 & has.adopted[random[1],j]==0) {
      has.adopted[random[1],j] <- sample(c(rep(1,p*100),rep(0,(1-p)*100)),1)}
    }
  return(has.adopted)
  }
 
  # return the output

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
initial.doctors <- c(rep(0,18),rep(1,2))
matrix1 <- sim.doctors(initial.doctors,length(initial.doctors),20,0.2)
plot <- plot(1:20,colSums(matrix1), type= "l", xlim= c(0,20), ylim= c(0,15), xlab= "Day", ylab= "# of doctors adopting drug", main= "Doctors Adopting Drug Over Time");plot
matrix2 <- sim.doctors(initial.doctors,length(initial.doctors),20,0.4)
lines(1:20,colSums(matrix2), type= "l")
matrix3 <- sim.doctors(initial.doctors,length(initial.doctors),20,0.6)
lines(1:20,colSums(matrix3), type= "l")
matrix4 <- sim.doctors(initial.doctors,length(initial.doctors),20,0.8)
lines(1:20,colSums(matrix4), type= "l")
matrix5 <- sim.doctors(initial.doctors,length(initial.doctors),20,1)
lines(1:20,colSums(matrix5), type= "l")
