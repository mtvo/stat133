#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.


#BML functions from bml_functions.R
#1: Initialization function
bml.init <- function(r,c,p) {
  car <- rep(c(1,2), ceiling((r*c*p)/2))
  no_car <- rep(0, ceiling(r*c*(1-p)))
  system <- sample(c(car,no_car), r*c, replace= FALSE)
  m <- matrix(system,r,c)
  return(m)
}

#2: Function to move traffic one time-step
blue_move <- function(m) {
  blue.in <- m
  for (i in 1:nrow(m)) {
    if ((m[i,(ncol(m)-1)]==2) & (m[i,ncol(m)] != 1)) {
      blue.in[i,ncol(m)] <-  2
      blue.in[i,(ncol(m)-1)] <- 0
    }
  }
  
  blue.in2 <- blue.in
  for (i in 1:nrow(blue.in2)) {
    for (j in (ncol(blue.in2)-2):1) {
      if ((blue.in2[i,j]== 2) & (blue.in2[i,j+1] == 0)) {
        blue.in2[i,j+1] <- 2 
        blue.in2[i,j] <- 0
      } 
    }          
  }
  blue.edge <- blue.in2
  blue.edge2 <- blue.in2
  for (i in 1:nrow(blue.in2)) {
    if ((blue.in2[i,ncol(blue.in2)]== 2 ) & (m[i, (ncol(m)-1)]== 2) & (m[i, (ncol(m))]== 2) & (blue.in2[i,1]== 0)) {
      blue.edge[i,1] <-2
      blue.edge2[i,1] <- 2
    } 
    else if ((blue.in2[i,ncol(blue.in2)]== 2 ) & (m[i, ncol(m)]== 2) & (m[i, (ncol(m)-1)] != 2) & (blue.in2[i,1]== 0)) {
      blue.edge[i,1] <-2
      blue.edge[i,ncol(blue.edge)] <- 0
      blue.edge2[i,1] <- 2
      blue.edge2[i,ncol(blue.edge)] <- 0
    } 
    else if ((blue.in2[i,ncol(blue.in2)]== 2) & (m[i, ncol(m)-1]== 2) & (m[i, ncol(m)]== 2) & (blue.in2[i,1] != 0) & (blue.in2[i,ncol(blue.in2)-1]==0)) {
      blue.edge2[i,ncol(blue.in2)-1] <- 2
    } else if ((blue.in2[i,ncol(blue.in2)]== 2) & (m[i, ncol(m)-1]== 2) & (m[i, ncol(m)]== 2) & (blue.in2[i,1] != 0) & (blue.in2[i,ncol(blue.in2)-1] !=0)) {
      for (j in ncol(blue.in2):2) {
        if (blue.in2[i,j]==0) break}
      blue.edge2[i,j] <- 2
      
    }
  }
  return(blue.edge2)
}

red_move <- function(m) {
  red.in <- m
  for (j in 1:ncol(m)) {
    if ((m[(nrow(m)-1),j]==1) & (m[nrow(m),j] != 2)) {
      red.in[nrow(m),j] <-  1
      red.in[(nrow(m)-1),j] <- 0
    }
  }
  
  red.in2 <- red.in
  for (j in 1:ncol(red.in2)) {
    for (i in (nrow(red.in2)-2):1) {
      if ((red.in2[i,j]== 1) & (red.in2[i+1,j] == 0)) {
        red.in2[i+1,j] <- 1 
        red.in2[i,j] <- 0
      } 
    }          
  }
  red.edge <- red.in2
  red.edge2 <- red.in2
  for (j in 1:ncol(red.in2)) {
    if ((red.in2[nrow(red.in2),j]== 1 ) & (m[(nrow(m)-1),j]== 1) & (m[(nrow(m)),j]== 1) & red.in2[1,j]== 0) {
      red.edge[1,j] <-1
      red.edge2[1,j] <- 1
    } 
    else if ((red.in2[nrow(red.in2),j]== 1 ) & (m[nrow(m),j]== 1) & (m[(nrow(m)-1),j] != 1) & red.in2[1,j]== 0) {
      red.edge[1,j] <-1
      red.edge[nrow(red.edge),j] <- 0
      red.edge2[1,j] <- 1
      red.edge2[nrow(red.edge),j] <- 0
    } 
    else if ((red.in2[nrow(red.in2),j]== 1) & (m[nrow(m)-1,j]== 1) & (m[nrow(m),j]== 1) & (red.in2[1,j] != 0) & (red.in2[nrow(red.in2)-1,j]==0)) {
      red.edge2[nrow(red.in2)-1,j] <- 1
    } else if ((red.in2[nrow(red.in2),j]== 1) & (m[nrow(m)-1,j]== 1) & (m[nrow(m),j]== 1) & (red.in2[1,j] != 0) & (red.in2[nrow(red.in2)-1,j] !=0)) {
      for (i in nrow(red.in2):2) {
        if (red.in2[i,j]==0) break}
      red.edge2[i,j] <- 1
    }
  }
  return(red.edge2)
}

bml.step <- function(m){
  orig.m <-m
  grid.new <- logical()
  m <- blue_move(m)
  m <- red_move(m)
  grid.new <- sum(orig.m != m) > 0
  return(list(m,grid.new))
}

#3: Bml simulation 
bml.sim <- function(r,c,p) {
  m <- bml.init(r,c,p)
  i <- 1
  while (i <= 20000) {
    i <- i + 1
    new.m <- bml.step(m)
    m <- new.m[[1]]
    y <- new.m[[2]]
    if (y== FALSE) break}
  print(i-1)
}

#ADDITIONAL FUNCTIONS

#traffic_n: captures image of traffic system at n-th timestep for the original matrix m
traffic_n <- function(m,n){
  for (i in 1:n) {
  m <- bml.step(m)[[1]]
  }
  return(m)
}

#bml.sim.mean: output is a list of two elements
#the first element gives the average time step to reach gridlock for 20 simulations with initial conditions r,c, and p
#the second element gives the number of simulations that resulted in free-flowing traffic 

bml.sim.mean <- function(r,c,p){
  avg <- vector()
  for (i in 1:20) {
    m <- bml.init(r,c,p)
    avg[i] <- bml.sim2(m)
  }
  avg[avg== 1000] <- NA
  mean <- mean(avg, na.rm= TRUE)
  miss <- sum(is.na(avg))
  return(list(mean,miss))
}

##p= 0.20, simulations at all sizes and shapes 
bml.sim.mean(10,10,0.20)
bml.sim.mean(20,5,0.20)
bml.sim.mean(5,20,0.20)
bml.sim.mean(50,50,0.20)
bml.sim.mean(100,25,0.20)
bml.sim.mean(25,100,0.20)
bml.sim.mean(100,100,0.20)
bml.sim.mean(200,50,0.20)
bml.sim.mean(50,200,0.20)

#image of free-flowing traffic at end of 1000 time steps
image1 <- image(traffic_n(bml.init(100,100,0.2),1000), col= c("white", "red", "blue"); image

##p= 0.35, simulations at all sizes and shapes 
bml.sim.mean(10,10,0.35)
bml.sim.mean(20,5,0.35)
bml.sim.mean(5,20,0.35)
bml.sim.mean(50,50,0.35)
bml.sim.mean(100,25,0.35)
bml.sim.mean(25,100,0.35)
bml.sim.mean(100,100,0.35)
bml.sim.mean(200,50,0.35)
bml.sim.mean(50,200,0.35)

#image of grid-locked traffic at 300 time steps
image2 <- image(traffic_n(bml.init(100,100,0.35),300), col= c("white", "red", "blue"); image2

##p= 0.50, simulations at all sizes and shapes 
bml.sim.mean(10,10,0.5)
bml.sim.mean(20,5,0.5)
bml.sim.mean(5,20,0.5)
bml.sim.mean(50,50,0.5)
bml.sim.mean(100,25,0.5)
bml.sim.mean(25,100,0.5)
bml.sim.mean(100,100,0.5)
bml.sim.mean(200,50,0.5)
bml.sim.mean(50,200,0.5)

#image of grid-locked traffic at 100 time steps
image3 <- image(traffic_n(bml.init(100,100,0.5),100), col= c("white", "red", "blue"); image3

##p= 0.75, simulations at all sizes and shapes 
bml.sim.mean(10,10,0.75)
bml.sim.mean(20,5,0.75)
bml.sim.mean(5,20,0.75)
bml.sim.mean(50,50,0.75)
bml.sim.mean(100,25,0.75)
bml.sim.mean(25,100,0.75)
bml.sim.mean(100,100,0.75)
bml.sim.mean(200,50,0.75)
bml.sim.mean(50,200,0.75)

#image of grid-locked traffic at 30 time steps
image4 <- image(traffic_n(bml.init(100,100,0.75),30), col= c("white", "red", "blue"); image4