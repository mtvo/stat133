#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r,c,p) {
  car <- rep(c(1,2), ceiling((r*c*p)/2))
  no_car <- rep(0, ceiling(r*c*(1-p)))
  system <- sample(c(car,no_car), r*c, replace= FALSE)
  m <- matrix(system,r,c)
  return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

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

#return(list(m, grid.new))

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

#function determines whether the system reaches gridlock within 1000 timesteps
#output prints out the timestep it hits gridlock
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

##bml.sim2: variation of first function, whose input is the matrix, m, instead of the dimensions (r,c,p)
bml.sim2 <- function(m) {
  i <- 1
  while (i <= 20000) {
    i <- i + 1
    new.m <- bml.step(m)
    m <- new.m[[1]]
    y <- new.m[[2]]
    if (y== FALSE) break}
  print(i-1)
}
