#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

bml.init <- function(r,c,p) {
  car <- rep(c(1,2), ceiling((r*c*p)/2))
  no_car <- rep(0, ceiling(r*c*(1-p)))
  system <- sample(c(car,no_car), r*c, replace= FALSE)
  m <- matrix(system,r,c)
  return(m)
}

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

bml.sim <- function(r,c,p) {
  m <- bml.init(r,c,p)
  i <- 1
  while (i <= 1000) {
    i <- i + 1
    new.m <- bml.step(m)
    m <- new.m[[1]]
    y <- new.m[[2]]
    if (y== FALSE) break}
  print(i-1)
}