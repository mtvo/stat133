#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

Use your BML experiment to conduct a simulation study of the behavior of the BML model.
Below are some questions to answering. Put the code you use to run the simulation study
in the file bml simulation.R.

#Question1: For what values of p, the density of the grid, did you find free flowing traffic 
#and traffic jams? Did you find any cases of a mixture of jams and free fowing traffic?

#get image of traffic system at n-th timestep for the original matrix,m
traffic_n <- function(m,n){
  for (i in 1:n) {
  m <- bml.step(m)[[1]]
  }
  return(m)
}

image(m, col= c("white", "red", "blue"))

#Question2: How many simulation steps did you need to run before observing this behavior?

#Question3: Does the transition depend on the size or shape of the grid?
bml.sim(200,200,0.30)
#[1] 598
 
bml.sim(100,100,0.30)
#[1] 349

bml.sim(10,10,0.30)
#[1] 1000


To answer these questions, run the experiment for different values of the input parameters.
For each set of input parameters that you examine, run the experiment multiple times to
understand the variability in the process. For example, does a p = 0:8 always end in a
traffic jam or does it only end in a traffic jam 70% of the time? Answer these questions
in the text file bml comments.txt or upload a report named bml comments.pdf. You
are encouraged to include 1-4 plots to support your observations, either as a separate file
(bml figures.pdf) or embedded in the pdf file with your comments.