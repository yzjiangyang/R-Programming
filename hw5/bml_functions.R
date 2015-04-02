#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r,c,p){
  matrix(sample(c(0,1,2), r*c, prob = c(1-p, p/2, p/2), replace = T), nrow = r,ncol=c)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

red_move_east <- function(m) {
  if(ncol(m)==1) {
    return(m)
  }
  blocked_cars <- m[, c(2:ncol(m), 1)]!=0
  red_cars <- m*(m==1)
  red_cars*blocked_cars + m*(m!=1) + (red_cars*!blocked_cars)[,c(ncol(m), 1:(ncol(m)-1))] 
}

blue_move_north <- function(m) {
  if(nrow(m)==1) {
    return(m)
  }
  blocked_cars <- m[c(nrow(m), 1:(nrow(m)-1)),]!=0
  blue_cars <- m*(m==2)
  blue_cars*blocked_cars + m*(m!=2) + (blue_cars*!blocked_cars)[c(2:nrow(m), 1), ] 
}

bml.step <- function(m){
  grid.new <- blue_move_north(red_move_east(m))
  return(list(grid.new, !all(m==grid.new)))
}


 
#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)
bml.sim<-function(r,c,p){
  m<-bml.init(r,c,p)
  i<-1
  steps<-10000
  grid.new<-bml.step(m)
  while(i<steps){
    grid.new<-bml.step(grid.new[[1]])
    if(grid.new[[2]]==F){break}
    i<-i+1
  }
  return(list(grid.new[[1]],grid.new[[2]],i))
}





