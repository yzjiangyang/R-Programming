#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.


#### The following codes are just from the file "bml_functions.R"
bml.init <- function(r,c,p){
  matrix(sample(c(0,1,2), r*c, prob = c(1-p, p/2, p/2), replace = T), nrow = r,ncol=c)
}

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

#### I create a simulation function to see how many steps does it takes to hit gridlock.
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

#### Let's try some examples to see how many steps will the traffic jams occur.

bml.sim(15,15,0.1) # After running many times, I observe that it still shows free flowing traffic.
# bml.sim(15,15,0.1)
#[[1]]
#     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15]
#[1,]    0    0    0    0    2    0    0    0    0     0     0     0     2     0     0
#[2,]    0    0    0    0    0    0    0    1    0     0     2     0     0     0     1
#[3,]    0    0    0    2    0    0    0    0    0     0     0     0     0     0     0
#[4,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0
#[5,]    0    0    0    0    0    0    1    0    2     1     0     1     0     0     0
#[6,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0
#[7,]    0    1    0    2    0    0    0    0    0     0     0     0     0     0     0
#[8,]    0    0    0    0    2    1    0    2    0     1     0     0     0     0     0
#[9,]    0    0    0    0    0    0    0    0    0     0     0     0     0     1     0
#[10,]    1    0    0    2    0    0    0    0    0     0     0     0     0     0     0
#[11,]    0    0    0    0    0    0    0    0    0     0     2     0     0     0     0
#[12,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0
#[13,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0
#[14,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0
#[15,]    0    0    0    0    0    1    0    0    0     0     0     0     0     0     0

#[[2]]
#[1] TRUE

#[[3]]
#[1] 10000

bml.sim(15,15,0.4) # After running many times, I observe that a few traffic jams occur, but the free flowing traffic system is more likely.
# bml.sim(15,15,0.4) ## This takes 4225 steps to hit gridlock.
#[[1]]               ## But the possibility of gridlock is lower.
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15]
#[1,]    0    0    0    0    0    0    1    1    1     1     2     2     2     0     0
#[2,]    0    0    0    0    0    0    1    1    2     2     2     2     0     0     0
#[3,]    0    0    0    0    1    1    1    2    2     2     2     2     0     0     0
#[4,]    0    0    1    1    1    1    2    2    2     2     0     2     0     0     0
#[5,]    1    1    1    1    1    2    2    2    2     0     0     2     0     0     0
#[6,]    0    0    1    1    2    2    0    2    2     0     0     2     0     0     0
#[7,]    1    1    1    2    2    2    0    0    0     0     0     1     1     1     1
#[8,]    2    2    0    2    2    0    0    0    0     0     0     0     0     0     1
#[9,]    1    2    1    2    0    0    0    0    0     0     0     0     1     1     1
#[10,]    2    2    0    0    0    0    0    0    0     0     0     0     1     1     1
#[11,]    2    2    0    0    0    0    0    0    0     0     1     1     1     1     2
#[12,]    2    0    0    0    0    0    0    0    0     1     1     1     1     2     2
#[13,]    2    0    0    0    0    0    0    0    0     0     0     1     2     2     2
#[14,]    0    0    0    0    0    0    0    1    1     1     1     1     2     0     2
#[15,]    0    0    0    0    0    0    0    0    0     1     1     2     2     0     0

#[[2]]
#[1] FALSE

#[[3]]
#[1] 4255

bml.sim(15,15,0.6) # After running many times, I observe that the possibility of traffic jams is much higher.
#bml.sim(15,15,0.6) # This takes only 193 steps to hit gridlock.
#[[1]]
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15]
#[1,]    2    2    2    0    0    0    0    0    0     0     0     1     1     1     2
#[2,]    0    2    2    0    0    0    0    0    0     0     1     1     1     2     2
#[3,]    0    0    2    0    0    0    0    0    0     1     1     1     2     2     2
#[4,]    0    0    0    0    0    0    0    0    1     1     1     2     2     2     2
#[5,]    0    0    0    0    0    0    1    1    1     1     2     2     2     0     2
#[6,]    0    0    0    0    0    1    1    1    1     2     2     2     2     0     0
#[7,]    0    0    0    1    1    1    1    1    2     2     2     2     2     0     0
#[8,]    0    0    0    0    2    2    2    2    2     2     2     2     0     0     0
#[9,]    0    1    1    1    1    1    2    2    0     2     0     2     0     0     0
#[10,]    0    0    2    2    2    2    2    2    0     2     0     2     0     0     0
#[11,]    1    1    1    1    2    1    2    2    0     2     0     0     0     0     0
#[12,]    1    1    1    2    2    1    2    0    0     0     0     0     0     1     1
#[13,]    1    1    2    2    2    1    2    0    0     0     0     0     0     0     0
#[14,]    1    2    2    0    2    1    2    0    0     0     0     0     0     0     1
#[15,]    2    2    2    0    0    1    2    0    0     0     0     0     1     1     1

#[[2]]
#[1] FALSE

#[[3]]
#[1] 193


bml.sim(5,5,0.6) # After running many times, I observe that the possibility of free flowing traffic is a bit higher than the possibiity of traffic jams.
#bml.sim(5,5,0.6) # This is free flowing traffic.
#[[1]]
#      [,1] [,2] [,3] [,4] [,5]
#[1,]    2    0    1    2    0
#[2,]    1    0    0    0    2
#[3,]    0    1    0    1    0
#[4,]    1    2    1    2    2
#[5,]    2    2    0    2    2

#[[2]]
#[1] TRUE

#[[3]]
#[1] 10000

bml.sim(15,15,0.9)
#bml.sim(15,15,0.9) # After running many times, I observe that the traffic jams always occur.
#[[1]]   # It takes only 5 steps.
#     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15]
#[1,]    0    1    2    1    1    1    2    1    2     2     2     0     1     1     2
#[2,]    1    1    1    2    1    1    2    2    1     1     2     0     2     1     1
#[3,]    1    2    2    2    2    1    2    2    0     1     2     1     2     2     2
#[4,]    2    2    2    2    0    1    2    1    1     2     1     1     2     1     1
#[5,]    1    2    2    2    1    2    1    1    2     2     2     2     1     1     1
#[6,]    0    1    2    0    1    2    2    2    0     0     1     1     1     2     2
#[7,]    0    1    2    0    1    1    1    2    1     1     1     1     1     2     0
#[8,]    1    2    2    0    2    2    1    2    1     1     2     1     1     1     1
#[9,]    2    2    2    0    1    2    2    2    1     1     2     2     0     1     1
#[10,]    1    2    0    1    2    1    2    0    1     1     2     1     1     1     1
#[11,]    1    2    1    2    2    2    2    1    2     2     1     1     2     1     2
#[12,]    1    2    1    2    1    1    2    2    2     1     1     1     1     2     1
#[13,]    2    2    1    2    2    2    2    2    2     2     2     2     1     1     2
#[14,]    1    1    1    2    2    1    1    2    2     2     2     2     1     2     2
#[15,]    2    1    1    2    0    1    1    2    1     1     2     2     1     1     1

#[[2]]
#[1] FALSE

#[[3]]
#[1] 5

bml.sim(5,5,0.9) # compare with bml.sim(15,15,0.9), the free flowing traffic is still possible for the grid(5x5) with p=0.9.
#[[1]] 
#      [,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    0    1    1
#[2,]    0    0    1    1    2
#[3,]    1    1    2    1    2
#[4,]    1    1    2    0    1
#[5,]    1    2    0    1    1

#[[2]]
#[1] TRUE

#[[3]]
#[1] 10000


######## CONCLUSIONS.
# In general, the higher the density(p) is, the more frequent the traffic jams are.
# When p is small, the free flowing traffic is more frequent.
# In addition, we can campare bml.sim(15,15,0.6) and bml.sim(5,5,0.6), and we observe that the 
# bigger grid sizes (15x15) tend to create more jams than the smaller ones (5x5), so the bigger
# the grid size is, the heavier the traffic jams are.



