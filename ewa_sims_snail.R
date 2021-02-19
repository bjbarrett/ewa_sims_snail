###let's do an individual learning sim from first principals using EWA model.
##we can start with a continuous simulation, and add agents and heterogeneity later

######Functions

######create a softmax function to simply code
Softmax <- function(x){
  exp(x)/sum(exp(x))
}

#color palate
col.pal <- c("#1B9E77", "#D95F02")
# green (low payoff) and red (high payoff)

#simulate data set conditions
dsim <- data.frame( timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
timesteps <- 100 # number of timesteps (t), in snail example, every interaction with a snail is a new timestep
phi <- 0.1 # low is more reliance on past memory and high is reliance on recent experience (0-1)
lambda <- 0.2 # sensitivity to differences in attraction, low is insensitive (0 is no attention to differences in attraction score, equal chance to pick each behavior), higher is more sensitive (0-infinity)
techmeans <- c(8,12) # pay offs of behaviors 1 and 2 
techvar <- c(1,1) # variance in pay offs of behavior across time steps
#visualize how hard to learn
plot(density(rnorm( 10000, mean=techmeans[1] , sd=techvar[1] ) ) ,col=col.pal[1] , xlim=c(0,20)) 
lines(density(rnorm( 10000, mean=techmeans[2] , sd=techvar[2] ) ) ,col=col.pal[2]) 

AC <- matrix(0,nrow=timesteps,ncol=2) 	

##simulated data for one discreet individual

for (t in 1:timesteps){
  prtech_i <-  Softmax(lambda*AC[t,]) #calculate probability of performing a behavior at this timestep
  tech <- sample( 1:2 , size=1 , prob=prtech_i) # sample a behavior with prtech_i
  payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
  obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
  obs_payoffs_t[tech] <- payoff #populate with each observed payoff
  
  dsim[t,] <- c(  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , AC[t,1] , AC[t,2] ,  prtech_i[1] , prtech_i[2] )
  
  # update attractions for next timestep t + 1, don't do on final round
   if(t<timesteps){ 
    for (k in 1:2){
      AC[t+1,k] <- (1-phi)*AC[t,k] + phi*obs_payoffs_t[k]
    }
  }
}

#below plots sim
plot(dsim$Pr1~dsim$timestep , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1.1) ) 
points(dsim$Pr2~dsim$timestep,col=col.pal[2],pch=19 )
points(rep(1.05,timesteps) ~ dsim$timestep,col=col.pal[dsim$tech],pch=19 )
abline(h=1)

####### mean dynamics of model i, no variance, just a numerical solution
dsim2 <- data.frame( timestep=0 , A1=0 , A2=0 , Pr1=0 , Pr2=0)

for (t in 1:timesteps){
  prtech_i <-  Softmax(lambda*AC[t,]) #calculate probability of performing a behavior at this timestep
  obs_payoffs_t <- techmeans #initialize observed payoffs vector

  dsim2[t,] <- c(  t , AC[t,1] , AC[t,2] ,  prtech_i[1] , prtech_i[2] )
  
  # update attractions for next timestep t + 1, don't do on final round
  if(t<timesteps){ 
    for (k in 1:2){
      AC[t+1,k] <- (1-phi)*AC[t,k] + phi*obs_payoffs_t[k]
    }
  }
}

#below plots sims
plot(dsim2$Pr1~dsim2$timestep , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1) ) 
points(dsim2$Pr2~dsim2$timestep,col=col.pal[2],pch=19 )


###lets write a simple function to plot dynamics on average

sim_reinf_learn <- function(phi,lambda){
  techmeans <- c(8,12)
  timesteps <- 100
  dsim2 <- data.frame( timestep=0 , A1=0 , A2=0 , Pr1=0 , Pr2=0)
  
  for (t in 1:timesteps){
    prtech_i <-  Softmax(lambda*AC[t,]) #calculate probability of performing a behavior at this timestep
    obs_payoffs_t <- techmeans #initialize observed payoffs vector
    dsim2[t,] <- c(  t , AC[t,1] , AC[t,2] ,  prtech_i[1] , prtech_i[2] )
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AC[t+1,k] <- (1-phi)*AC[t,k] + phi*obs_payoffs_t[k]
      }
    }
  }
  #below plots sims
  plot(dsim2$Pr1~dsim2$timestep , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1) ) 
  points(dsim2$Pr2~dsim2$timestep,col=col.pal[2],pch=19 )
  
}

##run some functions to explore average dynamics across simulation space
sim_reinf_learn(phi=0.5,lambda=0.1)
sim_reinf_learn(phi=0.9,lambda=0.1)
sim_reinf_learn(phi=0.01,lambda=0.3)
sim_reinf_learn(phi=0.1,lambda=0.3)
sim_reinf_learn(phi=0.5,lambda=0.3)
sim_reinf_learn(phi=0.1,lambda=1)
sim_reinf_learn(phi=0.9,lambda=1)

