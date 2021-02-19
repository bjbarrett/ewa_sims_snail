###let do an individual learning sim from first pricipals using EWA model.
##we can start with a continous simulation, and add agents and heterogeneity l8r

######Functions

######create a softmax function to simply code, add in logit and logistic fcns
Softmax <- function(x){
  exp(x)/sum(exp(x))
}

logit <- function(p){
  (log(p/(1-p)))
}

logistic <- function(x){
  (1/(1+exp(-x)))
}

#color pallate
col.pal <- c("#1B9E77", "#D95F02", "#7570B3") #graphing color pallette
#simulate data set conditions
dsim <- data.frame( timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
											#attraction scores for each behavior
# AC[,2] <- 0.01                                                             #make low payoff behavior rare
#Softmax(AC[1,]) 															#run to see initial prob of choosing a behavior
therow <- 1
timesteps <- 10
phi <- 0.1
lambda <- 1.1
techmeans <- c(8,10)
techvar <- c(0.001,0.001)


AC <- matrix(1,nrow=timesteps,ncol=2) 	

for (t in 1:timesteps){
  prtech_i <-  Softmax(lambda*AC[t,])
  tech <- sample( 1:2 , size=1 , prob=prtech_i)
  payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] )
  # update attractions
  obs_payoffs_t <- rep(0,2)
  obs_payoffs_t[tech] <- payoff#makes payoff yield
  for (k in 1:2){
    AC[t,k] <- (1-phi)*AC[t,k] + phi*obs_payoffs_t[k]
  }
  dsim[therow,] <- c( t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , AC[t,1] ,AC[t,2] ,  prtech_i[1] , prtech_i[2] )
  therow <- therow + 1
}

dsim







 ###########old stuff below

for ( r in 1:nbouts ) {
  for ( i in 1:n ) {  
    
    prtech_i <-  Softmax(k.lambda*AC[i,]) 
    my.phi <- logistic( phi.sim + phi.sim_i[i] )		#weight given to past experience for individual i
    my.fconf <- exp( fc.sim + fc.sim_i[i])  			#strength of frequency dependence for individual i

    # frequency dependent social learning aspect below
    if ( r >= 1 ) { 
      if (sum( prtech_su ) > 0 ) {
        
        # compute frequency cue
        for ( j in 1:3 ){ s_temp[j] <- prtech_su[j]^my.fconf}
        
        prtech_s <- s_temp/sum(s_temp)
        prtech <- (1-my.gam)*prtech_i + my.gam*prtech_s
        
      } else {
        prtech <- prtech_i
      }
    } else {
      prtech <- prtech_i
    }
    # choose tech
    tech <- sample( 1:3 , size=1 , prob=prtech)
    yield <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] )
    
    
    # update attractions
    yields <- rep(0,3)
    yields[tech] <- yield#makes payoff yield
    for (k in 1:3){
      AC[i,k] <- (1-my.phi)*AC[i,k] + my.phi*yields[k]
    }
    
    dsim_s[therow,] <- c( i , r , tech , yields[1] , yields[2] , yields[3] , S1[r] , S2[r] , S3[r] , AC[i,1] , AC[i,2] , AC[i,3] ,  prtech[1] , prtech[2], prtech[3] )
    therow <- therow + 1
  } #i
  S1[r+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$bout==r] )
  S2[r+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$bout==r] )
  S3[r+1] <- length( dsim_s$tech[dsim_s$tech==3 & dsim_s$bout==r] )
  
}

o <- order( dsim_s$id )
dsim <- dsim_s[o,]


###below is for later social learning complexity
######################################################
######begin data simulations###########################
#######################################################

#data sims
n <- 50                                  #number of individuals/pop size
nbouts <- 75                             #timesteps

#simulate values for options
techmeans <- c(8 , 8 , 8)               #mean efficiency of techniques
techvar <- c(0 , 0 ,0)                   #variance of techniques

#parameter sims
gamma.sim <- logit(1)                   #weight given to social info par on log-odds scale
phi.sim <- logit(0.2)                     #memory/attraction updating par on log-odds scale
fc.sim <- log(1.5)				          #frequency dep par on log scale
k.lambda <- 0.4                           #sensitivity to attraction score differences

#varying effects offsets for individuals
gamma.sim_i <- rnorm( n , mean=0 , sd=0) #weight given to social info offsets per i
phi.sim_i <- rnorm( n , mean=0 , sd=0)   #memory/attraction updating offsets per i
fc.sim_i <- rnorm( n , mean=0 , sd=0)     #frequency dependent offsets per i

#plot to visualize overlap of payoffs
dens(rnorm( 10000 , mean=techmeans[1] , sd=techvar[1] ) ,col=col.pal[1] , xlim=c(0,20) )
dens(rnorm( 10000 , mean=techmeans[2] , sd=techvar[2] ) ,col=col.pal[2] , xlim=c(0,20) , add=TRUE )
dens(rnorm( 10000 , mean=techmeans[3] , sd=techvar[3] ) ,col=col.pal[3] , xlim=c(0,20) , add=TRUE )

#unique parameters for each individual, to visualize heterogeneity and for plotting individual predictions
gamma.sim_id <- round( logistic(gamma.sim + gamma.sim_i), digits=2) 		##simulated gammas for all n individuals
phi.sim_id <- round(logistic(phi.sim +phi.sim_i), digits=2) 				##simulated phis for all n individuals
fc.sim_id <- round(exp(fc.sim + fc.sim_i), digits=2)  						##simulated strength of frequency dependent learning for all n individuals
gamma.sim_id
phi.sim_id
fc.sim_id

#begin to simulate data
dsim_s <- data.frame( id=0 , bout=0 , tech=0 , y1=0 , y2=0, y3=0 , s1=0 , s2=0 , s3=0 , A1=0 , A2=0 , A3=0 , Pr1=0 , Pr2=0 , Pr3=0 )
therow <- 1

AC <- matrix(1,ncol=3,nrow=n) 												#attraction scores for each behavior
# AC[,3] <- 0.01                                                             #make low payoff behavior rare
Softmax(AC[1,]) 															#run to see initial prob of choosing a behavior

S1 <- S2 <- S3 <- rep(0,n+1) 												# num of individuals choosing each tech in previous bout
PS1 <- PS2 <- PS3 <- rep(0,nbouts+1) 										# empty vector for mean observed in previous rounds
s_temp <-  rep(0,3)

S1[1] <- 0.5
S2[1] <- 0.3
S3[1] <- 0.2

for ( r in 1:nbouts ) {
  for ( i in 1:n ) {  
    
    prtech_i <-  Softmax(k.lambda*AC[i,]) 
    my.gam <- logistic( gamma.sim + gamma.sim_i[i] ) 	#social info weight for individual i
    my.phi <- logistic( phi.sim + phi.sim_i[i] )		#weight given to past experience for individual i
    my.fconf <- exp( fc.sim + fc.sim_i[i])  			#strength of frequency dependence for individual i
    prtech_su <- c(S1[r],S2[r],S3[r]) 					#social info individual i observed
    
    # frequency dependent social learning aspect below
    if ( r >= 1 ) { 
      if (sum( prtech_su ) > 0 ) {
        
        # compute frequency cue
        for ( j in 1:3 ){ s_temp[j] <- prtech_su[j]^my.fconf}
        
        prtech_s <- s_temp/sum(s_temp)
        prtech <- (1-my.gam)*prtech_i + my.gam*prtech_s
        
      } else {
        prtech <- prtech_i
      }
    } else {
      prtech <- prtech_i
    }
    # choose tech
    tech <- sample( 1:3 , size=1 , prob=prtech)
    yield <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] )
    
    
    # update attractions
    yields <- rep(0,3)
    yields[tech] <- yield#makes payoff yield
    for (k in 1:3){
      AC[i,k] <- (1-my.phi)*AC[i,k] + my.phi*yields[k]
    }
    
    dsim_s[therow,] <- c( i , r , tech , yields[1] , yields[2] , yields[3] , S1[r] , S2[r] , S3[r] , AC[i,1] , AC[i,2] , AC[i,3] ,  prtech[1] , prtech[2], prtech[3] )
    therow <- therow + 1
  } #i
  S1[r+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$bout==r] )
  S2[r+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$bout==r] )
  S3[r+1] <- length( dsim_s$tech[dsim_s$tech==3 & dsim_s$bout==r] )
  
}

o <- order( dsim_s$id )
dsim <- dsim_s[o,]

#plot raw data of group level effects
plot(s1/n ~ bout, data=dsim[dsim$bout>1,], col=col.pal[1] , ylim=c(0,1.1) , xlim=c(2,nbouts+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Choosing Option" )
points(s2/n ~ bout, data=dsim[dsim$bout>1,] , col=col.pal[2], pch=19)
points(s3/n ~ bout, data=dsim[dsim$bout>1,] , col=col.pal[3], pch=19)
legend("topleft", cex=0.85 , as.character(techmeans), pch=19 ,col=col.pal, horiz=TRUE , bty="n", title="Payoffs")
title(main = paste("Population Mean: lambda=",k.lambda ,", gamma=",round(logistic(gamma.sim), digits=2),", phi=",round(logistic(phi.sim),digits=2),", f=", round( exp(fc.sim), digits=2 ) ) , line = 0.5, outer = FALSE)
