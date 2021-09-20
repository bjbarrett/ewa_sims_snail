library(RColorBrewer)

######create a softmax function to simply code
Softmax <- function(x){
  exp(x)/sum(exp(x))
}

logit <- function(p){
  (log(p/(1-p)))
}

logistic <- function(x){
  (1/(1+exp(-x)))
}

##brendan added tech vars to make individual learning harder
##got rid of individual variation for ease of rhetorical approach

# AIMS
#1. show IL can limit spread of adaptive behavior, this is highlighted when memory is important (i.e. phi is low)
# we will fix some value of lambda, as it is likely not importnatn
#2. show positiv frequency dependence and an over reliance of social learning  limits spread of adaptive behavior
#unbiased might be informative but not necessary. lets seem lambda fixed, gamma vary at 0,0.2, 0.5 , 1)
# we can ignore loops for now, and perhaps write a function to save space
#color palet
col.pal <- c("#1B9E77", "#D95F02")
# green (low payoff) and red (high payoff)

individuals <- 100 # number of individuals 
timesteps <- 100 # number of timesteps (t), in snail example, every interaction with a snail is a new timestep

SimFreqDepEWA <- function(fc.sim , gamma.sim , phi.sim){
  individuals <- 100 # number of individuals 
  timesteps <- 100 # number of timesteps (t), in snail example, every interaction with a snail is a new timestep
  
  #setting varying phi per individual for stochastic model
  phi.sim_i <- rnorm(individuals, mean = 0, sd = 0) # can set standard deviation
  phi_id <- logistic(phi.sim +phi.sim_i)
  
  ##### Frequency-dependent learning #####
  # we have two new parameters, gamma (weight of social information) and fc (strength of frequency dependent learning)
  # gamma
  gamma.sim_i <- rnorm( individuals , mean=0 , sd=0) #weight given to social info offsets per i
  gamma.sim_id <- round( logistic(gamma.sim + gamma.sim_i), digits=2) 		##simulated gammas for all n individuals
  
  # frequency dependence 
  fc.sim_i <- rnorm( individuals , mean=0 , sd=0)     #frequency dependent offsets per i
  fc.sim_id <- round(exp(fc.sim + fc.sim_i), digits=2)  						##simulated strength of frequency dependent learning for all n individuals
  
  ## stochastic model with techmeans
  techmeans_bas <- c(6,12) # pay offs of behaviors 1 and 2 baseline
  techvar_bas <- c(2,3) # pay offs of behaviors 1 and 2 baseline
  
  techmeans_exp <- c(6,1) # pay offs switched (experiment)
  techvar_exp <- c(2,1) # pay offs of behaviors 1 and 2 baseline
  
  lambda = 1 
  
  #simulated data looping over individuals
  dsim_s <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, s1 = 0, s2 =0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
  therow <- 1
  
  AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) )
  AR[1,1,] <- 6.8 # attraction score first behavior (pounding)
  AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use
  
  S1 <- S2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
  s_temp <- rep(0,2)
  
  for (t in 1:timesteps){
    for (i in 1:individuals) {
      prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
      prtech_su <- c(S1[t], S2[t])
      
      # frequency dependent social learning
      if (t >= 1) { 
        if(sum(prtech_su) > 0) { 
          
          #compute frequency cue
          for( j in 1:2){  s_temp[j] <- prtech_su[j]^fc.sim_id[i]}
          
          prtech_s <- s_temp/sum(s_temp)
          prtech <- (1- gamma.sim_id[i])*prtech_i + gamma.sim_id[i]*prtech_s
          
        } else { 
          prtech <- prtech_i
        }
      } else {
        prtech <- prtech_i
      }
      #choose tech
      tech <- sample( 1:2 , size=1 , prob=prtech) # sample a behavior with prtech_i
      techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
      techvar <- if(t > timesteps/2) {techvar_exp} else {techvar_bas}
      payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
      obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
      obs_payoffs_t[tech] <- payoff #populate with each observed payoff
      
      # update attractions for next timestep t + 1, don't do on final round
      if(t<timesteps){ 
        for (k in 1:2){
          AR[t+1,k,i] <- (1-phi_id[i])*AR[t,k,i] + phi_id[i]*obs_payoffs_t[k]
        }
      }
      dsim_s[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , S1[t], S2[t], AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2])
      therow <- therow + 1
    }
    #i
    S1[t+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$timestep==t] )
    S2[t+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$timestep==t] )
    
  }
  
  o <- order( dsim_s$i ) #not sure if this is necessary but doesn't harm
  dsim <-  dsim_s[o,] #pparently this is no-no but we can strore this in global environment
  return(dsim)
}

SimPayoffEWA <- function(beta.sim , gamma.sim , phi.sim){
  individuals <- 100 # number of individuals 
  timesteps <- 100 # number of timesteps (t), in snail example, every interaction with a snail is a new timestep
  
  #setting varying phi per individual for stochastic model
  phi.sim_i <- rnorm(individuals, mean = 0, sd = 0) # can set standard deviation
  phi_id <- logistic(phi.sim +phi.sim_i)
  
  ##### Frequency-dependent learning #####
  # we have two new parameters, gamma (weight of social information) and fc (strength of frequency dependent learning)
  # gamma
  gamma.sim_i <- rnorm( individuals , mean=0 , sd=0) #weight given to social info offsets per i
  gamma.sim_id <- round( logistic(gamma.sim + gamma.sim_i), digits=2) 		##simulated gammas for all n individuals
  
  # frequency dependence 
  beta.sim_i <- rnorm( individuals , mean=0 , sd=0)     #frequency dependent offsets per i
  beta.sim_id <- round(exp(beta.sim + beta.sim_i), digits=2)  						##simulated strength of frequency dependent learning for all n individuals
  
  ## stochastic model with techmeans
  techmeans_bas <- c(6,12) # pay offs of behaviors 1 and 2 baseline
  techvar_bas <- c(2,3) # pay offs of behaviors 1 and 2 baseline
  
  techmeans_exp <- c(6,1) # pay offs switched (experiment)
  techvar_exp <- c(2,1) # pay offs of behaviors 1 and 2 baseline
  
  lambda = 1 
  
  #simulated data looping over individuals
  dsim_s <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, s1 = 0, s2 =0, pbar1 = 0 , pbar2=0 , A1=0 , A2=0 , Pr1=0 , Pr2=0)
  therow <- 1
  
  AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) )
  AR[1,1,] <- 6.8 # attraction score first behavior (pounding)
  AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use
 
  S1 <- S2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
  P1 <- P2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
  s_temp <- rep(0,2)
  
  for (t in 1:timesteps){
    for (i in 1:individuals) {
      prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
      prtech_su <- c(S1[t], S2[t])
      paytemp <- c(S1[t], S2[t])
      
      # frequency dependent social learning
      if (t >= 1) { 
        if(sum(prtech_su) > 0) { 
          
          #compute frequency cue
          for( j in 1:2){  s_temp[j] <- paytemp[j]^beta.sim_id[i]}

          prtech_s <- s_temp/sum(s_temp)
          prtech <- (1- gamma.sim_id[i])*prtech_i + gamma.sim_id[i]*prtech_s
          
        } else { 
          prtech <- prtech_i
        }
      } else {
        prtech <- prtech_i
      }
      #choose tech
      tech <- sample( 1:2 , size=1 , prob=prtech) # sample a behavior with prtech_i
      techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
      techvar <- if(t > timesteps/2) {techvar_exp} else {techvar_bas}
      payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
      obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
      obs_payoffs_t[tech] <- payoff #populate with each observed payoff
      
      # update attractions for next timestep t + 1, don't do on final round
      if(t<timesteps){ 
        for (k in 1:2){
          AR[t+1,k,i] <- (1-phi_id[i])*AR[t,k,i] + phi_id[i]*obs_payoffs_t[k]
        }
      }
      dsim_s[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , S1[t], S2[t], P1[t], P2[t] , AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2])
      therow <- therow + 1
    }
    #i
    S1[t+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$timestep==t] )
    S2[t+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$timestep==t] )
    P1[t+1] <- mean( dsim_s$payoff_i1[dsim_s$tech==1 & dsim_s$timestep==t] )
    P2[t+1] <- mean( dsim_s$payoff_i2[dsim_s$tech==2 & dsim_s$timestep==t] )
  }
  
  o <- order( dsim_s$i ) #not sure if this is necessary but doesn't harm
  dsim <-  dsim_s[o,] #pparently this is no-no but we can strore this in global environment
  return(dsim)
}

###lets do simulation, only for tool using populations, use function for brevity
# this stores function simulated dataframe, we can plot later if we call it different things
#individual learning
d_IL_lowphi<- SimFreqDepEWA(fc.sim=log(0.4) , gamma.sim=logit(0) , phi.sim=logit(0.05) ) # no social info, just IL
d_IL_midphi<- SimFreqDepEWA(fc.sim=log(0.4) , gamma.sim=logit(0) , phi.sim=logit(0.2) ) # no social info, just IL

plot(s2/individuals ~ timestep, data=d_IL_midphi[d_IL_midphi$timestep>1,], col=col.pal[1] , ylim=c(0,1.1) , xlim=c(2,timesteps+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Usimg Tools" ) #low phi, tool behavior
points(s2/individuals ~ timestep, data=d_IL_lowphi[d_IL_lowphi$timestep>1,] , col=col.pal[2], pch=19)
abline(v=50 , lty=2)
legend("topleft", cex=0.85 , c("0.05" , "0.20"), pch=19 ,col=col.pal, horiz=TRUE , bty="n", title="phi")
title(main = "Individual Learning" , line = 0.5, outer = FALSE)

###lets do  social learning case, just conformity
d_conf_SL_zerogamma <- SimFreqDepEWA(fc.sim=log(2) , gamma.sim=logit(0) , phi.sim=logit(0.15) ) # no social info, just IL
d_conf_SL_lowgamma <- SimFreqDepEWA(fc.sim=log(2) , gamma.sim=logit(0.25) , phi.sim=logit(0.15) ) # no social info, just IL
d_conf_SL_highgamma <- SimFreqDepEWA(fc.sim=log(2) , gamma.sim=logit(0.75) , phi.sim=logit(0.15) ) # no social info, just IL
d_conf_SL_onegamma <- SimFreqDepEWA(fc.sim=log(2) , gamma.sim=logit(1) , phi.sim=logit(0.15) ) # no social info, just IL

col_pal2 <- brewer.pal(4, 'PuBuGn')

plot(s2/individuals ~ timestep, data=d_conf_SL_zerogamma[d_conf_SL_zerogamma$timestep>1,], col=col_pal2[1] , ylim=c(0,1.1) , xlim=c(2,timesteps+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Usimg Tools" ) #low phi, tool behavior
points(s2/individuals ~ timestep, data=d_conf_SL_lowgamma[d_conf_SL_lowgamma$timestep>1,] , col=col_pal2[2], pch=19)
points(s2/individuals ~ timestep, data=d_conf_SL_highgamma[d_conf_SL_highgamma$timestep>1,] , col=col_pal2[3], pch=19)
points(s2/individuals ~ timestep, data=d_conf_SL_onegamma[d_conf_SL_onegamma$timestep>1,] , col=col_pal2[4], pch=19)
abline(v=50 , lty=2)

legend("topleft", cex=0.85 , c("0" , "0.25" , "0.75" , "1"), pch=19 ,col=col_pal2, horiz=TRUE , bty="n", title="gamma")
#title(main = "Individual Learning" , line = 0.5, outer = FALSE)


###lets vary fc, and do a 50/50 mix IL and SL

###lets do  social learning case, just conformity
d_SL_1 <- SimFreqDepEWA(fc.sim=log(2.5) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) # no social info, just IL
d_SL_2 <- SimFreqDepEWA(fc.sim=log(1.5) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) # no social info, just IL
d_SL_3 <- SimFreqDepEWA(fc.sim=log(1) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) # no social info, just IL
d_SL_4 <- SimFreqDepEWA(fc.sim=log(0.5) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) # no social info, just IL

col_pal2 <- brewer.pal(4, 'RdPu') #new color palatte

plot(s2/individuals ~ timestep, data=d_SL_1 [d_SL_1 $timestep>1,], col=col_pal2[1] , ylim=c(0,1.1) , xlim=c(1,timesteps+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Usimg Tools" ) #low phi, tool behavior
points(s2/individuals ~ timestep, data=d_SL_2[d_SL_2$timestep>1,] , col=col_pal2[2], pch=19)
points(s2/individuals ~ timestep, data=d_SL_3[d_SL_3$timestep>1,] , col=col_pal2[3], pch=19)
points(s2/individuals ~ timestep, data=d_SL_4[d_SL_4$timestep>1,] , col=col_pal2[4], pch=19)
abline(v=50 , lty=2)
legend("topleft", cex=0.85 , c("2.5" , "1.5" , "1" , "0.5"), pch=19 ,col=col_pal2, horiz=TRUE , bty="n", title="strength of frequency dependence")
#title(main = "Individual Learning" , line = 0.5, outer = FALSE)

###payoff
d_pay_SL_betaone <- SimPayoffEWA(beta.sim=log(1) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) # no social info, just IL
d_pay_SL_betamid <- SimPayoffEWA(beta.sim=log(2) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) # no social info, just IL
d_pay_SL_beta100 <- SimPayoffEWA(beta.sim=log(5) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) 

col_pal2 <- brewer.pal(4, 'PuOr') #new color palatte

plot(s2/individuals ~ timestep, data=d_pay_SL_betaone [d_pay_SL_betaone $timestep>1,], col=col_pal2[1] , ylim=c(0,1.1) , xlim=c(1,timesteps+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Using Tools" ) #low phi, tool behavior
points(s2/individuals ~ timestep, data=d_pay_SL_betamid[d_pay_SL_betamid$timestep>1,] , col=col_pal2[2], pch=19)
points(s2/individuals ~ timestep, data=d_pay_SL_beta100[d_pay_SL_beta100$timestep>1,] , col=col_pal2[3], pch=19)
abline(v=50 , lty=2)
legend("topleft", cex=0.85 , c("1" , "2" , "5" ), pch=19 ,col=col_pal2, horiz=TRUE , bty="n", title="strength of payoff-bias")
#title(main = "Individual Learning" , line = 0.5, outer = FALSE)

