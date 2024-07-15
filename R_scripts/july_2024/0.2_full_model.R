
# FULL Model - our expanded model including herbivore and algal functional groups

## Note:
 # - P2_pars are the parameter values that allow our full/expanded model to collapse down to the van de Leemput (vdL) et al. (2016) model. Used for analyses referred to as 'collapsed'.
 # - hh_pars are the parameter values that we use for the rest of our analyses as we adjusted the parameter values to be more realistic compared to P2_pars. Please refer to the supplement for justification of our assumptions underlying our parameter values.


# Initial Conditions - these are for 'collapsed' analyses.
G0<-0.45
R0<-0
B0<-0.45
C0<-0.75
T0<-0.05
M0<-0.05
S0 <- 1 - T0 - C0 - M0


# set state variables
P2_state_vars<-c(GG=G0, RR=R0, BB=B0, CC=C0, TT=T0, MM=M0, SS=S0)

#generate a series of times at which you want the ODE solver to output population sizes
P2_tseq<-seq(0, 1000, by=0.1)

#Generate a vector of parameter values
P2_pars<-c(bc<-0.3,
           bt<-0.8,      #use the same value as bm from basic model
           bm<-0.8,      #use the same value as bm from basic model
           ic<-0.05,
           it<-0.025,    #need two of these, one for turf and one for macroalgae
           im<-0.025,    #need two of these, one for turf and one for macroalgae
           dc<-0.1,
           r<-1,
           gt<-2,        #need two of these, one for turf and one for macroalgae
           gm<-2,        #need two of these, one for turf and one for macroalgae
           etaT<-1,      #need two of these, one for turf and one for macroalgae
           etaM<-1,      #need two of these, one for turf and one for macroalgae
           alphaT<-0.5,
           alphaM<-0.5,
           sigma<-0.6,
           f<-0.1,
           gamma<-0)



# Set up function to run the model
P2_Model<-function(P2_tseq, P2_state_vars, P2_pars){
  GG <- P2_state_vars[1]
  RR <- P2_state_vars[2]
  BB <- P2_state_vars[3]
  CC <- P2_state_vars[4]
  TT <- P2_state_vars[5]
  MM <- P2_state_vars[6]
  SS <- 1 - CC - TT - MM
  
  bc <- P2_pars[1]
  bt <- P2_pars[2]
  bm <- P2_pars[3]
  ic <- P2_pars[4]
  it <- P2_pars[5]
  im <- P2_pars[6]
  dc <- P2_pars[7]
  r <- P2_pars[8]
  gt <- P2_pars[9]
  gm <- P2_pars[10]
  etaT <- P2_pars[11]
  etaM <- P2_pars[12]
  alphaT <- P2_pars[13]
  alphaM <- P2_pars[14]
  sigma <- P2_pars[15]
  f <- P2_pars[16]
  gamma <- P2_pars[17]
  
  dG_dt <-r*GG*(1 - ((GG+RR+BB) / ((1-sigma) + sigma*CC))) - f*GG
  dR_dt <-r*RR*(1 - ((GG+RR+BB) / ((1-sigma) + sigma*CC))) - f*RR
  dB_dt <-r*BB*(1 - ((GG+RR+BB) / ((1-sigma) + sigma*CC))) - f*BB
  dC_dt <- (ic + bc*CC)*SS*(1-(alphaT*TT+alphaM*MM)) - dc*CC
  dT_dt <- (it + bt*TT)*(SS) - (gamma*TT) - (((gt*GG*TT) / (gt*etaT*TT+1)) + ((gt*RR*TT)/(gt*etaT*TT+gm*etaM*MM+1)))
  dM_dt <- (im + bm*MM)*(SS) + (gamma*TT) - (((gm*BB*MM) / (gm*etaM*MM+1)) + ((gm*RR*MM)/(gt*etaT*TT+gm*etaM*MM+1)))
  
  
  return(list(c(dG <- dG_dt,
                dR <- dR_dt,
                dB <- dB_dt,
                dC <- dC_dt,
                dT <- dT_dt,
                dM <- dM_dt)))
}



#Generate a vector of parameter values - for our full / expanded model
hh_pars<-c(bc<-0.3, bt<-0.8, bm<-0.5,     #expansion of current benthic cover
           ic<-0.05, it<-0.05, im<-0,     #import of propagules
           dc<-0.1,                       #coral mortality
           r<-1,                          #herbivore growth rate
           gt<-2, gm<-1,                  #algae mortality
           etaT<-0, etaM<-1,              #algae handling time
           alphaT<-0.25, alphaM<-0.5,     #competitive effect on coral
           sigma<-0.6,                    #relationship between coral & herbivores
           f<-0.5,                        #fishing pressure
           gamma<-0.1)                    #transition from turf to macroalgae


