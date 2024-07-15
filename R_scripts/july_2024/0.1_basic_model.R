
# BASIC MODEL - aka our recreation of the van de Leemput (vdL) et al. (2016) model

#Set initial conditions for state variables
H0<-0.9
C0<-0.75
A0<-0.1
S0 <- 1 - C0 - A0

# specify state variable initial values
state_vars<-c(HH=H0, CC=C0, AA=A0, SS=S0)

#generate a series of times at which you want the ODE solver to output population sizes
tseq<-seq(0, 1000, by=0.1)

#Generate a vector of parameter values
pars<-c(bc<-0.3,
        ba<-0.8,
        ic<-0.05,
        ia<-0.05,
        dc<-0.1,
        r<-1,
        g<-1,
        eta<-1,
        alpha<-0.5,
        sigma<-0.6,
        f<-0.1)

# Set up function to run the model
Basic_Model<-function(tseq, state_vars, pars){
  HH <- state_vars[1]
  CC <- state_vars[2]
  AA <- state_vars[3]
  SS <- 1 - CC - AA
  
  bc <- pars[1]
  ba <- pars[2]
  ic <- pars[3]
  ia <- pars[4]
  dc <- pars[5]
  r <- pars[6]
  g <- pars[7]
  eta <- pars[8]
  alpha <- pars[9]
  sigma <- pars[10]
  f <- pars[11]
  
  dC_dt <- (ic + bc*CC)*SS*(1-alpha*AA)- dc*CC
  dA_dt <-(ia + ba*AA)*SS - (g*HH*AA / (g*eta*AA+1))
  dH_dt <-r*HH*(1 - (HH / ((1-sigma) + sigma*CC))) - f*HH
  
  return(list(c(dH<- dH_dt,
                dC <- dC_dt,
                dA <- dA_dt)))
}
