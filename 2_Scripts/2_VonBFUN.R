#######################################
#Von B modeling for Mille Lacs Walleye#
#######################################

#Coded by:  Kevin McDonnell - MNDNR
#Date:      05/27/2025
#email:     kevin.mcdonnell.state.mn.us
#Purpose:   This script is used to run the Von Bertalanffy growth model for Mille Lacs walleye

#######################
#SS estimation of VonB#
#######################
VonBFUN<-function(params,Age,L,optimize=TRUE){
  Linf<-params[1]
  k<-params[2]
  t0<-params[3]
  #Calculate Pred Length
  L_pred<-Linf*(1-exp(-k*(Age-t0)))
  #Calculate SS
  SS<-sum((L-L_pred)^2)
  
  if(optimize==TRUE){
    out<-SS
    } else {
      out<-L_pred
      }

  return(out)
  }


##########################
#Bayesian via RTMB Method#
##########################
# VonBFUN_Bayes<-function(params){
#   
#   getAll(dater,params)
#   lengthz<-OBS(lengthz)
#   
#   #Priors - no uniform implementation with %~%
#   #.nll<-0
#   #nll_linf<-(Linf<0||Linf>1000)*log(0.000001)+(Linf>=0&&Linf<=1000)*log((1/(1000-0)))
#   #.nll<-.nll-nll_linf
#   
#   
#   Linf %~% dnorm(mean = 500,sd = 20)
#   k %~% dnorm(mean = 0, sd=50)
#   t0 %~% dnorm(mean = 0, sd=50)
#   sdeps %~% dgamma(shape = 0.001,scale = 0.001)
#   
#   
#   pred_L<-Linf*(1-exp(-k*(age-t0)))
#   lengthz %~% dnorm(mean = pred_L,sd = sdeps)
#   
#  #return(.nll)
#   
#   }


