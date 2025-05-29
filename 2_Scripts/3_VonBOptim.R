#######################################
#Von B modeling for Mille Lacs Walleye#
#######################################

#Coded by:  Kevin McDonnell - MNDNR
#Date:      05/27/2025
#email:     kevin.mcdonnell.state.mn.us
#Purpose:   This script is used to run the Von Bertalanffy growth model for Mille Lacs walleye

#Load Packages
#require("RTMB")
#require("tmbstan")

#Set Paths
main_path<-"C:/Users/EU01242075/OneDrive - State of Minnesota - MN365/GitExample/VonBExample/" #Point this to where the folders for analysis are
dat_path<-paste0(main_path,"1_Data/")
code_path<-paste0(main_path,"2_Scripts/")
out_path<-paste0(main_path,"3_Output/")

#Bring in scrubbed data
source(paste0(code_path,"1_DataPrep.R"))

#Load Von B function
source(paste0(code_path,"2_VonBFUN.R"))

#Make sure function works
#These are just random guesses at the parameter values used to check if the model works
VonBFUN(params = c(700,0.3,0),Age = dater[,"age"],L = dater[,"lengthz"],optimize = TRUE) #Should spit out some gigantic number

#Feed it into the nlimb optimzer
VonBOptim<-nlminb(start = c(700,0.3,0), #Initial guesses for the parameters
                   objective = VonBFUN, #The function to minimize
                   Age = dater[,"age"], #Age data
                   L = dater[,"lengthz"], #Length data
                   optimize = TRUE, #Set optimize to TRUE to return the sum of squares
                   control = list(trace=1)) #Control options, trace=1 will print out the optimization steps
VonBOptim
VonBOptim$par #Looks good

#Save the output
#Save parameter estimates as a RDS object
saveRDS(VonBOptim, file = paste0(out_path,"VonBOptim.rds"))

#Create a .pdf figure to show fit.
pdf(file = paste0(out_path,"MilleLacsWalleye_VonBfit.pdf"), width = 8, height = 6)
  par(mar=c(4.5,5,2,2))
  plot(dater[,"age"],dater[,"lengthz"],pch=19,col=adjustcolor("lightgrey",0.5),xlab="Age",ylab="",xaxt="n",las=2)
  axis(1)
  mtext(text = "Length (mm)",side = 2,line = 3.5)
  curve(VonBFUN(params = VonBOptim$par,L=NULL,Age = x,optimize=FALSE),from=0,to=25,col="red",lwd=2,lty=2,add=TRUE)
dev.off()

##########################
#Bayesian via RTMB Method#
##########################
# params<-list(Linf=500,
#               k=0.1,
#               t0=0,
#               sdeps=25)
# TapeConfig(comparison = "tape")
# obj<-MakeADFun(func = VonBFUN_Bayes,parameters = params)
# opt<-nlminb(start = obj$par,objective = obj$fn,gradient = obj$gr)
# opt
# 
# #Check out initital result
# windows()
# plot(dater[,"age"],dater[,"lengthz"],ylim=c(0,750),col=adjustcolor("lightgrey",0.25),pch=19)
# curve(opt$par["Linf"]*(1-exp(-opt$par["k"]*(x-opt$par["t0"]))),from=0,to=25,col="red",add=TRUE,lwd=2,lty=2)
# 
# #Use initial result as starting point for sampling posterior
# post_samps<-tmbstan(obj = obj,iter=2500,init="last.par.best",chains=3,thin=1)
# summary(post_samps) #Same results but with error estimates!
# 
# #check convergence
# windows()
# traceplot(post_samps,inc_warmup = TRUE)
# windows()
# plot(post_samps,plotfun="hist")







