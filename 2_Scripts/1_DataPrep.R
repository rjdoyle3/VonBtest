#######################################
#Von B modeling for Mille Lacs Walleye#
#######################################

#Coded by:  Kevin McDonnell - MNDNR
#Date:      05/27/2025
#email:     kevin.mcdonnell.state.mn.us
#Purpose:   This script is used to run the Von Bertalanffy growth model for Mille Lacs walleye

#Load Packages


#Set Paths
main_path<-"C:/Users/EU01242075/OneDrive - State of Minnesota - MN365/GitExample/VonBExample/" #Point this to where the folders for analysis are
dat_path<-paste0(main_path,"1_Data/")
code_path<-paste0(main_path,"2_Scripts/")
out_path<-paste0(main_path,"3_Output/")

#Load data
dater<-read.table(paste0(dat_path,"All gn data 2024_20241025.txt"),header = TRUE)
#Check data structure
summary(dater)

#Scrub data
colnames(dater)[colnames(dater)=="length"]<-"lengthz"

#Remove NAs from length and age data
#Convert weigth and length 0s to NA
dater[dater[,"weight"]==0 & !is.na(dater[,"weight"]),"weight"]<-NA
dater[dater[,"lengthz"]==0 & !is.na(dater[,"lengthz"]) ,"lengthz"]<-NA

dater<-dater[!is.na(dater$lengthz) & !is.na(dater$age) & !is.na(dater$weight),]

#Remove unknown sex
unique(dater[,"sexcode"])
dater<-dater[dater[,"sexcode"]%in%c("MI","FI","MM","FM"),]
dater$sexcode_reg<-0 #make males 0 and females 1
dater[dater[,"sexcode"]=="FM"|dater[,"sexcode"]=="FI","sexcode_reg"]<-1
#Calculate cohort
dater$cohort<-dater[,"year"]-dater[,"age"]

#Remove data with cohorts with sample size <30
cohort_remove<-names(table(dater[,"cohort"])[table(dater[,"cohort"])<30])
dater<-dater[!dater[,"cohort"]%in%cohort_remove,]

#Remove outliers due to observation/measurement errors via Weight/Length relationships
#This assumes that weight and length observation error are confounded with each other
#e.g. if the fish is weights more/less than predicted, it's weight or length is incorrect
#we're assuming we don't know which is "incorrectly" measured, and will remove fish where the relationship looks suspect.
#Note this is just for demonstration purposes, probably not a great assumption...

#W=a(L^b)
LW_fit<-lm(log(weight)~log(lengthz),data = dater)
summary(LW_fit)
LW_predint<-predict(object = LW_fit,interval="prediction",level=0.99999)

dater$outlier<-0
dater[dater[,"weight"]<exp(LW_predint[,"lwr"]) | dater[,"weight"]>exp(LW_predint[,"upr"]),"outlier"]<-1

#Plot it out to double check
windows()
  colz<-c("darkgrey","forestgreen")
  out_index<-dater[,"outlier"]==1
  plot(dater[!out_index,"lengthz"],dater[!out_index,"weight"],pch=19,col=adjustcolor(colz[1],0.1),xlab="length (mm)",ylab = "weight (g)")
  points(dater[out_index,"lengthz"],dater[out_index,"weight"],pch=19,col=adjustcolor(colz[2],0.1))
  curve(exp(LW_fit$coefficients[1]+LW_fit$coefficients[2]*log(x)),from = 100,to = 725,col="red",lty=2,lwd=2,add=TRUE)
legend("topleft",legend = c("Model Dater","Outliers"),col=colz,pch=19)

#How many points are classified as "outliers"
sum(dater[,"outlier"]) #1009
#Apply pred CI cut off
dater<-dater[dater[,"outlier"]==0,]
