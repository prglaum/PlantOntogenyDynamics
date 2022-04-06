##SIMULATION DATA LOADER##
##These packages need to be active to create lists of files to load 
##sequentially into a single R object
library(readxl)
library(readr)
library(dplyr)
library(tidymv)

###
#We loaded data in subsets based on our sensitivity analysis parameter, 
#i.e., the handling times (h) and the density dependent terms (alphas)
##
#To do this, download and save each combination of sensitivity parameters 
#into their own folder. This folder will be set as the working directory
#and will be the where we load individual files from. 
##

###############
###alpha .06, both h=1###
##############
#loading the data, it needs to be in one folder with nothing else in it
setwd("~/path to where you saved the folder/XXX")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)
h1a06 <- bind_rows(df.list, .id = "id")
##need to run these too
##Mathematica's precision, some values came with a slight error rate < e^-10.
##Run this to remove the unnecessary decimal. 
h1a06$rF=round(h1a06$rF,1)
h1a06$g1=round(h1a06$g1,2)
h1a06$g2=round(h1a06$g2,2)
h1a06$a2=round(h1a06$a2,1)
h1a06$aF=round(h1a06$aF,1)

#The following equations form the output from our model's subfunctions
#delta, gamma12(gam1), gamma2F(gam2), thetaF and thetaS2 in order to 
#make percCons
h1a06$delta=h1a06$Feqm*(h1a06$rF - h1a06$alpF*h1a06$Feqm)
h1a06$gam1=( h1a06$g1*h1a06$S1eqm/(1 + h1a06$alp1*(h1a06$Feqm + .2*(h1a06$S1eqm + h1a06$S2eqm))) )
h1a06$gam2=( h1a06$g2*h1a06$S2eqm/(1 + h1a06$alp2*(h1a06$Feqm + .2*(h1a06$S2eqm))) )

h1a06$thetaF= ((h1a06$Heqm*.6*h1a06$aF*h1a06$Feqm)/(1+h1a06$aF*h1a06$hF*h1a06$Feqm + h1a06$a2*h1a06$h2*h1a06$S2eqm) )
h1a06$thetaS2= ((h1a06$Heqm*.6*h1a06$a2*h1a06$S2eqm)/(1+h1a06$aF*h1a06$hF*h1a06$Feqm + h1a06$a2*h1a06$h2*h1a06$S2eqm) )
h1a06$thetas=h1a06$thetaF+h1a06$thetaS2

h1a06$percCons=((h1a06$thetaF/.6)+(h1a06$thetaS2/.6))/(h1a06$S1eqm+h1a06$S2eqm+h1a06$Feqm)

##############################
###alpha .06 h2=0.5, hF=1###
#############################
#loading the data, it needs to be in one folder with nothing else in it
setwd("~/path to where you saved the folder/XXX")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)
hf1h205a06 <- bind_rows(df.list, .id = "id")
##need to run these too
hf1h205a06$rF=round(hf1h205a06$rF,1)
hf1h205a06$g1=round(hf1h205a06$g1,2)
hf1h205a06$g2=round(hf1h205a06$g2,2)
hf1h205a06$a2=round(hf1h205a06$a2,1)
hf1h205a06$aF=round(hf1h205a06$aF,1)

#The following equations form the output from our model's subfunctions
#delta, gamma12(gam1), gamma2F(gam2), thetaF and thetaS2 in order to 
#make percCons
hf1h205a06$delta=hf1h205a06$Feqm*(hf1h205a06$rF - hf1h205a06$alpF*hf1h205a06$Feqm)
hf1h205a06$gam1=( hf1h205a06$g1*hf1h205a06$S1eqm/(1 + hf1h205a06$alp1*(hf1h205a06$Feqm + .2*(hf1h205a06$S1eqm + hf1h205a06$S2eqm))) )
hf1h205a06$gam2=( hf1h205a06$g2*hf1h205a06$S2eqm/(1 + hf1h205a06$alp2*(hf1h205a06$Feqm + .2*(hf1h205a06$S2eqm))) )

hf1h205a06$thetaF= ((hf1h205a06$Heqm*.6*hf1h205a06$aF*hf1h205a06$Feqm)/(1+hf1h205a06$aF*hf1h205a06$hF*hf1h205a06$Feqm + hf1h205a06$a2*hf1h205a06$h2*hf1h205a06$S2eqm) )
hf1h205a06$thetaS2= ((hf1h205a06$Heqm*.6*hf1h205a06$a2*hf1h205a06$S2eqm)/(1+hf1h205a06$aF*hf1h205a06$hF*hf1h205a06$Feqm + hf1h205a06$a2*hf1h205a06$h2*hf1h205a06$S2eqm) )
hf1h205a06$thetas=hf1h205a06$thetaF+hf1h205a06$thetaS2

hf1h205a06$percCons=((hf1h205a06$thetaF/.6)+(hf1h205a06$thetaS2/.6))/(hf1h205a06$S1eqm+hf1h205a06$S2eqm+hf1h205a06$Feqm)

##############################
###alpha .06 h2=1, hF=0.5###
#############################
#loading the data, it needs to be in one folder with nothing else in it
setwd("~/path to where you saved the folder/XXX")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)
hf05h21a06 <- bind_rows(df.list, .id = "id")
##need to run these too
hf05h21a06$rF=round(hf05h21a06$rF,1)
hf05h21a06$g1=round(hf05h21a06$g1,2)
hf05h21a06$g2=round(hf05h21a06$g2,2)
hf05h21a06$a2=round(hf05h21a06$a2,1)
hf05h21a06$aF=round(hf05h21a06$aF,1)

#The following equations form the output from our model's subfunctions
#delta, gamma12(gam1), gamma2F(gam2), thetaF and thetaS2 in order to 
#make percCons
hf05h21a06$delta=hf05h21a06$Feqm*(hf05h21a06$rF - hf05h21a06$alpF*hf05h21a06$Feqm)
hf05h21a06$gam1=( hf05h21a06$g1*hf05h21a06$S1eqm/(1 + hf05h21a06$alp1*(hf05h21a06$Feqm + .2*(hf05h21a06$S1eqm + hf05h21a06$S2eqm))) )
hf05h21a06$gam2=( hf05h21a06$g2*hf05h21a06$S2eqm/(1 + hf05h21a06$alp2*(hf05h21a06$Feqm + .2*(hf05h21a06$S2eqm))) )

hf05h21a06$thetaF= ((hf05h21a06$Heqm*.6*hf05h21a06$aF*hf05h21a06$Feqm)/(1+hf05h21a06$aF*hf05h21a06$hF*hf05h21a06$Feqm + hf05h21a06$a2*hf05h21a06$h2*hf05h21a06$S2eqm) )
hf05h21a06$thetaS2= ((hf05h21a06$Heqm*.6*hf05h21a06$a2*hf05h21a06$S2eqm)/(1+hf05h21a06$aF*hf05h21a06$hF*hf05h21a06$Feqm + hf05h21a06$a2*hf05h21a06$h2*hf05h21a06$S2eqm) )
hf05h21a06$thetas=hf05h21a06$thetaF+hf05h21a06$thetaS2

hf05h21a06$percCons=((hf05h21a06$thetaF/.6)+(hf05h21a06$thetaS2/.6))/(hf05h21a06$S1eqm+hf05h21a06$S2eqm+hf05h21a06$Feqm)

##############################
###alpha .06 h2=0.5, hF=0.5###
#############################
#loading the data, it needs to be in one folder with nothing else in it
setwd("~/path to where you saved the folder/XXX")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)
h05a06 <- bind_rows(df.list, .id = "id")
##need to run these too
h05a06$rF=round(h05a06$rF,1)
h05a06$g1=round(h05a06$g1,2)
h05a06$g2=round(h05a06$g2,2)
h05a06$a2=round(h05a06$a2,1)
h05a06$aF=round(h05a06$aF,1)

#The following equations form the output from our model's subfunctions
#delta, gamma12(gam1), gamma2F(gam2), thetaF and thetaS2 in order to 
#make percCons
h05a06$delta=h05a06$Feqm*(h05a06$rF - h05a06$alpF*h05a06$Feqm)
h05a06$gam1=( h05a06$g1*h05a06$S1eqm/(1 + h05a06$alp1*(h05a06$Feqm + .2*(h05a06$S1eqm + h05a06$S2eqm))) )
h05a06$gam2=( h05a06$g2*h05a06$S2eqm/(1 + h05a06$alp2*(h05a06$Feqm + .2*(h05a06$S2eqm))) )

h05a06$thetaF= ((h05a06$Heqm*.6*h05a06$aF*h05a06$Feqm)/(1+h05a06$aF*h05a06$hF*h05a06$Feqm + h05a06$a2*h05a06$h2*h05a06$S2eqm) )
h05a06$thetaS2= ((h05a06$Heqm*.6*h05a06$a2*h05a06$S2eqm)/(1+h05a06$aF*h05a06$hF*h05a06$Feqm + h05a06$a2*h05a06$h2*h05a06$S2eqm) )
h05a06$thetas=h05a06$thetaF+h05a06$thetaS2

h05a06$percCons=((h05a06$thetaF/.6)+(h05a06$thetaS2/.6))/(h05a06$S1eqm+h05a06$S2eqm+h05a06$Feqm)

##############################
###alpha .1 h2=0.5, hF=1###
#############################
#loading the data, it needs to be in one folder with nothing else in it
setwd("~/path to where you saved the folder/XXX")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)
rm(hf1h205a1);
hf1h205a1 <- bind_rows(df.list, .id = "id")
##need to run these too
hf1h205a1$rF=round(hf1h205a1$rF,1)
hf1h205a1$g1=round(hf1h205a1$g1,2)
hf1h205a1$g2=round(hf1h205a1$g2,2)
hf1h205a1$a2=round(hf1h205a1$a2,1)
hf1h205a1$aF=round(hf1h205a1$aF,1)

#The following equations form the output from our model's subfunctions
#delta, gamma12(gam1), gamma2F(gam2), thetaF and thetaS2 in order to 
#make percCons
hf1h205a1$delta=hf1h205a1$Feqm*(hf1h205a1$rF - hf1h205a1$alpF*hf1h205a1$Feqm)
hf1h205a1$gam1=( hf1h205a1$g1*hf1h205a1$S1eqm/(1 + hf1h205a1$alp1*(hf1h205a1$Feqm + .2*(hf1h205a1$S1eqm + hf1h205a1$S2eqm))) )
hf1h205a1$gam2=( hf1h205a1$g2*hf1h205a1$S2eqm/(1 + hf1h205a1$alp2*(hf1h205a1$Feqm + .2*(hf1h205a1$S2eqm))) )

hf1h205a1$thetaF= ((hf1h205a1$Heqm*.6*hf1h205a1$aF*hf1h205a1$Feqm)/(1+hf1h205a1$aF*hf1h205a1$hF*hf1h205a1$Feqm + hf1h205a1$a2*hf1h205a1$h2*hf1h205a1$S2eqm) )
hf1h205a1$thetaS2= ((hf1h205a1$Heqm*.6*hf1h205a1$a2*hf1h205a1$S2eqm)/(1+hf1h205a1$aF*hf1h205a1$hF*hf1h205a1$Feqm + hf1h205a1$a2*hf1h205a1$h2*hf1h205a1$S2eqm) )
hf1h205a1$thetas=hf1h205a1$thetaF+hf1h205a1$thetaS2

hf1h205a1$percCons=((hf1h205a1$thetaF/.6)+(hf1h205a1$thetaS2/.6))/(hf1h205a1$S1eqm+hf1h205a1$S2eqm+hf1h205a1$Feqm)


##############################
###alpha .1 h2=1, hF=0.5###
#############################
#loading the data, it needs to be in one folder with nothing else in it
setwd("~/path to where you saved the folder/XXX")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)
hf05h21a1 <- bind_rows(df.list, .id = "id")
##need to run these too
hf05h21a1$rF=round(hf05h21a1$rF,1)
hf05h21a1$g1=round(hf05h21a1$g1,2)
hf05h21a1$g2=round(hf05h21a1$g2,2)
hf05h21a1$a2=round(hf05h21a1$a2,1)
hf05h21a1$aF=round(hf05h21a1$aF,1)

#The following equations form the output from our model's subfunctions
#delta, gamma12(gam1), gamma2F(gam2), thetaF and thetaS2 in order to 
#make percCons
hf05h21a1$delta=hf05h21a1$Feqm*(hf05h21a1$rF - hf05h21a1$alpF*hf05h21a1$Feqm)
hf05h21a1$gam1=( hf05h21a1$g1*hf05h21a1$S1eqm/(1 + hf05h21a1$alp1*(hf05h21a1$Feqm + .2*(hf05h21a1$S1eqm + hf05h21a1$S2eqm))) )
hf05h21a1$gam2=( hf05h21a1$g2*hf05h21a1$S2eqm/(1 + hf05h21a1$alp2*(hf05h21a1$Feqm + .2*(hf05h21a1$S2eqm))) )

hf05h21a1$thetaF= ((hf05h21a1$Heqm*.6*hf05h21a1$aF*hf05h21a1$Feqm)/(1+hf05h21a1$aF*hf05h21a1$hF*hf05h21a1$Feqm + hf05h21a1$a2*hf05h21a1$h2*hf05h21a1$S2eqm) )
hf05h21a1$thetaS2= ((hf05h21a1$Heqm*.6*hf05h21a1$a2*hf05h21a1$S2eqm)/(1+hf05h21a1$aF*hf05h21a1$hF*hf05h21a1$Feqm + hf05h21a1$a2*hf05h21a1$h2*hf05h21a1$S2eqm) )
hf05h21a1$thetas=hf05h21a1$thetaF+hf05h21a1$thetaS2

hf05h21a1$percCons=((hf05h21a1$thetaF/.6)+(hf05h21a1$thetaS2/.6))/(hf05h21a1$S1eqm+hf05h21a1$S2eqm+hf05h21a1$Feqm)

##############################
###alpha .1 h2=0.5, hF=0.5###
#############################
#loading the data, it needs to be in one folder with nothing else in it
setwd("~/path to where you saved the folder/XXX")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)
h05a1 <- bind_rows(df.list, .id = "id")
##need to run these too
h05a1$rF=round(h05a1$rF,1)
h05a1$g1=round(h05a1$g1,2)
h05a1$g2=round(h05a1$g2,2)
h05a1$a2=round(h05a1$a2,1)
h05a1$aF=round(h05a1$aF,1)

#The following equations form the output from our model's subfunctions
#delta, gamma12(gam1), gamma2F(gam2), thetaF and thetaS2 in order to 
#make percCons
h05a1$delta=h05a1$Feqm*(h05a1$rF - h05a1$alpF*h05a1$Feqm)
h05a1$gam1=( h05a1$g1*h05a1$S1eqm/(1 + h05a1$alp1*(h05a1$Feqm + .2*(h05a1$S1eqm + h05a1$S2eqm))) )
h05a1$gam2=( h05a1$g2*h05a1$S2eqm/(1 + h05a1$alp2*(h05a1$Feqm + .2*(h05a1$S2eqm))) )

h05a1$thetaF= ((h05a1$Heqm*.6*h05a1$aF*h05a1$Feqm)/(1+h05a1$aF*h05a1$hF*h05a1$Feqm + h05a1$a2*h05a1$h2*h05a1$S2eqm) )
h05a1$thetaS2= ((h05a1$Heqm*.6*h05a1$a2*h05a1$S2eqm)/(1+h05a1$aF*h05a1$hF*h05a1$Feqm + h05a1$a2*h05a1$h2*h05a1$S2eqm) )
h05a1$thetas=h05a1$thetaF+h05a1$thetaS2

h05a1$percCons=((h05a1$thetaF/.6)+(h05a1$thetaS2/.6))/(h05a1$S1eqm+h05a1$S2eqm+h05a1$Feqm)

##############################
#####alpha .1 h2=1, hF=1#####
#############################
#loading the data, it needs to be in one folder with nothing else in it
setwd("~/path to where you saved the folder/XXX")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)
h1a1 <- bind_rows(df.list, .id = "id")
##need to run these too
h1a1$rF=round(h1a1$rF,1)
h1a1$g1=round(h1a1$g1,2)
h1a1$g2=round(h1a1$g2,2)
h1a1$a2=round(h1a1$a2,1)
h1a1$aF=round(h1a1$aF,1)

#The following equations form the output from our model's subfunctions
#delta, gamma12(gam1), gamma2F(gam2), thetaF and thetaS2 in order to 
#make percCons
h1a1$delta=h1a1$Feqm*(h1a1$rF - h1a1$alpF*h1a1$Feqm)
h1a1$gam1=( h1a1$g1*h1a1$S1eqm/(1 + h1a1$alp1*(h1a1$Feqm + .2*(h1a1$S1eqm + h1a1$S2eqm))) )
h1a1$gam2=( h1a1$g2*h1a1$S2eqm/(1 + h1a1$alp2*(h1a1$Feqm + .2*(h1a1$S2eqm))) )

h1a1$thetaF= ((h1a1$Heqm*.6*h1a1$aF*h1a1$Feqm)/(1+h1a1$aF*h1a1$hF*h1a1$Feqm + h1a1$a2*h1a1$h2*h1a1$S2eqm) )
h1a1$thetaS2= ((h1a1$Heqm*.6*h1a1$a2*h1a1$S2eqm)/(1+h1a1$aF*h1a1$hF*h1a1$Feqm + h1a1$a2*h1a1$h2*h1a1$S2eqm) )
h1a1$thetas=h1a1$thetaF+h1a1$thetaS2

h1a1$percCons=((h1a1$thetaF/.6)+(h1a1$thetaS2/.6))/(h1a1$S1eqm+h1a1$S2eqm+h1a1$Feqm)


##Check the general stability of each subset##
percStable=
length(h05a1$StableB[h05a1$StableB==1])/length(h05a1$StableB)
length(hf05h21a1$StableB[hf05h21a1$StableB==1])/length(hf05h21a1$StableB)
length(hf1h205a1$StableB[hf1h205a1$StableB==1])/length(hf1h205a1$StableB)
length(h1a1$StableB[h1a1$StableB==1])/length(h1a1$StableB)

length(h05a06$StableB[h05a06$StableB==1])/length(h05a06$StableB)
length(hf05h21a06$StableB[hf05h21a06$StableB==1])/length(hf05h21a06$StableB)
length(hf1h205a06$StableB[hf1h205a06$StableB==1])/length(hf1h205a06$StableB)
length(h1a06$StableB[h1a06$StableB==1])/length(h1a06$StableB)

