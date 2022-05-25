########################
#SUPPLEMENTARY FIGURES##
########################
library(readxl)
library(readr)
library(dplyr)
library(tidymv)
library(ggplot2)
library(pROC) #for estimating AUC 
library(pls) #for partial least squares regression

##Use simDataLoader to load chosen herbivory subset. We use hF=h2=1 and 
##alpF=alp1=alp2=0.06 here. The file is labeled h1a06 below.  
##Code and figures describing COMPADRE analysis are 
##available in the COMPADRE section 

###############
###alpha .06, both h=1###
##############
#loading the data, it needs to be in one folder with nothing else in it
setwd("YOUR DIRECTORY/NEWESTFiveParamSweep-alp0.06-hF1-h21/")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)
h1a06 <- bind_rows(df.list, .id = "id")
##need to run these too
h1a06$rF=round(h1a06$rF,1)
h1a06$g1=round(h1a06$g1,2)
h1a06$g2=round(h1a06$g2,2)
h1a06$a2=round(h1a06$a2,1)
h1a06$aF=round(h1a06$aF,1)

h1a06$delta=h1a06$Feqm*(h1a06$rF - h1a06$alpF*h1a06$Feqm)
h1a06$gam1=( h1a06$g1*h1a06$S1eqm/(1 + h1a06$alp1*(h1a06$Feqm + .2*(h1a06$S1eqm + h1a06$S2eqm))) )
h1a06$gam2=( h1a06$g2*h1a06$S2eqm/(1 + h1a06$alp2*(h1a06$Feqm + .2*(h1a06$S2eqm))) )

h1a06$thetaF= ((h1a06$Heqm*.6*h1a06$aF*h1a06$Feqm)/(1+h1a06$aF*h1a06$hF*h1a06$Feqm + h1a06$a2*h1a06$h2*h1a06$S2eqm) )
h1a06$thetaS2= ((h1a06$Heqm*.6*h1a06$a2*h1a06$S2eqm)/(1+h1a06$aF*h1a06$hF*h1a06$Feqm + h1a06$a2*h1a06$h2*h1a06$S2eqm) )
h1a06$thetas=h1a06$thetaF+h1a06$thetaS2

h1a06$percCons=((h1a06$thetaF/.6)+(h1a06$thetaS2/.6))/(h1a06$S1eqm+h1a06$S2eqm+h1a06$Feqm)


	###########Fig S5###########
##Single stage herbivory on adult stage##
	##############################
stderr <- function(x) {if (length(x>1)) {sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))} else {0}}
CV <- function(x){
      (sd(x,na.rm=TRUE)/mean(as.numeric(x),na.rm=TRUE))*100
      }
df=subset(h1a06,aF==1&a2==0) %>%
  group_by(rF) %>%
  summarize(avgGam1=mean(gam1,na.rm=TRUE), sdGam1=sd(gam1,na.rm=TRUE),
  avgGam2=mean(gam2,na.rm=TRUE), sdGam2=sd(gam2,na.rm=TRUE),
  avgPerc=mean(percCons,na.rm=TRUE), sdPerc=sd(percCons,na.rm=TRUE) )

ggplot() + theme_bw() +
   geom_point(data=df,aes(x=rF,y=avgPerc,color='black')) + 
   geom_errorbar(data=df,aes(x=rF,y=avgPerc,ymin=avgPerc-sdPerc, ymax=avgPerc+sdPerc), width=.05, col='black') +
   geom_point(data=df,aes(x=rF,y=avgGam1,color='red')) + 
   geom_errorbar(data=df,aes(x=rF,y=avgGam1,ymin=avgGam1-sdGam1, ymax=avgGam1+sdGam1), width=.05, col='red') +
   geom_point(data=df,aes(x=rF,y=avgGam2,color='blue')) + 
   geom_errorbar(data=df,aes(x=rF,y=avgGam2,ymin=avgGam2-sdGam2, ymax=avgGam2+sdGam2), width=.05, col='blue') +
   xlab(expression(paste("Seed Prodct. Rate,", r[F]))) + ylab('Ecological Factor at Eqm') +
   theme(legend.position = c(0.18, 0.78)) +
   scale_color_manual(name=" Function", values=c("black","blue","red"),labels=c(expression(L["ratio"]),expression(gamma[12]),expression(gamma["2F"]) ) ) +
   theme(text = element_text(size=15))

	###########Fig S6###########
##Single stage herbivory on adult stage##
	##############################
qw=subset(h1a06,aF==1&a2==0)

###Fig S6a
ggplot(data=qw,aes(x=gam1+gam2,y=(Feqm/(Feqm+S2eqm+S1eqm)),col=g1+g2 ) ) + theme_bw() +
  xlab(expression(gamma[12]^"*"+gamma["2F"]^"*")) + ylab(bquote(F^"*"~"Ratio") ) + 
  geom_point() +
  #scale_color_discrete(name="  Stable" ,labels=c("No","Yes") ) +
  #scale_color_continuous(name=expression(" "~g[12]+g["2F"]) )+#,labels=c("No","Yes") ) +
  theme(legend.position = c(0.87, 0.76)) +
  scale_color_gradient2(name=expression(" "~g[12]+g["2F"]),midpoint=1,low="blue",mid="white",high="red", space ="Lab")

###Fig S6b
ggplot(data=qw,aes(x=gam1+gam2,y=(Feqm/(Feqm+S2eqm+S1eqm)),col=rF ) ) + theme_bw() +
  xlab(expression(gamma[12]^"*"+gamma["2F"]^"*")) + ylab(bquote(F^"*"~"Ratio") ) + 
  geom_point() +
  scale_color_continuous(name=expression(" "~r[F]) )+
  theme(legend.position = c(0.87, 0.76)) 

###Fig S6c
ggplot(data=qw,aes(x=gam1+gam2,y=(Feqm/(Feqm+S2eqm+S1eqm)),col=as.factor(StableB) ) ) + theme_bw() +
  xlab(expression(gamma[12]^"*"+gamma["2F"]^"*")) + ylab(bquote(F^"*"~"Ratio") ) + 
  geom_point() +
  scale_color_discrete(name="  Stable" ,labels=c("No","Yes") ) +
  theme(legend.position = c(0.87, 0.76))  

#######
#### This was not used in the publication, but is provided here for interested readers. 
#######
ggplot() + theme_bw() +
  xlab(expression(gamma[12]^"*"+gamma["2F"]^"*")) + ylab(bquote(L_Ratio) ) + ylim(0,.5)+
  #geom_point(data=subset(h1a06,aF==1&a2==0),aes(x=gam1+gam2,y=percCons,col=as.factor(StableB) )) +
  geom_point(data=subset(h1a06,aF==0.2&a2==1),aes(x=gam1+gam2,y=percCons,col=as.factor(StableB) )) +
  scale_color_discrete(name="  Stable" ,labels=c("No","Yes") ) +
  #scale_color_continuous(name=expression(" "~r[F]) )+#,labels=c("No","Yes") ) +
  theme(legend.position = c(0.15, 0.78)) 


        ###########Fig S7###########
##Made in Mathematica: Available upon request##
        ##############################


	###########Fig S8###########
##Comparing a2==1&aF==0.2 and a2==0&aF==1##
	##############################

##Create separate data frames for a2==1&aF==0.2 and a2==0&aF==1 subsets
df_a21aF02=subset(h1a06,a2==1&aF==0.2) %>%
  group_by(rF) %>% 
  summarize(avgPerc=mean(percCons ),sdPerc=sd(percCons ),avgGams=mean(gam1+gam2 ),sdGams=sd(gam1+gam2 ),
  avgRat=mean(percCons/(gam1+gam2) ),sdRat=sd(percCons/(gam1+gam2) ) )

df_a20aF1=subset(h1a06,a2==0&aF==1) %>%
  group_by(rF) %>% 
  summarize(avgPerc=mean(percCons ),sdPerc=sd(percCons ),avgGams=mean(gam1+gam2 ),sdGams=sd(gam1+gam2 ),
  avgRat=mean(percCons/(gam1+gam2) ),sdRat=sd(percCons/(gam1+gam2) ) )

df_a202aF1=subset(h1a06,a2==0.2&aF==1) %>%
  group_by(rF) %>% 
  summarize(avgPerc=mean(percCons ),sdPerc=sd(percCons ),avgGams=mean(gam1+gam2 ),sdGams=sd(gam1+gam2 ),
  avgRat=mean(percCons/(gam1+gam2) ),sdRat=sd(percCons/(gam1+gam2) ) )

df_plot <- bind_rows(df_a21aF02,df_a20aF1, .id = "id")
df_plot <- bind_rows(df_a21aF02,df_a202aF1,df_a20aF1, .id = "id")
View(df_plot)

##Plot each subset on the same plot for comparison
ggplot(data=df_plot,aes(x=rF,y=avgRat,col=id)) + theme_bw() + ylim(0,1.05)+ xlim(0.3,3.1) +
  xlab(expression(paste("Seed Prodct. Rate,", r[F]))) + ylab(expression(paste("L_ratio/", gamma[12]^"*"+gamma["2F"]^"*")) ) +
  geom_point() + theme(axis.text = element_text(size = 18)) +
  theme(axis.title = element_text(size = 18)) +
  geom_errorbar(aes(ymin=avgRat-sdRat, ymax=avgRat+sdRat), width=.2) +
  scale_color_discrete(name="  Subset" ,labels=c(expression(paste(a[2], "=1.0, ", a[F], "=0.2")),expression(paste(a[2], "=0.0, ", a[F], "=1.0"))) )  +
  theme(legend.position = c(0.18, 0.15))

ggplot(data=df_plot,aes(x=rF,y=avgRat,col=id)) + theme_bw() + ylim(0,1.05)+ xlim(0.3,3.1) +
  xlab(expression(paste("Seed Prodct. Rate,", r[F]))) + ylab(expression(paste("L_ratio/", gamma[12]^"*"+gamma["2F"]^"*")) ) + 
  geom_point() + theme(axis.text = element_text(size = 18)) +
  theme(axis.title = element_text(size = 18)) +
  geom_errorbar(aes(ymin=avgRat-sdRat, ymax=avgRat+sdRat), width=.2) +
  scale_color_discrete(name="  Subset" ,labels=c(expression(paste(a[2], "=1.0, ", a[F], "=0.2")), expression(paste(a[2], "=0.2, ", a[F], "=1.0")) , expression(paste(a[2], "=0.0, ", a[F], "=1.0"))) )  +
  theme(legend.position = c(0.18, 0.15))

	###########Fig S9###########
##Comparing a2==0.2&aF==1 and a2==0&aF==1##
	##############################
#make subset data frame for a2==0.2 & aF==1
temp=subset(h1a06,a2==0.2&aF==1)

##Fig showing relationship between model parameters g1 & g2 and the 
##ecological factors gamma12 & gamma2F
ggplot(temp) + theme_bw() + theme(text = element_text(size=14)) +
  geom_point(aes(x=g2-g1,y=gam2-gam1,col=(rF))) +
  scale_color_continuous(name=expression(" "~r[F]) ) +
  xlab(expression(paste(g["2F"]," - ",g[12]))) + ylab(expression(gamma["2F"]^"*" - gamma[12]^"*") ) +
  theme(legend.position = c(0.91, 0.17))

##Relationship between high gamma2F, low gamma12, and the percent of consumption
##focused on the seedlings. 
ggplot(temp) + theme_bw() + theme(text = element_text(size=14)) +
  geom_point(aes(x=(gam2-gam1),y=((thetaS2)/thetas),col=(rF))) +
  scale_color_continuous(name=expression(" "~r[F]) ) +
  xlab(expression(gamma["2F"]^"*" - gamma[12]^"*") ) + ylab("% consumption from seedling" ) +
  theme(legend.position = c(0.11, 0.17))

##Making binomial regression of stability as predicted by the percent of consumption
##focused on the seedlings.
temp$eatPercS2=((temp$thetaS2)/temp$thetas)
glmS2=glm(StableB~eatPercS2,data=temp,family=binomial)

#Make predictions
x0 <- seq(min(temp$eatPercS2), max(temp$eatPercS2), length = 100)  ## prediction grid
y0 <- predict.glm(glmS2, newdata = list(eatPercS2 = x0),type="response",se.fit=TRUE)  ## predicted values
plotdata=data.frame(x0,y0$fit); 
colnames(plotdata)=c("x0","fit");

# include CIs based on se
plotdata$LL <- plotdata$fit - y0$se.fit 
plotdata$UL <- plotdata$fit + y0$se.fit 

# plot predicted probabilities of stability and CI
ggplot(plotdata, aes(x0, fit)) + theme_bw() + theme(text = element_text(size=14)) +
  geom_ribbon(aes(ymin = LL, ymax = UL),alpha = 0.25) + 
  geom_line( size = 1.5) + 
  xlab("% Consumption from Seedling Stage" ) + ylab("Prob. of Stability")



	###########Fig S10###########
##Finding & displaying effects of ecological##
##factors across all herbivory allocations  ##
##for alphas set to 0.6, handling time on   ##
##seedlings set to 0.5, and handling time on##
##adult plants set to 1. 			  ##
	##############################
##Use simDataLoader to load the hf1h205a06 herbivory subset. 
##We have copied that code here for the convenience of the user:

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


##Set up data storing matrices
As=seq(0.0,2,.2); As=round(As,1);
compDF=expand.grid(a2=As, aF=As);
percCM=matrix(0,length(As),length(As)); gam1M=matrix(0,length(As),length(As)); gam2M=matrix(0,length(As),length(As));

##Run partial least squares regression for each herbivory allocation
for(i in 1:length(As) ) {
	for(j in 1:length(As) ) {
		temp=subset(hf1h205a06,aF==As[i]&a2==As[j])
		temp=data.frame(temp$StableB,temp$MaxEVal,temp$rF,temp$g1,temp$g2,temp$percCons,temp$gam1,temp$gam2);
		colnames(temp)=c('Stable','EV','rF','g1','g2','percCons','gam1','gam2'); temp$Stable=as.factor(temp$Stable);
		
		tempPLSmod=plsr(EV~percCons+gam1+gam2,data=temp)
		coefficients=coef(tempPLSmod)
		sum.coef = sum(sapply(coefficients, abs))
		coefficients = coefficients * 100 / sum.coef
		coefficients = sort(coefficients[, 1 , 1])
		#barplot(tail(coefficients, 5))

		percCM[i,j]=coefficients[['percCons']]
		gam1M[i,j]=coefficients[['gam1']]
		gam2M[i,j]=coefficients[['gam2']]
	}
}

#Create dataframe from matrices above:
compCoeff=expand.grid(a2=As, aF=As);
compCoeff$percCcoeff=c(percCM); compCoeff$gam1coeff=c(gam1M); compCoeff$gam2coeff=c(gam2M);

#Set the first entry to 0 as this represents results with no herbivory:
compCoeff$percCcoeff[1]=NA; compCoeff$gam1coeff[1]=NA; compCoeff$gam2coeff[1]=NA;

#Choose one ecological factor and plot its coefficients. To change the 
#factor dataframe in the "fill" category below:
ggplot(compCoeff, aes(as.factor(a2), as.factor(aF), fill= gam2coeff)) + 
  geom_tile() + theme_bw() + coord_fixed() +
  ylab(bquote('Attack Rate on S2 '~(a[2])~'') ) +
  xlab(bquote('Attack Rate on F '~(a[F])~'') )




