#####################
##MAIN TEXT FIGURES##
#####################
##Use simDataLoader to load chosen herbivory subset. We use hF=h2=1 and 
##alpF=alp1=alp2=0.06 here. The file is labeled h1a06 below.
##Random Forest specific figures are detailed & described in 
##Supplementary Info XXX


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ggrepel)
library(pls)

############# ----------------------------------------------- #############
# Figure 2: Figures a through f -----------------------------------------------

## Figure 2a: Variable Importance vs H-statistic (interactivity) 
##data.frames. Reruning the full dataset analysis takes very long. 
##So we have provided the RF outputs and H-statistic info here. 
params=c('rF','g12','g2F','a2','aF','rF','g12','g2F','aF')
AccDec=c(0.06187916,0.25017296,0.06086832,0.19885779,0.21016671,0.04421194,0.16133084,0.16096122,0.12333047);
GiniDec=c(14876.46,71219.25,15085.70,61129.33,61425.42,1323.030,3455.225,3548.774,3630.182);
Hstat=c(0.2589770,0.5268021,0.2731556,0.4170823,0.3609375,0.4281434,0.7663815,0.6863262,0.6024723);
Version=c("5","5","5","5","5","4","4","4","4")

DF=data.frame(params,Version,AccDec,GiniDec,Hstat); DF$params=as.factor(DF$params);
labelZ=c(bquote(r[F]),bquote(g[12]),bquote(g["2F"]),bquote(a[2]),bquote(a[F]),bquote(r[F]),bquote(g[12]),bquote(g["2F"]),bquote(a[F]))

#plot figure using ggplot
g<-ggplot(DF, aes(x=AccDec,y=Hstat,label=params,color=Version))+ 
   theme_bw() + coord_cartesian(ylim = c(0.15, .85)) + 
   geom_smooth(aes(color=Version),method='lm', formula= y~x,se=TRUE,alpha=0.2,) +
   geom_point(aes(color=Version),size=2) +
   xlab("Var. Imp.(Mean Acc. Decrease)")+
   ylab("H statistic") +
   #ggtitle("Information Value Summary") + 
   theme(legend.position = c(0.7, 0.10)) + #c(0.68, 0.2)
   theme(text = element_text(size=16)) + theme(legend.title=element_blank()) +
   scale_color_discrete(labels=c(expression(paste(a[2], "=0.0, ", a[F], ">0.0")) ,"all 5 parameters")) + guides(color = guide_legend(nrow = 1)) +
   geom_label_repel(
			aes(label = labelZ),
			parse=TRUE,
                  box.padding   = 0.35, 
                  point.padding = 0.5,
                  segment.color = 'grey50',show.legend = F)

#ggsave("Fig2a.png",g,width = 6.5,height = 3.5,dpi = 300)

## Figures 2b - 2f use the all_alp.1_VIvsH_2.csv data set. 
## load the data set. 
#VIvsH=read.csv(file = "~path to file/all_alp.1_VIvsH_2.csv")
VIvsH=read.csv(file = "C:/Users/prglaum/Documents/Stage-Structure/SimonEtAl2021/all_alp.1_VIvsH_2.csv")

## Figure 2b & 2c:...

##Prep...
VIvsHboxplot=data.frame(c(rep('rF',length(VIvsH$rFm)),rep('g2',length(VIvsH$g2m)),rep('g1',length(VIvsH$g1m)) ),
c(VIvsH$rFm,VIvsH$g2m,VIvsH$g1m),
c(VIvsH$rFh,VIvsH$g2h,VIvsH$g1h), 
c(VIvsH$alp,VIvsH$alp,VIvsH$alp),c(VIvsH$h2,VIvsH$h2,VIvsH$h2),c(VIvsH$hF,VIvsH$hF,VIvsH$hF),
c(VIvsH$a2,VIvsH$a2,VIvsH$a2),c(VIvsH$aF,VIvsH$aF,VIvsH$aF))
colnames(VIvsHboxplot)=c("params","VI","H","alp","h2","hF","a2","aF")

##Figure 2b: Plot...
Fig2b=ggplot(data=VIvsHboxplot,aes(params,VI)) + theme_bw() + theme(axis.text.x = element_text(size = 12)) +
  xlab('Parameters') + ylab('Variable Imp.') +
  geom_boxplot(fill='lightblue',outlier.alpha = 0.4) +
  scale_x_discrete(labels=c(bquote(''~g["12"]~''),bquote(''~g["2F"]~''),bquote(''~r["F"]~'')) )
#ggsave("Fig2b.png",Fig2b,width = 3.5,height = 3.5,dpi = 300)
##Figure 2c: Plot...
Fig2c=ggplot(data=VIvsHboxplot,aes(params,H)) + theme_bw() + theme(axis.text.x = element_text(size = 12)) +
  xlab('Parameters') + ylab('H Statistic') +
  geom_boxplot(fill='lightblue',outlier.alpha = 0.4) +
  scale_x_discrete(labels=c(bquote(''~g["12"]~''),bquote(''~g["2F"]~''),bquote(''~r["F"]~'')) )
#ggsave("Fig2c.png",Fig2c,width = 3.5,height = 3.5,dpi = 300)

## Figure 2d-2f: ...
## Fig 2d:
Fig2d=ggplot(subset(VIvsH,h2==0.5&hF==0.5), aes(as.factor(a2), as.factor(aF), fill= rFm)) + 
  geom_tile() + theme_bw() + coord_fixed() +
  xlab(bquote('Attack Rate on '~S[2]~' '~(a[2])~'') ) +
  ylab(bquote('Attack Rate on F '~(a[F])~'') ) +
  scale_fill_gradient(name=bquote('Var.\nImp.'~(r["F"])~'') )
#ggsave("Fig2d.png",Fig2d,width = 5.75,height = 3.5,dpi = 300)
## Fig 2e:
Fig2e=ggplot(subset(VIvsH,h2==0.5&hF==0.5), aes(as.factor(a2), as.factor(aF), fill= g1m)) + 
  geom_tile() + theme_bw() + coord_fixed() +
  xlab(bquote('Attack Rate on '~S[2]~' '~(a[2])~'') ) +
  ylab(bquote('Attack Rate on F '~(a[F])~'') ) +
  scale_fill_gradient(name=bquote('Var.\nImp.'~(g["12"])~'') ) 
#ggsave("Fig2e.png",Fig2e,width = 5.75,height = 3.5,dpi = 300)
## Fig 2f:
Fig2f=ggplot(subset(VIvsH,h2==0.5&hF==0.5), aes(as.factor(a2), as.factor(aF), fill= g2m)) + 
  geom_tile() + theme_bw() + coord_fixed() +
  xlab(bquote('Attack Rate on '~S[2]~' '~(a[2])~'') ) +
  ylab(bquote('Attack Rate on F '~(a[F])~'') ) +
  scale_fill_gradient(name=bquote('Var.\nImp.'~(g["2F"])~'') )
#ggsave("Fig2f.png",Fig2f,width = 5.75,,height = 3.5,dpi = 300) 


############# ----------------------------------------------- #############
# Figure 3: Threshold plots -----------------------------------------------
##Note, see random forest code for code for PD plots
g1s=sort(unique(h1a06_threshold$g1)); rFs=sort(unique(h1a06_threshold$rF)); g2s=sort(unique(h1a06_threshold$g2));
signChange=data.frame(sort(rep(rFs,length(g1s))),rep(g1s,length(rFs)),rep(NA,length(g1s)*length(rFs)),rep(NA,length(g1s)*length(rFs)))
colnames(signChange)=c('rF','g1','g2','direction')
for (r in 1:length(rFs) ) {
  df=subset(h1a06_threshold,rF==rFs[r]&a2==0&aF==1) %>% 
    group_by(g1,g2) %>%
    summarize(avg_ev = mean(MaxEVal,na.rm=TRUE) )
  
  for(i in 1:length(g1s) ) {
    if( sum(diff(sign(df$avg_ev[df$g1==g1s[i]]))>0) ) { #stable below line
      switchg2=g2s[which(diff(sign(df$avg_ev[df$g1==g1s[i]]))>0)] 
      signChange$g2[signChange$rF==rFs[r]&signChange$g1==g1s[i] ]=switchg2
      signChange$direction[signChange$rF==rFs[r]&signChange$g1==g1s[i] ]='stable below'
    } else if ( sum(diff(sign(df$avg_ev[df$g1==g1s[i]]))<0) ) { #unstable below line
      switchg2=g2s[which(diff(sign(df$avg_ev[df$g1==g1s[i]]))<0)] 
      signChange$g2[signChange$rF==rFs[r]&signChange$g1==g1s[i] ]=switchg2
      signChange$direction[signChange$rF==rFs[r]&signChange$g1==g1s[i] ]='unstable below'
    } else signChange$g2[signChange$rF==rFs[r]&signChange$g1==g1s[i] ]=NA; #if not switch, g2=1=out of bounds
  }
}

#Create the color palette for the graph
rbPal <- colorRampPalette(c("blue", "orange", "red"))
colors <- rbPal(14)[as.numeric(cut(unique(seq(.6,4.0,.2)),breaks = 14))]

#Producing the multi-line graph
ggplot( ) + theme_bw() + theme(text = element_text(size=20)) + ylim(.12,1) + xlim(.12,1) +#xlim(.12,.9)
  coord_cartesian(ylim=c(0.12,0.88),xlim=c(0.12,0.88)) +
  #xlab(expression(atop("Base Germination", paste("Rate g" [1])))) +  
  xlab(bquote('Base Germination Rate '~(g[1])~'') ) +
  ylab(bquote('Base Seedling Maturation Rate '~(g[2])~'') ) +
  geom_smooth(data=subset(signChange,rF==.4),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[1]) +
  geom_smooth(data=subset(signChange,rF==.6),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[2]) +
  geom_smooth(data=subset(signChange,rF==.8),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[3]) +
  geom_smooth(data=subset(signChange,rF==1.0),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[4]) +
  geom_smooth(data=subset(signChange,rF==1.2),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[5]) +
  geom_smooth(data=subset(signChange,rF==1.4),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[6]) +
  geom_smooth(data=subset(signChange,rF==1.6),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[7]) +
  geom_smooth(data=subset(signChange,rF==1.8),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[8]) +
  geom_smooth(data=subset(signChange,rF==2.0),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[9]) +
  geom_smooth(data=subset(signChange,rF==2.2),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[10]) +
  geom_smooth(data=subset(signChange,rF==2.4),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[11]) +
  geom_smooth(data=subset(signChange,rF==2.6),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[12]) + 
  geom_smooth(data=subset(signChange,rF==2.8),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[13]) +
  geom_smooth(data=subset(signChange,rF==3.0),aes(x=g1,y=g2),se=FALSE,span=.55,fullrange=TRUE,col=colors[14]) 

#Note: the "span" above controls how "wiggly" the line is, with lower numbers being more prone to 
#following every small change in the data

#The legend
legend_image <- rev(as.raster(matrix(rbPal(18), ncol=1)))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'rF')
text(x=1.5, y = seq(0,1,l=9), labels = seq(.4,3,l=9))
rasterImage(legend_image, 0, 0, 1,1)


############# ----------------------------------------------- #############
# Figure 4: Partial least squares -----------------------------------------
# Describe what these coefficients are...

##Figure 4a: 
matrix = matrix(NA,12,3)
colnames(matrix)=c('consumption','variable','coefficient')
counter = 1;
consumption = c("seedling-only","adult-only","seedling-dominant","adult-dominant")
aFs=c(0,1,0.2,1);
a2s=c(1,0,1,0.2);

for (i in c(1,2,3,4)) {
  plsmod=plsr(MaxEVal~percCons+gam1+gam2,data=subset(h1a06,aF==aFs[i]&a2==a2s[i]))
  ##Extract coefficients
  coefficients=coef(plsmod)
  ##Normalizing them
  sum.coef = sum(sapply(coefficients, abs))
  coefficients = coefficients * 100 / sum.coef
  coefficients = sort(coefficients[, 1 , 1])
  col1 = consumption[i]
  matrix[counter,]=c(col1,"gamma12",coefficients[c(2)])
  counter=counter+1;
  matrix[counter,]=c(col1,"gamma2F",coefficients[c(1)])
  counter=counter+1;
  matrix[counter,]=c(col1,"L:D ratio",coefficients[c(3)])
  counter=counter+1;
}

matrix=as.data.frame(matrix)
matrix$coefficient=as.numeric(matrix$coefficient)
matrix$coefficient=round(matrix$coefficient,2)

##This creates the two parameter bargraphs
ggplot(matrix, aes(x=factor(variable), y=coefficient,fill=factor(consumption))) + #ylim(-.007,.011)  +
  geom_bar(stat="identity", width=0.8,
           position=position_dodge(0.8)) +
  theme_bw() + theme(text = element_text(size=20)) +
  labs(x = "Coefficients by Variable", y = "Coefficients", fill="Variables") +
  theme(legend.direction = "horizontal") +
  theme(legend.text = element_text(colour="black", size = 11)) + 
  theme(legend.position="bottom")

##Figures 4b-4d
## Prep...
As=seq(0.0,2,.2); As=round(As,1);
compDF=expand.grid(a2=As, aF=As);
percCM=matrix(0,length(As),length(As)); gam1M=matrix(0,length(As),length(As)); gam2M=matrix(0,length(As),length(As));
for(i in 1:length(As) ) {
	for(j in 1:length(As) ) {
		temp=subset(h1a06,aF==As[i]&a2==As[j])
		temp=data.frame(temp$StableB,temp$MaxEVal,temp$rF,temp$g1,temp$g2,temp$LDratio,temp$gam1,temp$gam2);
		colnames(temp)=c('Stable','EV','rF','g1','g2','LDratio','gam1','gam2'); temp$Stable=as.factor(temp$Stable);
		
		tempPLSmod=plsr(EV~LDratio+gam1+gam2,data=temp)
		coefficients=coef(tempPLSmod)
		sum.coef = sum(sapply(coefficients, abs))
		coefficients = coefficients * 100 / sum.coef
		coefficients = sort(coefficients[, 1 , 1])

		percCM[i,j]=coefficients[['LDratio']]
		gam1M[i,j]=coefficients[['gam1']]
		gam2M[i,j]=coefficients[['gam2']]
	}
}

compCoeff=expand.grid(a2=As, aF=As);
compCoeff$percCcoeff=c(percCM); compCoeff$gam1coeff=c(gam1M); compCoeff$gam2coeff=c(gam2M);
compCoeff$percCcoeff[1]=NA; compCoeff$gam1coeff[1]=NA; compCoeff$gam2coeff[1]=NA;

## Plot...
Fig4b=ggplot(compCoeff, aes(as.factor(a2), as.factor(aF), fill= percCcoeff)) + 
  geom_tile() + theme_bw() + coord_fixed() +
  ylab(bquote('Attack Rate on '~S[2]~' '~(a[2])~'') ) +
  xlab(bquote('Attack Rate on F '~(a[F])~'') ) +
  scale_fill_gradient(name=bquote('  L:D Ratio\nRegression\n     Coeff.') )
#ggsave("Fig4b.png",Fig4b,width = 5.75,height = 3.5,dpi = 300)

Fig4c=ggplot(compCoeff, aes(as.factor(a2), as.factor(aF), fill= gam1coeff)) + 
  geom_tile() + theme_bw() + coord_fixed() +
  ylab(bquote('Attack Rate on '~S[2]~' '~(a[2])~'') ) +
  xlab(bquote('Attack Rate on F '~(a[F])~'') ) +
  scale_fill_gradient(name=expression(atop(gamma[12], "Regression\n     Coeff.")) )
#ggsave("Fig4c.png",Fig4c,width = 5.75,height = 3.5,dpi = 300)

Fig4d=ggplot(compCoeff, aes(as.factor(a2), as.factor(aF), fill= gam2coeff)) + 
  geom_tile() + theme_bw() + coord_fixed() +
  ylab(bquote('Attack Rate on '~S[2]~' '~(a[2])~'') ) +
  xlab(bquote('Attack Rate on F '~(a[F])~'') ) +
  scale_fill_gradient(name=expression(atop(gamma["2F"], "Regression\n     Coeff.")) )
#ggsave("Fig4d.png",Fig4d,width = 5.75,height = 3.5,dpi = 300)
