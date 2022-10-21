
# Load packages -----------------------------------------------------------

library(ggplot2)
library(pls)

# Figure 2: Threshold plots -----------------------------------------------

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
rbPal <- colorRampPalette(c("blue", "orange", "red"))#, in this case, red is the higher value...I feel confused?
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

# Figure 4: Partial least squares -----------------------------------------

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
  matrix[counter,]=c(col1,"gam1",coefficients[c(2)])
  counter=counter+1;
  matrix[counter,]=c(col1,"gam2",coefficients[c(1)])
  counter=counter+1;
  matrix[counter,]=c(col1,"percCons",coefficients[c(3)])
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

