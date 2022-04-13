########################
#SUPPLEMENTARY FIGURES##

##a2=0,aF=1 fig
qw=subset(h1a06,aF==1&a2==0)
qw2=subset(h1a06,aF==0.2&a2==1)
plot(qw$gam1+qw$gam2,qw$Feqm/(qw$Feqm+qw$S2eqm+qw$S1eqm) )

boxplot(qw$percCons/(qw$gam1+qw$gam2),qw2$percCons/(qw2$gam1+qw2$gam2))
plot(qw$rF,qw$percCons/(qw$gam1+qw$gam2))
plot(qw2$rF,qw2$percCons/(qw2$gam1+qw2$gam2))

ggplot(data=qw,aes(x=gam1+gam2,y=(Feqm/(Feqm+S2eqm+S1eqm)),col=g1+g2 ) ) + theme_bw() +
  xlab(expression(gamma[12]^"*"+gamma["2F"]^"*")) + ylab(bquote(F^"*"~"Ratio") ) + 
  geom_point() +
  #scale_color_discrete(name="  Stable" ,labels=c("No","Yes") ) +
  #scale_color_continuous(name=expression(" "~g[12]+g["2F"]) )+#,labels=c("No","Yes") ) +
  theme(legend.position = c(0.87, 0.76)) +
  scale_color_gradient2(name=expression(" "~g[12]+g["2F"]),midpoint=1,low="blue",mid="white",high="red", space ="Lab")

ggplot(data=qw,aes(x=gam1+gam2,y=percCons,col=rF ) ) + theme_bw() +
  xlab(expression(gamma[12]^"*"+gamma["2F"]^"*")) + ylab(bquote(L_Ratio) ) + 
  geom_point() +
  #scale_color_discrete(name="  Stable" ,labels=c("No","Yes") ) +
  scale_color_continuous(name=expression(" "~r[F]) )+#,labels=c("No","Yes") ) +
  theme(legend.position = c(0.15, 0.78)) 

ggplot(data=qw,aes(x=gam1+gam2,y=percCons,col=StableB ) ) + theme_bw() +
  xlab(expression(gamma[12]^"*"+gamma["2F"]^"*")) + ylab(bquote(L_Ratio) ) + 
  geom_point() +
  scale_color_discrete(name="  Stable" ,labels=c("No","Yes") ) +
  #scale_color_continuous(name=expression(" "~r[F]) )+#,labels=c("No","Yes") ) +
  theme(legend.position = c(0.15, 0.78)) 

####
ggplot() + theme_bw() +
  xlab(expression(gamma[12]^"*"+gamma["2F"]^"*")) + ylab(bquote(L_Ratio) ) + ylim(0,.5)+
  #geom_point(data=subset(h1a06,aF==1&a2==0),aes(x=gam1+gam2,y=percCons,col=as.factor(StableB) )) +
  geom_point(data=subset(h1a06,aF==0.2&a2==1),aes(x=gam1+gam2,y=percCons,col=as.factor(StableB) )) +
  scale_color_discrete(name="  Stable" ,labels=c("No","Yes") ) +
  #scale_color_continuous(name=expression(" "~r[F]) )+#,labels=c("No","Yes") ) +
  theme(legend.position = c(0.15, 0.78)) 

qw=subset(h1a06,a2==1&aF==0.2)
plot(qw$gam1,qw$MaxEVal)

df_a21aF02=subset(h1a06,a2==1&aF==0.2) %>%
  group_by(rF) %>% 
  summarize(avgPerc=mean(percCons ),sdPerc=sd(percCons ),avgGams=mean(gam1+gam2 ),sdGams=sd(gam1+gam2 ),
  avgRat=mean(percCons/(gam1+gam2) ),sdRat=sd(percCons/(gam1+gam2) ) )

df_a20aF1=subset(h1a06,a2==0&aF==1) %>%
  group_by(rF) %>% 
  summarize(avgPerc=mean(percCons ),sdPerc=sd(percCons ),avgGams=mean(gam1+gam2 ),sdGams=sd(gam1+gam2 ),
  avgRat=mean(percCons/(gam1+gam2) ),sdRat=sd(percCons/(gam1+gam2) ) )

plot(df_a21aF02$rF,df_a21aF02$avgGams,pch=20,xlim=c(0.4,3))#,ylim=c(0,0.8))
points(df_a20aF1$rF,df_a20aF1$avgGams,col='red',pch=20)

df_plot <- bind_rows(df_a21aF02,df_a20aF1, .id = "id")
View(df_plot)

ggplot(data=df_plot,aes(x=rF,y=avgRat,col=id)) + theme_bw() + ylim(0,1.05)+ xlim(0.3,3.1) +
  xlab(expression(paste("Seed Prodct. Rate,", r[F]))) + ylab(expression("L_ratio/"+gamma[12]^"*"+gamma["2F"]^"*") ) + 
  geom_point() + theme(axis.text = element_text(size = 18)) +
  theme(axis.title = element_text(size = 18)) +
  geom_errorbar(aes(ymin=avgRat-sdRat, ymax=avgRat+sdRat), width=.2) +
  scale_color_discrete(name="  Subset" ,labels=c(expression(paste(a[2], "=1.0, ", a[F], "=0.2")),expression(paste(a[2], "=0.0, ", a[F], "=1.0"))) )  +
  theme(legend.position = c(0.18, 0.15))
