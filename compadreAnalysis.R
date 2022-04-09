##
#COMPADRE is an online repository for matrix population models (MPMs)
#and metadata on plants
#Please see https://jonesor.github.io/Rcompadre/index.html & 
# https://jonesor.github.io/CompadreGuides/user-guide.html
#for in-depth discussion of COMPADRE install and usage. 
##

##Install the stable release package from CRAN with:
install.packages("Rcompadre")
##Remote install from github with:
#remotes::install_github("jonesor/Rcompadre", build_vignettes = FALSE)
##
library(Rcompadre)
compadre <- cdb_fetch("compadre")
require(ggplot)

##Check the current distribution of different matrix structures in COMPADRE
table(compadre$MatrixDimension)

##Creating full species name column###
#Check number of species represented by 3 stages currently in COMPADRE
compadre$fullSpName=paste(compadre$Genus,compadre$Species,sep=" ")
length(unique(compadre$fullSpName[compadre$MatrixDimension==3]))
#107

#Check number of families represented by 3 stages currently in COMPADRE
length(unique(compadre$Family[compadre$MatrixDimension==3]))
#Check number of genera represented by 3 stages currently in COMPADRE
length(unique(compadre$Genus[compadre$MatrixDimension==3]))
#Check the break down of 3-stage entries in COMPADRE
unique(compadre$Family[compadre$MatrixDimension==3])
table(compadre$Family[compadre$MatrixDimension==3])

#Check number of continents and ecoregions currently 
#represented by 3 stages currently in COMPADRE
unique(compadre$Continent[compadre$MatrixDimension==3])
unique(compadre$Ecoregion[compadre$MatrixDimension==3])

#######################
#### 3 STAGE DATA #####
#######################
##Create a subset of only 3-stage matrices
mat3=subset(compadre, MatrixDimension==3)
##COMPADRE uses "accessor functions" to obtain specific parts of the matrices.
##matF accesses the fecundity part of the matrix, akin to our rF
Fmats=matF(mat3)
##matU accesses the survival-related transitions, akin to our g12 and g2F
Umats=matU(mat3)

#To store entry. Fs for reproduction alone, F.U for reproduction & transitions. 
Fs=rep(); F.U=matrix(0,1260,4)
for(i in 1:length(Fmats) ) {
	Fs[i]=sum(Fmats[i][[1]][1,3] )
	F.U[i,1:3]=Umats[i][[1]][lower.tri(Umats[i][[1]])] ##only take the transitions
	F.U[i,4]=Fs[i] #include reproduction
}
#make easy to use data frame for our currated data. 
#name columns in the vein of our parameters. Note, d31 named for the 
#addition into stage 1 from stage 3 is the same as our 
UnF=as.data.frame(F.U)
colnames(UnF)=c('g12','g13','g23','d31')
UnF=subset(UnF,UnF$d31>0&UnF$d31<100)

#################
##Scatter plots showing correlations between parameters
#################
require(gridExtra)
g12vrF=ggplot(UnF) + theme_bw() +
 geom_point(aes(x=g12,y=log(d31)),alpha=.4,size=2) +
 xlab(bquote('maturation rate'~(g['12'])) )+
 ylab(bquote('log(reproduction rate)'~(r[F])) )

g23vrF=ggplot(UnF) + theme_bw() +
 geom_point(aes(x=g23,y=log(d31)),alpha=.4,size=2) +
 xlab(bquote('maturation rate'~(g['2F'])) )+
 ylab(bquote('log(reproduction rate)'~(r[F])) )

g23vg12=ggplot(UnF) + theme_bw() +
 geom_point(aes(x=g23,y=g12),alpha=.4,size=2) +
 xlab(bquote('maturation rate'~(g['2F'])) )+
 ylab(bquote('germination rate'~(g['12'])) )

grid.arrange(g12vrF, g23vrF, g23vg12, ncol=3)

#################
##Histograms of the ranges for each parameter
#################
Fss=Fs[Fs<100&Fs>0]
h1<-hist(na.omit(Fss),breaks=80,col='light blue',xlim=c(0,10.6),plot=FALSE);
h1$density = h1$counts/sum(h1$counts)
h22<-hist(na.omit(UnF$g12[UnF$g12>0]),breaks=40,col='light blue',plot=FALSE);
h22$density = h22$counts/sum(h22$counts)
h3<-hist(na.omit(UnF$g23[UnF$g23>0]),breaks=40,col='light blue',plot=FALSE);
h3$density = h3$counts/sum(h3$counts)

par( mfrow= c(1,3) )
plot(h,freq=FALSE,col='light blue',xlim=c(0,10.6),
xlab="Measured Rate",ylab="Density",main="Reproduction into 1st Stage")#,ylim=c(0,.5))

plot(h22,freq=FALSE,col='light blue',
xlab="Measured Rate",ylab="Density",main="1st to 2nd Stage Transition")#,ylim=c(0,.5))

plot(h3,freq=FALSE,col='light blue',
xlab="Measured Rate",ylab="Density",main="2nd to 3rd Stage Transition")#,ylim=c(0,.5))
