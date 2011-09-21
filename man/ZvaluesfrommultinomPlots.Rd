\name{ZvaluesfrommultinomPlots}
\alias{ZvaluesfrommultinomPlots}
\title{
A generic functon to plot Density of Z values from a simulation from a multinomial 
population using the balanced  and unbalanced  studies and a 3D representaion of the Z values
}
\description{
A functon to plot Density of Z values from a simulation from a multinomial 
population using the balanced  and unbalanced  studies and a 3D representaion of the Z values. 
These plots are useful as visual tools for the confounding effects. The median values are indicated on these
plots and these can be used as the consesus values of the effects of covariates in sample size calculations.
}
\usage{
ZvaluesfrommultinomPlots(nsim,nobs,proposeddesign,balanceddesign,...)
}
\arguments{
\item{nsim}{
number of simulations to be done
}
\item{nobs}{
number of multinomial observation
}
\item{proposeddesign}{
a numeric vector with four elements indicating the design weights
}
  \item{balanceddesign}{
a numeric vector with all the four elements being one, indicating equal weights
}
\item{\dots}{
other arguments
}
}
\details{
This function generates saples from a given four cell multinomial populaton then uses the
resulting multinomial probabilities in calculating the effect of covariates (Z values). Currently 
we implement a design arising
from a proteomics study in which there is a binary confounder and a binary exposure. 
The cross-tabulation of the categories of these covariates results into a 4-cell multinomial categories.
}
\value{
\item{density plot}{Density plot of the Z values}
\item{3D plot of Z values}{3D plot of the Z values against a two dimensional subspace of 
the 3-D space of multinomial probabilities}
}
\references{
Nyangoma SO, Ferreira JA, Collins SI, Altman DG, Johnson PJ, and
Billingham LJ: Sample size calculations for planning clinical
proteomic profiling studies using mass spectrometry.  (Working paper)
}
\author{
Stephen Nyangoma
}
\seealso{
Also see the function f.
}
\examples{
#density plots

nsim=10000;nobs=300;proposeddesign=c(1,2,1,7);balanceddesign=c(1,1,1,1)

f=function (x,y,z) {
Z=(1-x-z)*(x+y)/(2*(((1-x-z)*(1-x-y)*(1-y-z))-(1-x-y-z)^2))
Z
}

mul_1=rmultinom(nsim, nobs, prob = proposeddesign)/nobs
mul_1=t(mul_1)

mul_1=data.frame(mul_1)


 
names(mul_1)=c('x','y','z')


x=mul_1$x
y=mul_1$y

z=mul_1$z

# compute Z values (see Nyangoma et al. 2009)

Z=f(x,y,z)


x1=x
y1=y

z1=Z


#########################################
#####  pr=c(1,1,1,1)# balanced design
#########################################

mul_2=rmultinom(nsim, nobs, prob = balanceddesign)/nobs
mul_2=t(mul_2)


mul_2=data.frame(mul_2) 
names(mul_2)=c('x','y','z')

x=mul_2$x
y=mul_2$y

z=mul_2$z

Zb=f(x,y,z)

x2=x
y2=y

z2=Zb

#####################################
#####################################
#summary(Zb)
pdf('ZvaluesDensityPlots.pdf')
densityZ=density(Z,bw=0.1)
densityZb=density(Zb,bw=0.1)

plot(density(Zb,bw=0.1),xlim=c(min(c(densityZ$x,densityZb$x)),max(c(densityZ$x,densityZb$x))),
ylim=c(min(c(densityZ$y,densityZb$y)),max(c(densityZ$y,densityZb$y))),col='blue',lwd=2,lty=1,xlab='confounding effect - Z values',main='')


lines(density(Z,bw=0.1),lwd=2,xlab='',main='')

abline(v=median(Z),lty=2,col='red')
abline(v=median(Zb),lty=2,col='green')

legend(max(c(densityZ$x,densityZb$x)) - 2, max(c(densityZ$y,densityZb$y)) - 0.2, legend=c("blanced","unblanced"), col = c("blue","black"), lty = 1)
dev.off()
####################################
####################################

library(lattice)
library(rgl)
 library(scatterplot3d)
a=c(x1,x2)
 b=c(y1,y2)
 Z1=c(z1,z2)
 
group=c(rep(1,10000),rep(2,10000))

Data=data.frame(a,b,Z=Z1,group)

Data$group=as.factor(Data$group)

Plot3D=cloud(b ~ a*Z,scales = list(arrows = FALSE), data=Data, group=group,screen = list(x = 30, y = -60),ylim=c(0,15),zlim=c(0.05,0.45),xlim=c(0,0.45))

Plot3D
nsim=10000;nobs=300;proposeddesign=c(1,2,1,7);balanceddesign=c(1,1,1,1)
ZvaluesfrommultinomPlots(nsim,nobs,proposeddesign,balanceddesign)
}
\keyword{generic}