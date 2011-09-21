`ZvaluesfrommultinomPlots`<-
 
function (nsim,nobs,proposeddesign,balanceddesign,...) {

#########################################################
######
###### Proposed design
######
#########################################################


mul_1=rmultinom(nsim, nobs, prob = proposeddesign)/nobs
mul_1=t(mul_1)

mul_1=data.frame(mul_1)


 
names(mul_1)=c('x','y','z')


x=mul_1$x
y=mul_1$y

z=mul_1$z

# compute Z values (see Nyangoma et al. 2009)
f <- function (x, y, z) 
{
    Z = (1 - x - z) * (x + y)/(2 * (((1 - x - z) * (1 - x - y) * 
        (1 - y - z)) - (1 - x - y - z)^2))
    Z
}

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

#x11()
###########################
# 3D plot
###########################

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

pdf('Zvalues3DPlots.pdf')
print(Plot3D)
dev.off()

Zvalue=median(Z)
varZ=var(Z)

##################
##################


list(Zvalue=Zvalue,varZ=varZ)


}

