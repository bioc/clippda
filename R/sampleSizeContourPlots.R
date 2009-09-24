sampleSizeContourPlots <-
function(Z,m,DIFF,VAR,beta,alpha,observedPara,Indicator) {

pdf('SampleSizeContourPlot.pdf')

#########################################
#########################################
##### rho=0.70 beta=0.70 alpha=0.05 #####
#########################################
#########################################

Corr= 0.70

j=3

i=3

bj = beta[j]

ai=alpha[i]

f <- function(DIFF,VAR) { Ssize=(Z*(qnorm(ai)+qnorm(bj))^2*VAR*(1+(m-1)*Corr))/(m*(DIFF^2)) }
z <- outer(DIFF, VAR, f)

dimnames(z) <- list(DIFF, VAR)

s3d.dat1 <- data.frame(variance=as.vector(col(z)),parameters=as.vector(row(z)),
      value=as.vector(z))

s3d.dat <- data.frame(variance=rep(VAR,each=41),difference=rep(DIFF,39),
     no.samples=as.vector(as.matrix(z)))

s3d.datNEW=s3d.dat[!(s3d.dat$difference %in% unique(s3d.dat$difference)[1:10]) & !(s3d.dat$variance %in% unique(s3d.dat$variance)[20:40]),]
 s3d.datNEWSELECTED=matrix(s3d.datNEW[order(s3d.datNEW$difference),]$no.samples,19,31)
dimnames(s3d.datNEWSELECTED)=list(unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference))

par(mfrow=c(2, 2), cex=0.55, mar=c(3.9, 3.9, 3, 2), mex=0.8)

contour(x=unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),
y=unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference),z=s3d.datNEWSELECTED,ylab='difference',xlab='variance')

abline(v=unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),col='gray',lty=2)
abline(h=unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference),col='gray',lty=2)

if(is.null(dim(observedPara))) points(observedPara[1],observedPara[2],col=1,pch=24)  else {for(i in 1:dim(observedPara)[1]) points(as.vector(t(observedPara[i,]))[1],as.vector(t(observedPara[i,]))[2],col=i,pch=24)}


text(0.25,0.49,"(a)"~alpha)

text(0.47,0.49," = 0.05,")

text(0.64,0.49,~beta )

text(0.77,0.49," = 0.30")

text(0.29,0.47,~rho)

text(0.47,0.47," = 0.70")

if(Indicator == 1) {
points(1.8,0.34,pch=16,col='blue')

points(1.6, 0.26,   col="red",  pch=16) #maldi urine 

points(0.8, 0.39,  col="green",  pch=16)  # late stage maldi

points(1.6, 0.24,   col="dark green",  pch=16) # maldi-ds


# colorectal serum

points(0.7, 0.37,  col="purple",  pch=16) # imac

points(1.5, 0.43,  col="gray",  pch=16) #cm10

points(0.5, 0.33, col="orange", pch=16) # q10

}

#########################################
##### rho=0.70 beta=0.90 alpha=0.05 #####
#########################################

Corr=0.70

j=1

i=3

bj = beta[j]

ai=alpha[i]

f <- function(DIFF,VAR) { Ssize=(Z*(qnorm(ai)+qnorm(bj))^2*VAR*(1+(m-1)*Corr))/(m*(DIFF^2)) }
z <- outer(DIFF, VAR, f)

dimnames(z) <- list(DIFF, VAR)

s3d.dat1 <- data.frame(variance=as.vector(col(z)),parameters=as.vector(row(z)),
      value=as.vector(z))

s3d.dat <- data.frame(variance=rep(VAR,each=41),difference=rep(DIFF,39),
     no.samples=as.vector(as.matrix(z)))

s3d.datNEW=s3d.dat[!(s3d.dat$difference %in% unique(s3d.dat$difference)[1:10]) & !(s3d.dat$variance %in% unique(s3d.dat$variance)[20:40]),]
 s3d.datNEWSELECTED=matrix(s3d.datNEW[order(s3d.datNEW$difference),]$no.samples,19,31)
dimnames(s3d.datNEWSELECTED)=list(unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference))

contour(x=unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),
y=unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference),z=s3d.datNEWSELECTED,ylab='difference',xlab='variance'
)

abline(v=unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),col='gray',lty=2)
abline(h=unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference),col='gray',lty=2)

if(is.null(dim(observedPara))) points(observedPara[1],observedPara[2],col=1,pch=24)  else {for(i in 1:dim(observedPara)[1]) points(as.vector(t(observedPara[i,]))[1],as.vector(t(observedPara[i,]))[2],col=i,pch=24)}

text(0.25,0.49,"(b)"~alpha)

text(0.47,0.49," = 0.05,")

text(0.64,0.49,~beta )

text(0.77,0.49," = 0.10")

text(0.29,0.47,~rho)

text(0.47,0.47," = 0.70")


if(Indicator == 1) {

points(1.8,0.34,pch=16,col='blue')

points(1.6, 0.26,   col="red",  pch=16) #maldi urine 

points(0.8, 0.39,  col="green",  pch=16)  # late stage maldi

points(1.6, 0.24,   col="dark green",  pch=16) # maldi-ds

# colorectal serum

points(0.7, 0.37,  col="purple",  pch=16) # imac

points(1.5, 0.43,  col="gray",  pch=16) #cm10

points(0.5, 0.33, col="orange", pch=16) # q10

}

#########################################
##### rho=0.90 beta=0.70 alpha=0.05 #####
#########################################

Corr=0.90

j=3

i=3


bj = beta[j]

ai=alpha[i]


f <- function(DIFF,VAR) { Ssize=(Z*(qnorm(ai)+qnorm(bj))^2*VAR*(1+(m-1)*Corr))/(m*(DIFF^2)) }
z <- outer(DIFF, VAR, f)

dimnames(z) <- list(DIFF, VAR)

s3d.dat1 <- data.frame(variance=as.vector(col(z)),parameters=as.vector(row(z)),
      value=as.vector(z))

s3d.dat <- data.frame(variance=rep(VAR,each=41),difference=rep(DIFF,39),
     no.samples=as.vector(as.matrix(z)))

s3d.datNEW=s3d.dat[!(s3d.dat$difference %in% unique(s3d.dat$difference)[1:10]) & !(s3d.dat$variance %in% unique(s3d.dat$variance)[20:40]),]
 s3d.datNEWSELECTED=matrix(s3d.datNEW[order(s3d.datNEW$difference),]$no.samples,19,31)
dimnames(s3d.datNEWSELECTED)=list(unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference))


contour(x=unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),
y=unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference),z=s3d.datNEWSELECTED,ylab='difference',xlab='variance')

abline(v=unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),col='gray',lty=2)
abline(h=unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference),col='gray',lty=2)

if(is.null(dim(observedPara))) points(observedPara[1],observedPara[2],col=1,pch=24)  else {for(i in 1:dim(observedPara)[1]) points(as.vector(t(observedPara[i,]))[1],as.vector(t(observedPara[i,]))[2],col=i,pch=24)}

text(0.25,0.49,"(c)"~alpha)

text(0.47,0.49," = 0.05,")

text(0.64,0.49,~beta )

text(0.77,0.49," = 0.30")

text(0.29,0.47,~rho)

text(0.47,0.47," = 0.90")

if(Indicator == 1) {
points(1.8,0.34,pch=16,col='blue')

points(1.6, 0.26,   col="red",  pch=16) #maldi urine 

points(0.8, 0.39,  col="green",  pch=16)  # late stage maldi

points(1.6, 0.24,   col="dark green",  pch=16) # maldi-ds

# colorectal serum

points(0.7, 0.37,  col="purple",  pch=16) # imac

points(1.5, 0.43,  col="gray",  pch=16) #cm10

points(0.5, 0.33, col="orange", pch=16) # q10

}
#########################################
##### rho=0.90 beta=0.90 alpha=0.05 #####
#########################################

Corr=0.90

j=1

i=3


bj = beta[j]

ai=alpha[i]


f <- function(DIFF,VAR) { Ssize=(Z*(qnorm(ai)+qnorm(bj))^2*VAR*(1+(m-1)*Corr))/(m*(DIFF^2)) }
z <- outer(DIFF, VAR, f)

dimnames(z) <- list(DIFF, VAR)

s3d.dat1 <- data.frame(variance=as.vector(col(z)),parameters=as.vector(row(z)),
      value=as.vector(z))

s3d.dat <- data.frame(variance=rep(VAR,each=41),difference=rep(DIFF,39),
     no.samples=as.vector(as.matrix(z)))

s3d.datNEW=s3d.dat[!(s3d.dat$difference %in% unique(s3d.dat$difference)[1:10]) & !(s3d.dat$variance %in% unique(s3d.dat$variance)[20:40]),]
 s3d.datNEWSELECTED=matrix(s3d.datNEW[order(s3d.datNEW$difference),]$no.samples,19,31)
dimnames(s3d.datNEWSELECTED)=list(unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference))

contour(x=unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),
y=unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference),z=s3d.datNEWSELECTED,ylab='difference',xlab='variance'
)

abline(v=unique(s3d.datNEW[order(s3d.datNEW$difference),]$variance),col='gray',lty=2)
abline(h=unique(s3d.datNEW[order(s3d.datNEW$difference),]$difference),col='gray',lty=2)

if(is.null(dim(observedPara))) points(observedPara[1],observedPara[2],col=1,pch=24)  else {for(i in 1:dim(observedPara)[1]) points(as.vector(t(observedPara[i,]))[1],as.vector(t(observedPara[i,]))[2],col=i,pch=24)}

text(0.25,0.49,"(d)"~alpha)

text(0.47,0.49," = 0.05,")

text(0.64,0.49,~beta )

text(0.77,0.49," = 0.10")

text(0.29,0.47,~rho)

text(0.47,0.47," = 0.90")

if(Indicator==1) {
points(1.8,0.34,pch=16,col='blue')

points(1.6, 0.26,   col="red",  pch=16) #maldi urine 

points(0.8, 0.39,  col="green",  pch=16)  # late stage maldi

points(1.6, 0.24,   col="dark green",  pch=16) # maldi-ds


# colorectal serum

points(0.7, 0.37,  col="purple",  pch=16) # imac

points(1.5, 0.43,  col="gray",  pch=16) #cm10

points(0.5, 0.33, col="orange", pch=16) # q10

}

mtext(paste("Sample size contours"), side=3, line=1, outer=TRUE)

dev.off()

}

