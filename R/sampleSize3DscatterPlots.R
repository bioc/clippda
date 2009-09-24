sampleSize3DscatterPlots <-
function(Z,m,DIFF,VAR,beta,alpha,observedDIFF,observedVAR,observedSampleSize,Angle,Indicator) {

Corr= 0.70

j=3

i=3

bj = beta[j]

ai=alpha[i]


pdf('SampleSize3DscatterPlot.pdf')

f <- function(DIFF,VAR) { Ssize=(Z*(qnorm(ai)+qnorm(bj))^2*VAR*(1+(m-1)*Corr))/(m*(DIFF^2)) }

z <- outer(DIFF, VAR, f)

dimnames(z) <- list(DIFF, VAR)

s3d.dat1 <- data.frame(variance=as.vector(col(z)),parameters=as.vector(row(z)), value=as.vector(z))

s3d.dat <- data.frame(variance=rep(VAR,each=41),difference=rep(DIFF,39),
     no.samples=as.vector(as.matrix(z)))

s3dplot=scatterplot3d(s3d.dat[!(s3d.dat$difference %in% unique(s3d.dat$difference)[1:10]) & !(s3d.dat$variance %in% unique(s3d.dat$variance)[20:40]),], 
type="h", lwd=2, pch=" ", zlim=c(0,800),angle=Angle)

########
s3dplot$points3d(observedVAR,observedDIFF,  observedSampleSize,
             col="maroon", type="h", pch=24) # from the pilot data

if(Indicator==1) {

s3dplot$points3d(1.8,       0.34,  201,
             col="blue", type="h", pch=16) # seldi urine

s3dplot$points3d(1.6,       0.26,  297,
             col="red", type="h", pch=16) #maldi urine 

s3dplot$points3d(0.8,       0.39,  73,
             col="green", type="h", pch=16)  # late stage maldi

s3dplot$points3d(1.6,       0.24,  400,
           col="dark green", type="h", pch=16) # maldi-ds - late stage

# colorectal serum

s3dplot$points3d(0.7,       0.37,  60,
             col="purple", type="h", pch=16) # imac

s3dplot$points3d(1.5,       0.43,  103,
             col="gray", type="h", pch=16) #cm10

s3dplot$points3d(0.5,       0.33,66,
             col="orange", type="h", pch=16) # q10

########
s3dplot$points3d(3.4,       0.42,  271,
             col="maroon", type="h", pch=16) # dab fish
}

dev.off()

}

