ZvaluescasesVcontrolsPlots <-
function(probs) {

x=probs
y=1-probs
mat1=data.frame(x,y)
mat2=mat1[!(mat1$x ==0 | mat1$y ==0),]

zmat=data.frame(mat2,z=ztwo(mat2$x,mat2$y),prop=mat2$x/mat2$y )
zmat1=zmat[zmat$prop < 1 & zmat$z <= 4,]

pdf('ZvaluescasesVcontrolsPlots.pdf')

plot(zmat1$prop,zmat1$z,xlim=c(0,5),ylim=c(2,4),type='l',lty=2,
xlab='odds of being a case',ylab='adjustment for covariates - Z',lwd=2)
lines(1/zmat1$prop,zmat1$z,type='l',lty=2,lwd=2)

points(1,2,col='red',pch=17, cex = 1.5,lwd=2)


p1=1/4
po=1-p1
ztwo(po,p1)
ztwo(p1,po)
p1/po
po/p1
points(0.3333333,2.666667,col='green',pch=15, cex = 1.5,lwd=2)
points(3,2.666667,col='green',pch=15, cex = 1.5,lwd=2)
 

p1=1/5
po=1-p1
ztwo(po,p1)
ztwo(p1,po)
p1/po
po/p1
points(0.25,3.125,col='blue',pch=16, cex = 1.5,lwd=2)
points(4,3.125,col='blue',pch=16, cex = 1.5,lwd=2)

p1=1/6
po=1-p1
ztwo(po,p1)
ztwo(p1,po)
p1/po
po/p1

points(0.2,3.6,col='black',pch=18, cex = 1.5,lwd=2)
points(5,3.6,col='black',pch=18, cex = 1.5,lwd=2)
 

legend(1,3.8, c('1:1','1:3','1:4','1:5'), pch=c(17,15,16,18), pt.bg="white", 
lty=2, col = c('red','green',"blue",'black'),title ='ratio of cases to controls',cex=0.9)

dev.off()

}

