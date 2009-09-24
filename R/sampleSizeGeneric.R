##################################################################
##################################################################
##################################################################
# sample size calculations
#################################################################
#################################################################
#################################################################

setGeneric("sampleSize",

function(Data, intraclasscorr, signifcut, ...) 

{

#sampleSize(Corr, techVar, bioVar, DIFF, Z, k, n, m)

#sampleSizeParameters(Data, intraclasscorr, signifcut)

no.peaks= Data@no.peaks 


#showMethods("sampleSize")
RESULTS <- sampleSizeParameters(Data, intraclasscorr, signifcut)

Corr <- RESULTS$Corr
techVar <- RESULTS$techVar
bioVar <- RESULTS$bioVar
DIFF <- RESULTS$DIFF

k <- length(Data@covariates) + 1 #no. parameters

n <- no.peaks #53

m <- 2  # no. technical replicates

# Z is the second diagonal element of  the output matrix from the FisherInformation 
#function, i.e. the expected Fisher Information

Z <- as.vector(fisherInformation(Data)[2,2])/2 # or chosee in the range 2.0 - 3.0 e.g. 2.4

##sampleSize(Corr, techVar, bioVar, DIFF, Z, k, n, m)

#####
#`sampleSize` <-
#function(Corr,techVar,bioVar,DIFF,Z,k ,n,m=2) {

beta=c(0.90,0.80,0.70)

alpha = 1 - c(0.001, 0.01,0.05)/2

SAMPLERSIZE1MAT=NULL

VARMAT=NULL

SAMPLERSIZE2MAT=NULL


for(j in 1:length(beta)) {

   bj = beta[j]
	
        #########################
	SAMPLERSIZE1=NULL
	VAR=NULL
	SAMPLERSIZE2=NULL
        #########################


     for(i in 1:length(alpha)) {

         ai=alpha[i]
         
           VAR=(((n-k)*bioVar/(1+(m-1)*Corr))+(n*(m-1)*techVar/(1-Corr)))/((n*m) - k)

           SAMPLERSIZE1=c(SAMPLERSIZE1,round((2*(qnorm(ai)+qnorm(bj))^2*VAR*(1+(m-1)*Corr))/(m*(DIFF^2))))

             
   SAMPLERSIZE2=c(SAMPLERSIZE2,round((Z*(qnorm(ai)+qnorm(bj))^2*VAR*(1+(m-1)*Corr))/(m*(DIFF^2))))

}

SAMPLERSIZE1MAT=rbind(SAMPLERSIZE1MAT,SAMPLERSIZE1)

VARMAT=rbind(VARMAT,VAR)

SAMPLERSIZE2MAT=rbind(SAMPLERSIZE2MAT,SAMPLERSIZE2)

}


VAR = round(VAR,1)

rownames(SAMPLERSIZE1MAT) = 1:dim(SAMPLERSIZE1MAT)[1]

rownames(SAMPLERSIZE2MAT) = 1:dim(SAMPLERSIZE2MAT)[1]

SAMPLERSIZE1MAT = data.frame(SAMPLERSIZE1MAT)

SAMPLERSIZE2MAT = data.frame(SAMPLERSIZE2MAT)

alphaName = c('alpha0.001', 'alpha0.01','alpha0.05')

betaName = c('beta0.1', 'beta0.2', 'beta0.3')

names(SAMPLERSIZE1MAT) = alphaName

row.names(SAMPLERSIZE1MAT) = betaName


names(SAMPLERSIZE2MAT) = alphaName


row.names(SAMPLERSIZE2MAT) = betaName


list(protein_variance=VAR, replicate_correlation=Corr, difference=DIFF,  sample_size=cbind(SAMPLERSIZE1MAT,SAMPLERSIZE2MAT))

standardGeneric("sampleSize")
}

)

########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
#######################################################################