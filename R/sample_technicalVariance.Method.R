##################################################################
##################################################################
##################################################################
# within sample variance computation
#################################################################
#################################################################
#################################################################

setMethod("sample_technicalVariance",signature=signature(Data="aclinicalProteomicsData"),

function(Data,...) {

rawData <- proteomicsExprsData(Data)

no.peaks <- Data@no.peaks

JUNK_DATA <- sampleClusteredData(rawData,no.peaks)

JUNK_DATA=negativeIntensitiesCorrection(JUNK_DATA)

names(JUNK_DATA) <- as.character(as.numeric(names(JUNK_DATA)))

# we use the log base 2 expression values

LOGDATA <- log2(JUNK_DATA) 

#Data=OBJECT

variableClass = Data@variableClass

PhenoInfo <- proteomicspData(Data)

#PhenoInfo <- phenoDataFrame(PhenoData, variableClass)

LOGDATA <- LOGDATA[,as.character(PhenoInfo$SampleTag)]


#`withinsampleVariance` <-
#function(LOG_DATA,PhenoInfo,Data,no.peaks,...) {

workingPheno<- rbind(PhenoInfo,PhenoInfo)

SampleTag <-  workingPheno$SampleTag[order(as.character(workingPheno$SampleTag))]

workingPheno<- workingPheno[order(as.character(workingPheno$SampleTag)),]

rownames(workingPheno)<- 1:dim(workingPheno)[1]

# split matrix into subsets

d <- dim(LOGDATA)

bin <- 2

#

proteinData <- split(as.matrix(LOGDATA[,unique(as.character(workingPheno$SampleTag))]),rep(seq(1, d[1]/bin), each=bin))

###################
# non confounder adjusted fit: this is the bit to be used in computing the within-replicate variances
VAR_MAT<- NULL


VAR <- NULL

for(i in 1:length(proteinData)) {

regressionData1<- data.frame(workingPheno,expression=proteinData[[i]])

out <- mixedModel2(expression~tumor, random=SampleTag, data=regressionData1)

VAR<- c(VAR,as.vector(out$varcomp[2]))

}

VAR_MAT<- VAR

mzMAT<- matrix(rawData$Substance.Mass,no.peaks,dim(rawData)[1])

mz<- round(apply(mzMAT,1,mean))

names(VAR_MAT)<- mz

withinsamplevariance<- VAR_MAT

VAR_MAT

}

)

########################################################################
########################################################################
######
######
########################################################################
########################################################################









