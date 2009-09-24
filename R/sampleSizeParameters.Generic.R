##################################################################
##################################################################
##################################################################
# sample size parametes
#################################################################
#################################################################
#################################################################

setGeneric("sampleSizeParameters",
#`sampleSizeParameters` <-
function(Data,intraclasscorr,signifcut,...) {

##################################
##################################
rawData <- proteomicsExprsData(Data)

no.peaks <- Data@no.peaks

JUNK_DATA <- sampleClusteredData(rawData,no.peaks)

JUNK_DATA=negativeIntensitiesCorrection(JUNK_DATA)
names(JUNK_DATA) <- as.character(as.numeric(names(JUNK_DATA)))

# we use the log base 2 expression values

LOGDATA <- log2(JUNK_DATA) 

#Data=OBJECT

variableClass = Data@variableClass

PhenoData <- data.frame(Data@phenotypicData)

PhenoInfo <- phenoDataFrame(PhenoData, variableClass)

LOGDATA <- LOGDATA[,as.character(PhenoInfo$SampleTag)]

##################################
##################################

COR <- replicateCorrelations(Data)$all.correlations

within <- sample_technicalVariance(Data)


betweenSampleVarianceRESULTS=betweensampleVariance(Data)

betweenSampleVariance <- betweenSampleVarianceRESULTS$betweensamplevariance


parameter <- betweenSampleVarianceRESULTS$differences


# SIGNIFICANCE is a data frame contating results of differential expression analysis

significance <- betweenSampleVarianceRESULTS$significance

mz <- row.names(significance)

names(COR) <- mz
names(within) <- mz
names(betweenSampleVariance) <- mz
names(parameter) <- mz

########################################################
###########   CONSIDER ONLY SIGNIFICANT PEAKS
########################################################

signProteins <- row.names(significance[significance$tumorn<=signifcut,])

# select proteins with meaningful correlations

CORR_JUNK <- COR[signProteins][COR[signProteins]>intraclasscorr]

signProteins1 <- names(abs(parameter)[names(CORR_JUNK)][abs(parameter)[names(CORR_JUNK)]>0.1])

betweenNew <- betweenSampleVariance[signProteins1]

betweenSummary <- summary(betweenNew)
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
# names of proteins in various ranges of biological variance

RANGE1 <- names(betweenNew[betweenNew<=as.vector(betweenSummary[2])])

RANGE2 <- names(betweenNew[betweenNew<=as.vector(betweenSummary[3]) & betweenNew>as.vector(betweenSummary[2])])

RANGE3 <- names(betweenNew[betweenNew<=as.vector(betweenSummary[5]) & betweenNew>as.vector(betweenSummary[3])])
###################################################################################################

Corr <- median(COR[RANGE2])
techVar <- median(within[RANGE2])
bioVar <- median(betweenSampleVariance[RANGE2])
DIFF <- median(abs(parameter[RANGE2]))

list(Corr=Corr,techVar=techVar,bioVar=bioVar,DIFF=DIFF)

standardGeneric("sampleSizeParameters")}

)


########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
