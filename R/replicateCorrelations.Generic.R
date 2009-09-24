#################################################################
#################################################################
#################################################################
# replicate correlations
#################################################################
#################################################################
#################################################################

setGeneric("replicateCorrelations",

function(Data,...) {

rawData <- proteomicsExprsData(Data)

no.peaks <- Data@no.peaks

JUNK_DATA <- sampleClusteredData(rawData,no.peaks)

JUNK_DATA <- negativeIntensitiesCorrection(JUNK_DATA)

names(JUNK_DATA)=as.character(as.numeric(names(JUNK_DATA)))

# we use the log base 2 expression values

LOGDATA <- log2(JUNK_DATA) 

#Data=OBJECT

variables <- Data@covariates

variableClass = Data@variableClass

#PhenoData <- proteomicspData(Data)

#PhenoInfo <- phenoDataFrame(PhenoData, variableClass)

PhenoInfo <- proteomicspData(Data)


LOGDATA <- LOGDATA[,as.character(PhenoInfo$SampleTag)]


library(limma)
library(statmod)
tempdata <- as.matrix(LOGDATA)

      junkData<- as.vector(t(LOGDATA[1,]))

    	if(length(unique(variables))==1) 
  
                 LM <- lm(junkData~PhenoInfo,x=TRUE)

                       LM <- lm(junkData ~.,data=PhenoInfo,x=TRUE)
                    
         design=(data.frame(LM$x))

     dupfit <- duplicateCorrelation(tempdata,design=design,ndups=2,spacing=1)
 
all.correlations <- tanh(dupfit$atanh.correlations)

list(consensus=dupfit$consensus,all.correlations=all.correlations)

standardGeneric("replicateCorrelations")}
)


########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
