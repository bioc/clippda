spectrumFilter <-
function(Data,threshold,no.peaks) {

UNIQUETAGS=as.character(unique(Data$SampleTag))

TempData=Data
TempPeakData=Data$Intensity
TempPeakData<-negativeIntensitiesCorrection(TempPeakData) #transform data for logs to be defined
    
TempData$Intensity=log2(TempPeakData)

CORRELATION=NULL

for(i in 1:length(UNIQUETAGS)) {

COR=cor(matrix(TempData[TempData$SampleTag == UNIQUETAGS[i],]$Intensity,no.peaks,2))

CORRELATION=rbind(CORRELATION,c(UNIQUETAGS[i],COR[1,2]))

}

CORRELATION=data.frame(CORRELATION)

names(CORRELATION)=c('SampleTags','correlation')

CORRELATION$correlation=as.numeric(as.vector(CORRELATION$correlation))

CORRELATION$SampleTags=as.vector(CORRELATION$SampleTags)

CORRELATION=CORRELATION[CORRELATION$correlation >= threshold,]

 TAGS=CORRELATION$SampleTag

Data <- Data[Data$SampleTag %in% TAGS,]
 
Data

}

