preProcRepeatedPeakData <-
function(rawData,no.peaks,no.replicates,threshold) {

# get the sample tags for samples with disparate replicates 

TOTAL_COUNT = checkNo.replicates(rawData,no.peaks,no.replicates) 

  TOTAL_COUNT1 = NULL

    RepCounts = NULL

      for(i in 1:length(TOTAL_COUNT)) {

       if((length(rawData[rawData$SampleTag ==TOTAL_COUNT[i],]$Intensity)/no.peaks) ==1) 

  TOTAL_COUNT1=c(TOTAL_COUNT1,TOTAL_COUNT[i])
}

# remove samples that are not replicated

NewRawData=rawData[!(rawData$SampleTag %in% TOTAL_COUNT1),] 

## sift through the remaining data to obtain duplicates that are highly correlated among replicates

NewRawDataTags=unique(as.character(NewRawData$SampleTag))

#NewRawDataTags=unique(NewRawData$SampleTag)

NEWJUNK_DATA=NULL

for(i in 1:length(NewRawDataTags)) {

    COLUMNS=dim(NewRawData[NewRawData$SampleTag == NewRawDataTags[i],])[1]/no.peaks

      TempPeakData=NewRawData[NewRawData$SampleTag == NewRawDataTags[i],]$Intensity

      TempPeakData<-negativeIntensitiesCorrection(TempPeakData) #transform data for logs to be defined

      DataMat=data.frame(matrix(log2(TempPeakData),no.peaks,COLUMNS))

       Mat=DataMat

       PAIRS_SELECTED=sort(mostSimilarTwo(Mat))

       JUNK_DATA = NewRawData[NewRawData$SampleTag == NewRawDataTags[i],]

       JUNK_DATA = data.frame(JUNK_DATA,Index=c(rep(1:COLUMNS,each=no.peaks)))

       JUNK_DATA = JUNK_DATA[JUNK_DATA$Index %in% PAIRS_SELECTED,]

      JUNK_DATA = JUNK_DATA[,-dim(JUNK_DATA)[2]]

    #JUNK_DATA

NEWJUNK_DATA = rbind(NEWJUNK_DATA,JUNK_DATA)

}

NewRawData1=NEWJUNK_DATA

NewRawData1


}


