checkNo.replicates <-
function(rawData,no.peaks,no.replicates) {

rawData$SampleTag=as.character(rawData$SampleTag)

COUNTER=unique(rawData$SampleTag)

TOTAL_COUNT=NULL


for(i in 1:length(COUNTER)) {

no.peaksrep=no.replicates*no.peaks 

      if(dim(rawData[rawData$SampleTag == COUNTER[i][1],])[1] != no.peaksrep)
        
      TOTAL_COUNT=c(TOTAL_COUNT,COUNTER[i])      
}

TOTAL_COUNT
}

