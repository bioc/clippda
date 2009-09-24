setMethod("proteomicsExprsData",signature=signature(Data="aclinicalProteomicsData"),

function (Data, ...) {
    
rawData <- data.frame(Data@rawSELDIdata)

    rawData$SampleTag <- as.vector(rawData$SampleTag)

         rawData$Intensity <- as.numeric(as.vector(rawData$Intensity))

    rawData$Substance.Mass <- as.numeric(as.vector(rawData$Substance.Mass))

rawData

}
)
