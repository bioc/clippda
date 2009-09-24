`sampleClusteredData` <-
function(Data,no.peaks) {
Intensity <- NULL

junkData <- NULL

n1 <- 2*no.peaks 

columnstoUse1  <-  1:n1

columnIndices1  <- data.frame(split(columnstoUse1, 1:2))

uniqueTags1 <- unique(as.character(Data$SampleTag))

for(i in 1:length(uniqueTags1)) {

vec <- apply(matrix(Data[Data$SampleTag == uniqueTags1[i],]$Intensity,no.peaks,2),1,mean)

Intensity <- cbind(Intensity,vec)

######
somejunk <- matrix(Data[Data$SampleTag == uniqueTags1[i],]$Intensity,no.peaks,2)

somejunk1 <- somejunk[,1]

names(somejunk1) <- columnIndices1$X1

somejunk2 <- somejunk[,2]

names(somejunk2) <- columnIndices1$X2

somejunk1_2 <- rep(1,n1)

names(somejunk1_2) <- 1:n1

somejunk1_2[names(somejunk1_2) %in% names(somejunk1)] <- somejunk1

somejunk1_2[names(somejunk1_2) %in% names(somejunk2)] <- somejunk2
######

junkData <- cbind(junkData,somejunk1_2)

}

junkData <- data.frame(junkData)

names(junkData) <- uniqueTags1

junkData

}

