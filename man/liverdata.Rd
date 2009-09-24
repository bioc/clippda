\name{liverdata}
\alias{liverdata}
\docType{data}
\title{A dataframe of the protein expression data, peak information, and
 sample information}
\description{
  A dataframe of the duplicate protein expression data, peak information,
 sample information (e.g. sample ID, stage, gender, etc.). 
This is a pre-processed version of ``raw \code{.csv}'' file from the 
Biomarker wizard. The pre-processing involves filtering out samples with
conflicting peak information, and detecting and discarding samples with no replicates. 
}
\usage{data(liverdata)}
\format{
  A data frame with 13886 observations on the following 6 variables.
  \describe{
    \item{\code{SampleTag}}{a numeric vector of sample ID.}
    \item{\code{CancerType}}{a factor, with levels \code{c} and \code{n}, indicating 
      cancer class}
    \item{\code{Spectrum}}{a numeric vector, indicating the experimental run.}
    \item{\code{Peak}}{a numeric vector identifying the peak.}
    \item{\code{Intensity}}{a numeric vector of expression values.}
    \item{\code{Substance.Mass}}{a numeric vector contaning the m/z (mass-to-charge ratio) value.}
  }
}
\source{
  Ward DG, Cheng Y, N'Kontchou G, Thar TT, Barget N, Wei W, Billingham LJ, Martin A, Beaugrand M, Johnson PJ: 
	Changes in the serum proteome associated with the development of hepatocellular carcinoma in hepatitis C-related 
cirrhosis. Br J Cancer. 2006, 94(2):287-92.
}
\references{
   Ward DG, Cheng Y, N'Kontchou G, Thar TT, Barget N, Wei W, Billingham LJ, Martin A, Beaugrand M, Johnson PJ: 
	Changes in the serum proteome associated with the development of hepatocellular carcinoma in hepatitis C-related 
cirrhosis. Br J Cancer. 2006, 94(2):287-92.
}
\examples{
#######################################################
#######################################################
## a pre-proceesed version of the raw .csv file from the
## Biomarker wizard.
#######################################################
#######################################################
 
data(liverdata)
data(liverRawData)
############################################################################################
############################################################################################
# liverdata is obtained by pre-processing of the raw .csv file from the Biomarker wizard 
# as follows. These samples pre-processed to:
#  (i) discard the information on samples which have no replicate data, and
# (ii) for samples with more than 2 replicate expression data, only duplicates with most 
#      similar peak information are retained for use in subsequent analyses. 
# A wrapper function for executing these two pre-processing steps is preProcRepeatedPeakData
#############################################################################################
#############################################################################################

threshold <- 0.80 
no.replicates <- 2
no.peaks <- 53
Data <- preProcRepeatedPeakData(liverRawData, no.peaks, no.replicates, threshold)

###########################################################################################
###########################################################################################
# Only sample with ID 250 has no replicates and has been omitted from the data to be used 
# in subsequent analyses. This fact may varified by  using:
###########################################################################################
###########################################################################################

setdiff(unique(liverRawData$SampleTag),unique(liverdata$SampleTag))
setdiff(unique(Data$SampleTag),unique(liverdata$SampleTag))

#########################################################################
# Now filter out the samples with conflicting replicate peak information
# using the spectrumFilter function:
#########################################################################

TAGS <- spectrumFilter(Data,threshold,no.peaks)$SampleTag

NewRawData2 <- spectrumFilter(Data,threshold,no.peaks) 
dim(Data)

dim(liverdata)

dim(NewRawData2)

#########################################################################################
#########################################################################################
# In the case of this data (the liver data), all technical replicates have coherent peak 
# information, since no sample information has been discarded by spectra filter.
#########################################################################################
#########################################################################################

##########################################################################################
##########################################################################################
# Let us have a look at what the pre-processing does to samples with more than 2 replicate
# spectra. Both samples with IDs 25 and 40 have more than 2 replicates.
##########################################################################################
##########################################################################################

length(liverRawData[liverRawData$SampleTag == 25,]$Intensity)/no.peaks
length(liverRawData[liverRawData$SampleTag == 40,]$Intensity)/no.peaks

######################################################################################
######################################################################################
# Take correlations of the log-intensities to find which of the 2 replicates have the 
# most coherent peak information.
########################################################################################
########################################################################################

Mat1 <- matrix(liverRawData[liverRawData$SampleTag == 25,]$Intensity,53,3)
Mat2 <-matrix(liverRawData[liverRawData$SampleTag == 40,]$Intensity,53,4)
cor(log2(Mat1))
cor(log2(Mat2))

#use mostSimilarTwo function to get duplicate spectra with most coherent peak information

Mat1 <- matrix(liverRawData[liverRawData$SampleTag == 25,]$Intensity,53,3)
Mat2 <-matrix(liverRawData[liverRawData$SampleTag == 40,]$Intensity,53,4)
sort(mostSimilarTwo(cor(log2(Mat1))))
sort(mostSimilarTwo(cor(log2(Mat2))))

#######################################################################################
#######################################################################################
#Next, check that the pre-processed data, \Robject{NewRawData2}, contains similar 
# information to liverdata (the allready pre-processed data, included in the clippda).
#######################################################################################
#######################################################################################
names(NewRawData2)
dim(NewRawData2)
names(liverdata)
dim(liverdata)
setdiff(NewRawData2$SampleTag,liverdata$SampleTag)
setdiff(liverdata$SampleTag,NewRawData2$SampleTag)
summary(NewRawData2$Intensity)
summary(liverdata$Intensity)
}
\keyword{datasets}
