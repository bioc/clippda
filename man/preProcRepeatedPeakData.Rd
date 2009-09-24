\name{preProcRepeatedPeakData}
\alias{preProcRepeatedPeakData}
\title{A function to pre-process repeated raw peak data}
\description{
This function pre-processes repeated peak data from the Biomarker wizard.
It identifies, and removes,  samples which have no replicates. 
Then, for the samples with two or more replicates, it selects and returns data
 for the two replicates with most similar expression pattern. 
Then,  the samples with conflicting 
peak information may be removed using the function: 
\code{spectrumFilter}.
The output is similar to the \code{liverdata}, a pre-processed data which is included in this package.
}
\usage{
preProcRepeatedPeakData(rawData, no.peaks, no.replicates,threshold)
}
\arguments{
  \item{rawData}{ the raw data from the Biomarker wizard.}
  \item{no.peaks}{ the number of peaks detected by the Biomarker wizard.}
  \item{no.replicates}{the intended number of replicates. The departures from 
this number could be due to mislabelling, quality control (QC) assays, or 
a few other samples which could have been assayed more times than the majority of samples.
}
 \item{threshold}{The threshold for declaring expression patterns between duplicates as being ``similar''. 
It is especially needed in the function \code{spectrumFilter}, which is used by this function.
}
}
\value{
  It returns a dataframe of duplicate expression data for all peaks in the same format as 
the ``.csv'' data from the 
Biomarker 
wizard. }
\references{ Ward DG, Nyangoma S, Joy H, Hamilton E, Wei W, Tselepis C, Steven N, Wakelam MJ, Johnson PJ, Ismail T, Martin A: Proteomic profiling of urine for 
the detection of colon cancer. Proteome Sci. 2008,  16(6):19}
\author{Stephen Nyangoma}
\examples{
#######################################################
#######################################################
# a pre-proceesed version of the raw .csv file from the
# Biomarker wizard.
#######################################################
#######################################################
 
data(liverdata) # allready pre-processed data
data(liverRawData) # raw version of liverdata
############################################################################################
############################################################################################
# These samples pre-processed to:
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

#TAGS <- spectrumFilter(Data,threshold,no.peaks)$SampleTag

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