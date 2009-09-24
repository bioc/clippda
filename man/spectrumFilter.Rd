\name{spectrumFilter}
\alias{spectrumFilter}
\title{A function to filter out samples with conflicting pair-wise compound
 information}
\description{
This function filters out samples whose spectra  have poor pairwise correlations. These 
samples give conflicting TIC information.
}
\usage{
spectrumFilter(Data,threshold,no.peaks)
}
\arguments{
  \item{Data}{ a data frame of duplicate peak data, in the format of "raw .csv" data 
from the Biomarker wizard. The original data frame to have the following columns:
\code{SampleTag}, \code{CancerType}, \code{Spectrum.}, \code{Peak}.         
\code{Intensity} and \code{Substance.Mass}.
}
  \item{threshold}{indicates the threshold for rejecting 
samples whose spectra contain conflicting signal information. 
}
  \item{no.peaks}{the number of peaks.}
 }
\value{
    A data frame with two columns: the first contains the IDs of the samples
meeting the threshold and the second contains the corresponding correlations 
(i.e. similarities in peak information across spectra).
  }
\references{ Ward DG, Nyangoma S, Joy H, Hamilton E, Wei W, Tselepis C, Steven N, Wakelam MJ, Johnson PJ, Ismail T, Martin A: Proteomic profiling of urine for 
the detection of colon cancer. Proteome Sci. 2008,  16(6):19}
\author{Stephen Nyangoma}
\examples{
#######################################################
#######################################################
 
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

head(spectrumFilter(Data,threshold,no.peaks))
}