\name{checkNo.replicates}
\alias{checkNo.replicates}
\title{A function to detect disparity in the number of replicates across assays}
\description{
Sometimes in a mass spectrometry experiment, it happens that  
a few samples have been mislabelled. Mislabelling means that some replicates are 
in the wrong sample group,  and this results in some
samples having more (or less) replicates than the number intended by the experimentalist. 
Apart from disparity in the number of replicates due to mislabelling, a few samples, e.g. 
the quality control (QC) samples, are often assayed 
several times. The aim is to analyze data with the same number of technical
replicates (in this case, duplicates) for every sample.
The function \code{checkNo.replicates} identifies samples with  a disparate
number of replicates. The identified samples are treated as follows: 

(i) The QC samples can be 
independently analysed to ascertain the reproducibility of the data.
 
(ii) The samples with no replicates
are discarded from further analysis. 

(iii) The samples with more replicates than expected, due to mislabelling (or otherwise), are 
pre-processed using the function: \code{mostSimilarTwo} which detects and discards 
replicates which give conflicting peak information compared to the rest of the replicates.
Here, the two most similar replicates are treated as the correct replicates for 
the sample in question.
}
\usage{
checkNo.replicates(rawData, no.peaks, no.replicates)
}
\arguments{
  \item{rawData}{Duplicate data in the same format as the raw data from
 the Biomarker wizard software.}
  \item{no.peaks}{The number of peaks detected by the Biomarker wizard}
  \item{no.replicates}{The number of replicates intended by the biologist.}
}
\value{
   It returns a vector whose elements are labels for samples
with a disparate number peaks.
}
\author{Stephen Nyangoma}
\examples{

data(liverRawData)

rawData <- liverRawData

no.peaks <- 53

no.replicates <- 2

checkNo.replicates(rawData,no.peaks,no.replicates) 

}