\name{negativeIntensitiesCorrection}
\Rdversion{1.1}
\alias{negativeIntensitiesCorrection}
\title{A function to correct the data for the negative
intensities caused by the normalization and background correction 
procedures of mass spectrometry data
}
\description{
This function corrects the mass spectra data, which has been pre-processed using
tools tools from the Biomarkers Wizard \code{PROcess} softwares, for the negative
intensities caused by their normalization and background correction 
procedures.
}
\usage{
negativeIntensitiesCorrection(Data)
}
\arguments{
  \item{Data}{
is a dataframe , or a matrix, or a vector, of numerical values.}
}
\value{
A dataframe , or a matrix, or a vector (whichever is the input quantity),  
of nonnegative numerical values.
}
\author{
Stephen Nyangoma
}
\examples{
data(liverdata)

no.peaks <- 53

JUNK_DATA <- sampleClusteredData(liverdata,no.peaks)

Data=JUNK_DATA

Data=JUNK_DATA
Data=Data+1
temp=negativeIntensitiesCorrection(Data)
temp[,1]
Data[,1]
}
