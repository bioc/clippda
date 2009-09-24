\name{sampleClusterdData}
\alias{sampleClusteredData}
\title{ A function to arrange the data in sample-wise pairs}
\description{
 This function arranges the duplicate data, grouped by samples, 
in a form which can be averaged using the \code{limma} function 
\code{avedups}.}
\details{
The output is a dataframe of repeated sample expression values, clustered by samples.
This data is used as an input to the \code{betweensampleVariance} function, and is then
 averaged and used to compute 
the biological variance as well as the mean difference in the expression values between 
cancer and noncancer controls. 
}
\usage{
sampleClusteredData(Data, no.peaks)
}
\arguments{
  \item{Data}{ A data frame of duplicate data from the biomarker wizard.}
  \item{no.peaks}{The number of peaks detected by the biomarker wizard.}
}
\value{
  The output is a dataframe of repeated sample expression values, clustered by samples.
}
\author{ 
Stephen Nyangoma
}
\examples{
# arrange the data in a form which can be averaged by the limma function dupcor
# use the function called limmaData

data(liverdata)

no.peaks <- 53

Data <- liverdata

JUNK_DATA <- sampleClusteredData(Data,no.peaks)
  }
