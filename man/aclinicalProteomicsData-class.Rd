\name{aclinicalProteomicsData-class}
\Rdversion{1.1}
\docType{class}
\alias{aclinicalProteomicsData-class}
\alias{aclinicalProteomicsData}
\title{Class "aclinicalProteomicsData"}
\description{This is a class object for the mass spectrometry data sets, which 
are in the same format as the raw data from the Biomarkers wizard software. 
It has slots of matrices of raw mass spectrometry and phenotypic data sets, a 
character variable for the classes of all the covariates in the phenotypic data 
matrix, a character variable for the covariates of interest, and numeric value for 
the number of peaks of interest.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("aclinicalProteomicsData", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{rawSELDIdata}:}{Object of class \code{"matrix"}}
    \item{\code{phenotypicData}:}{Object of class \code{"matrix"}}
    \item{\code{variableClass}:}{Object of class \code{"character"}}
    \item{\code{covariates}:}{Object of class \code{"character"}}
    \item{\code{no.peaks}:}{Object of class \code{"numeric"}}
  }
}
\section{Methods}{
  \describe{
    \item{show:}{Display an \code{aclinicalProteomicsData} instance.}
}
  
%No methods defined with class "aclinicalProteomicsData" in the signature.
}
\author{S Nyangoma }
\examples{
showClass("aclinicalProteomicsData")

data(liverdata)
data(liver_pheno)

OBJECT = new("aclinicalProteomicsData",
             rawSELDIdata=as.matrix(liverdata),
             phenotypicData=as.matrix(liver_pheno),
             variableClass=c('numeric','factor','factor'),
             no.peaks=53)

show(OBJECT)
}
\keyword{classes}
