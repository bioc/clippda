\name{phenoDataFrame}
\Rdversion{1.1}
\alias{phenoDataFrame}
\title{
A generic function to set classes for the variables in the dataframe of phenotypic information.
}
\description{
A function to set classes (e.g. as numeric or factor) to the variables in the dataframe.
}
\usage{
phenoDataFrame(PhenoData, variableClass)
}
\arguments{
  \item{PhenoData}{
is the dataframe of phenotypic information extracted from an \code{object} of class 
\code{aclinicalProteomicsData}.
}
\item{variableClass}{
a character vector of length equal to the number of columns of the
dataframe of phenotypic information, giving classes of the variables studied.
}
}
\value{
A dataframe of class-corrected phenotypic variables.
}
\author{
Stephen Nyangoma
}
\examples{
data(liverdata)

data(liver_pheno)

OBJECT=new("aclinicalProteomicsData")

OBJECT@rawSELDIdata=as.matrix(liverdata)
OBJECT@covariates=c("tumor" ,    "sex")
OBJECT@phenotypicData=as.matrix(liver_pheno)
OBJECT@variableClass=c('numeric','factor','factor')
OBJECT@no.peaks=53

Data=OBJECT
variableClass =Data@variableClass

variables = c("SampleTag","tumor","sex")

PhenoInfo <- data.frame(Data@phenotypicData)

PhenoData <- data.frame(Data@phenotypicData)

pData=phenoDataFrame(PhenoData, variableClass) 

class(pData$sex)
class(pData$SampleTag)
class(pData$tumor)
}
