\name{aclinicalProteomicsData-methods}
\alias{aclinicalProteomicsData-methods}
\title{ S4 method for the aclinicalProteomicsData class}
\description{
  An S4 method for the object \code{aclinicalProteomicsData} class objects. 
}
\examples{
setClass("clinicalProteomicsData",representation(rawSELDIdata="matrix",phenotypicData="matrix",varInfo="character",
variableClass="character",no.peaks="numeric"),
prototype(rawSELDIdata=matrix(0),phenotypicData=matrix(0),varInfo=as.character(0),variableClass=as.character(0),no.peaks=0))

slotNames("aclinicalProteomicsData")

setMethod("show","aclinicalProteomicsData",
function(object) {
cat("clinical proteomics data")
cat("Type			:", class(object), "\n")
cat("raw data		:", paste(object@rawSELDIdata), "\n")
cat("phenotypic data	:", paste(object@phenotypicData), "\n")
cat("variable information	:", paste(object@varInfo), "\n")
cat("variable class		:", paste(object@variableClass), "\n")
cat("number of peaks	:", paste(object@no.peaks), "\n")
}
)

slotNames( new("clinicalProteomicsData"))

## library(clippda)
data(liverdata)
data(liver_pheno)

OBJECT=new("clinicalProteomicsData")
OBJECT@rawSELDIdata=as.matrix(liverdata)
OBJECT@varInfo=c("tumor" ,    "sex")
OBJECT@phenotypicData=as.matrix(liver_pheno)
OBJECT@variableClass=c('numeric','factor','factor')
OBJECT@no.peaks=53

show(OBJECT)
}
