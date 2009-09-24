\name{proteomicspData}
\Rdversion{1.1}
\alias{proteomicspData}
\title{
A function to extract a dataframe of phenotypic information from an 
object of aclinicalProteomicsData class 
}
\description{
This function takes an object of \code{aclinicalProteomicsData} class, 
extracts a matrix of phenotypic data, and converts it to a dataframe with
with variables having defined classes.  
}
\usage{
proteomicspData(Data, ...)
}
\arguments{
  \item{Data}{
is an object of \code{aclinicalProteomicsData} class.
}
  \item{\dots}{
means other defined arguments. Currently, we have not defined additional arguments. 
}
}
\value{
Returns a dataframe with variables having defined classes.
}  
\author{
S Nyangoma
}
\examples{
########################################
##### the data
########################################


data(liverdata)

data(liver_pheno)

OBJECT=new("aclinicalProteomicsData")

OBJECT@rawSELDIdata=as.matrix(liverdata)
OBJECT@covariates=c("tumor" ,    "sex")
OBJECT@phenotypicData=as.matrix(liver_pheno)
OBJECT@variableClass=c('numeric','factor','factor')
OBJECT@no.peaks=53

head(proteomicspData(OBJECT))

}
