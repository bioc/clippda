\name{proteomicsExprsData}
\Rdversion{1.1}
\alias{proteomicsExprsData}
\title{
A generic fuction to extract duplicate SELDI data from an 
object of aclinicalProteomicsData class 
in the same format as the data from
Biomarkers wizard 
}
\description{
This generic function extracts a matrix of duplicate SELDI data from an 
object of aclinicalProteomicsData class. It then converts it into a dataframe 
which is in the same format as the data from
Biomarkers wizard. In this dataframe, the intensities and the subject 
mass-to-charge ratio are represented as numeric variables, while the sample-tag is
 represented as a character variable.  
}
\usage{
proteomicsExprsData(Data, ...)
}
\arguments{
   \item{Data}{
is an object of a \code{aclinicalProteomicsData} class.
}
  \item{\dots}{
means other defined arguments. Currently, we have not defined additional arguments. 
}
}
\value{
A dataframe of expression values, the substance mass, patient labels, and 
any other defined sample information.
}
\author{
S Nyangoma
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

head(proteomicsExprsData(OBJECT))
}