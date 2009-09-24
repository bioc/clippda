\name{sample_technicalVariance}
\alias{sample_technicalVariance}
\alias{withinsampleVariance}
\title{A generic function for computing the technical variance}
\description{
  This generic function computes the within-sample (technical) variance.
It fits a simple regression model for repeated measures using the \code{mixedModel2} 
function in the \code{statmod}
 package. 
The technical variance is the block component of the \code{varcomp} output.
}
\usage{
sample_technicalVariance(Data, \dots)
}
\arguments{
\item{Data}{An object of \code{aclinicalProteomicsData}  class.
  }
  \item{\dots}{Some methods for this generic function may take
    additional, optional arguments.  At present none do.}
}
\value{
  It returns a vector of the within-sample variances, one for each peak.
}
\author{Stephen Nyangoma }
\examples{
#arrange the data in a form that can be averaged by limma function dupcor
# use the function called limmaData
data(liverdata)

data(liver_pheno)

OBJECT=new("aclinicalProteomicsData")

OBJECT@rawSELDIdata=as.matrix(liverdata)
OBJECT@covariates=c("tumor" , "sex")
OBJECT@phenotypicData=as.matrix(liver_pheno)
OBJECT@variableClass=c('numeric','factor','factor')
OBJECT@no.peaks=53

sample_technicalVariance(OBJECT)

}

