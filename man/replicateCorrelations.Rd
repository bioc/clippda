\name{replicateCorrelations}
\alias{replicateCorrelations}
\alias{intraclassCorrelations}
\title{A generic function to compute intraclass correlations}
\description{This generic function computes intraclass correlations for duplicate peak data.}
\usage{
replicateCorrelations(Data, \dots)
}
\arguments{
  \item{Data}{An object of \code{aclinicalProteomicsData}  class.
  }
  \item{\dots}{Some methods for this generic function may take
    additional, optional arguments.  At present none do.}
}
\value{
    \item{consensus:}{consensus intraclass correlation.}
  \item{correlations:}{intraclass correlations for each peak.}
 }
\references{
Nyangoma SO, Ferreira JA, Collins SI, Altman DG, Johnson PJ, and
Billingham LJ: Sample size calculations for planning clinical
proteomic profiling studies using mass spectrometry. 
Bioinformatics (Submitted)

Smyth GK, et al.: Use of within-array replicate spots for assessing differential
expression in microarray experiments. Bioinformatics 2005, 21, 2067 - 75

Smyth GK: Linear models and emperical Bayes methods for assessing differential 
expression
in microarray experiments. Stat Appl Genet Mol Biol 2004, 3, 1, Article 3
}
\author{Stephen Nyangoma}
\examples{
data(liverdata)

data(liver_pheno)

OBJECT=new("aclinicalProteomicsData")

OBJECT@rawSELDIdata=as.matrix(liverdata)
OBJECT@covariates=c("tumor" ,    "sex")
OBJECT@phenotypicData=as.matrix(liver_pheno)
OBJECT@variableClass=c('numeric','factor','factor')
OBJECT@no.peaks=53

replicateCorrelations(OBJECT)
}