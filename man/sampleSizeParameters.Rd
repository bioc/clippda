\name{sampleSizeParameters}
\alias{sampleSizeParameters}
\title{A generic function to calculate sample size parameters}
\description{
 This generic function computes input parameters for the sample size calculation function.
}
\usage{
sampleSizeParameters(Data,intraclasscorr,signifcut, \dots)
}
\arguments{ 
\item{Data}{An object of \code{aclinicalProteomicsData}  class.  }
\item{intraclasscorr}{An object of \code{numeric}  class. It is a known
 value of the intraclass correlation, or an estimate from a pilot data. }
\item{signifcut}{An object of \code{numeric}  class. 
It is significance threshold (usually, taken to be 0.05 in the analysis 
of the protein profiling studies).}
\item{\dots}{Some methods for this generic function may take     additional, optional arguments.  At present none do.}
}
\value{A list of parameters:
  \item{Corr }{the intraclass correlation from your pilot data.}
  \item{techVar}{the technical variance from your pilot data.}
  \item{bioVar}{the biological variance from your pilot data.}
\item{DIFF}{the clinically important difference from your pilot data.}
\item{no.peaks}{the number of peaks detected by the Biomarker wizard.}
}
\references{
Nyangoma SO, Ferreira JA, Collins SI, Altman DG, Johnson PJ, and
Billingham LJ: Sample size calculations for planning clinical
proteomic profiling studies using mass spectrometry. 
Bioinformatics, 2009, Submitted

Smyth GK, et al.: Use of within-array replicate spots for assessing differential
expression in microarray experiments. Bioinformatics 2005, 21, 2067 - 75

Smyth GK: Linear models and emperical Bayes methods for assessing differential expression
in microarray experiments. Stat Appl Genet Mol Biol 2004, 3, 1, Article 3 
}
\author{ Stephen Nyangoma}
\examples{

intraclasscorr  <-  0.60 #cut-off for intraclass correlation

signifcut <- 0.05      #significance cut-off

data(liverdata)

data(liver_pheno)

OBJECT=new("aclinicalProteomicsData")

OBJECT@rawSELDIdata=as.matrix(liverdata)
OBJECT@covariates=c("tumor" ,    "sex")
OBJECT@phenotypicData=as.matrix(liver_pheno)
OBJECT@variableClass=c('numeric','factor','factor')
OBJECT@no.peaks=53


sampleSizeParameters(OBJECT,intraclasscorr,signifcut)
}