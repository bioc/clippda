\name{sampleSize}
\alias{sampleSize}
\title{A function for sample size calculations}
\description{
This generic function \code{sampleSize} calculates the protein variance and the sample size required to estimate the clinically important differences (\code{DIFF}).
The input data are the consensus parameters of peaks with medium biological variation.
}
\usage{
sampleSize(Data,intraclasscorr,signifcut, \dots)
}
\arguments{
\item{Data}{An object of \code{aclinicalProteomicsData}  class.
  }
\item{intraclasscorr}{An object of \code{numeric}  class. It is a known
 value of the intraclass correlation, or an estimate from a pilot data.
  }
\item{signifcut}{An object of \code{numeric}  class. 
It is significance threshold (usually, taken to be 0.05 in the analysis 
of the protein profiling studies).
  }
 \item{\dots}{Some methods for this generic function may take
    additional, optional arguments.  At present none do.}
}
\details{
The sample sizes are computed for various combinations of the power with 
values \code{beta=c(0.90,0.80,0.70)} and the significance values, \code{alpha = c(0.001, 0.01,0.05)}. 
Note that here we use \code{beta} for power rather
than the conventional \code{1-beta}. 
 }
\value{
   \item{protein_variance}{consensus protein variance}
  \item{replicate_correlation }{consensus intraclass correlation}
  \item{sample_size}{the sample size required}
}
\references{
Nyangoma SO, Ferreira JA, Collins SI, Altman DG, Johnson PJ, and
Billingham LJ: Sample size calculations for planning clinical
proteomic profiling studies using mass spectrometry. 
Bioinformatics (Submitted)

Smyth GK, et al.: Use of within-array replicate spots for assessing differential
expression in microarray experiments. Bioinformatics 2005, 21, 2067 - 75

Smyth GK: Linear models and emperical Bayes methods for assessing differential expression
in microarray experiments. Stat Appl Genet Mol Biol 2004, 3, 1, Article 3
}
\author{
Stephen Nyangoma
}
\examples{
########################################################################
## SAMPLE SIZE
#######################################################################
#The function sampleSize calculates the biological variance, differences.
#These are the consensus values of peaks with median biological variation
# It also gives sample sizes for beta=c(0.90,0.80,0.70) and alpha = c(0.001, 0.01,0.05)

####################################################################
####################################################################
####################################################################

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


sampleSize(OBJECT,intraclasscorr,signifcut)

####################################################################
####################################################################
####################################################################
}