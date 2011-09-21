\name{betweensampleVariance}
\alias{betweensampleVariance}
\alias{biologicalVariance}
\title{A generic function for computing the biological variance and mean differences 
between cases and controls}
\description{
  This generic function fits a regression model to the averaged replicate data. 
The outputs are the between sample variance, and the differences in mean expression 
between cases and controls, adjusted for confounders. 
}
\usage{
betweensampleVariance(Data, \dots)
}
\arguments{
 \item{Data}{An object of \code{aclinicalProteomicsData}  class.
  }
  \item{\dots}{Some methods for this generic function may take
    additional, optional arguments.  At present none do.}
  }
\value{It returns a list with the following components:
\item{betweensamplevariance }{A vector of the between-sample variance for each peak.}
\item{differences }{A vector of the differences in mean expression values
between the cases and controls, adjusted for confounders for each peak.
}
\item{significance }{A dataframe, or a vector of the differential-expression p-values
for each peak.}
}
\author{ Stephen Nyangoma}
\examples{
########################################
##### methods for the generic function
########################################

showMethods("betweensampleVariance")

###################################################
# Creating data of a aclinicalProteomicsData class
###################################################

data(liverdata)

data(liver_pheno)

OBJECT=new("aclinicalProteomicsData")

OBJECT@rawSELDIdata=as.matrix(liverdata)

OBJECT@covariates=c("tumor" , "sex")

OBJECT@phenotypicData=as.matrix(liver_pheno)

OBJECT@variableClass=c('numeric','factor','factor')

OBJECT@no.peaks=53

Data=OBJECT

#################################################################################
# Data manipulation carried out internally by the betweensampleVariance function 
#################################################################################

rawData <- proteomicsExprsData(Data)

no.peaks <- Data@no.peaks

JUNK_DATA <- sampleClusteredData(rawData,no.peaks)

JUNK_DATA=negativeIntensitiesCorrection(JUNK_DATA)

# we use the log-basetwo2 expression values

LOG_DATA <- log2(JUNK_DATA)

#######################################################################################
# compute biological variation, difference to be estimated, and the p-values 
#######################################################################################

BiovarDiffSig <- betweensampleVariance(OBJECT)

BiovarDiffSig

}
