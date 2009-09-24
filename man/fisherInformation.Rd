\name{fisherInformation}
\alias{fisherInformation}
\title{A generic function to compute the heterogeneity correction factor in sample 
size calculations}
\description{
This generic function computes the inverse of the expected 'Fisher' information matrix, 
\code{I^-1/theta} (see 
the definition of theta in section 2.3 of Nyangoma \code{et al}., 2009). The 
second diagonal element of this matrix is the variance of the mean difference
between cases and controls, having adjusted for the effect of confounders. 
The elements of \code{I} are sums of the proportions 
of samples having given attributes, or sums of proportions of class memberships 
of given conditional 
contingency tables obtained from the cross
tabulated the attributes of the samples under study. Thus it
contains  information on the heterogeneity in the
data due to imbalances in the proportions of samples having given 
attributes. 
}
\usage{
fisherInformation(Data, \dots)
}
\arguments{
  \item{Data}{An object of \code{aclinicalProteomicsData}  class. It extracts 
and uses a dataframe of the clinical information about the samples from a \code{slot}
in the \code{Data}.
  }
  \item{\dots}{Some methods for this generic function may take
    additional, optional arguments.  At present none do.}
}
\details{
Note that continuous variables must first be discretized, and the variable 
names must coincide 
with the column 
names of \code{PhenoInfo} extracted from the \code{object}.  
Currently this function only accepts a maximum of three binary variables. 
The existing methods (e.g. Diggle et al. 1997, page 31) 
for continuous repeated data 
consider only a single exposure variable.
 We 
recommend that some form of variable selection be used to determine which covariates
 to include 
in the analysis.
}
\value{
 This function returns a matrix: and its second diagonal element (divided by 2) is the
quantity called \code{Z} (or the heterogeneity-correction factor) 
in the sample size calculation function, \code{sampleSize}.
}
\references{ 1. Nyangoma SO, Ferreira JA, Collins SI, Altman DG, Johnson PJ, and Billingham LJ (2009): Sample size 
calculations for planning clinical proteomic profiling studies using mass spectrometry. Bioinformatics (Submitted)

2. Diggle PJ, Heagerty P, Liang K.-Y and Zeger SL. (2002). Analysis of Longitudinal 
Data (second edition). Oxford: Oxford University Press
}
\author{ Stephen Nyangoma}
\examples{
#########################################################################################
#The matrices of interest are of the form (see eq. 15, 18 and 22 Nyangoma et al. (2009))
#########################################################################################

#Examples are:

###################
# 1 binary variable
###################

data.frame(x1=c(1,'b'),x2=c('b','b'))

#####################
# 2 binary variables
#####################
data.frame(x1=c(1,'b','c'),x2=c('b','b','d'),x3=c('c','d','c'))

##########################################
# 3 binary variables

data.frame(x1=c(1,'b','c','d'),x2=c('b','b','e','f'),x3=c('c','e','c','g'),x4=c('d','f','g','d'))

##############################################################################
##############################################################################
# Data # pheno_urine
# the phenotypic information of the urine cancer patients and normal controls.
#####
# I have discretized protein concentration
# concentration<=70 and concentration>70
##########################################
##########################################

#data(pheno_urine)
#PhenoInfo <- pheno_urine

#variables <- c('Tumor','Sex','Protein_concIndex')

#variables=c('Tumor','Sex')
#variables=c('Tumor')


# Tumor must contain characters "c" and "n"

#Protein_concIndex <- pheno_urine[!(pheno_urine$stage == 'late'),]$Protein_conc

#Protein_concIndex[Protein_concIndex<=70] <- 0
#Protein_concIndex[Protein_concIndex>70] <- 1
#Protein_concIndex=as.factor(Protein_concIndex)

#PhenoInfo <- data.frame(pheno_urine[!(pheno_urine$stage == 'late'),],Protein_concIndex)

#FisherInformation(PhenoInfo,variables)

data(liverdata)

data(liver_pheno)

OBJECT=new("aclinicalProteomicsData")

OBJECT@rawSELDIdata=as.matrix(liverdata)
OBJECT@covariates=c("tumor" ,    "sex")
OBJECT@phenotypicData=as.matrix(liver_pheno)
OBJECT@variableClass=c('numeric','factor','factor')
OBJECT@no.peaks=53

inversefisherinformation <- fisherInformation(OBJECT)

inversefisherinformation


}