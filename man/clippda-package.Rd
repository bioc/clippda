\name{clippda-package}
\alias{clippda-package}
\alias{clippda}
\docType{package}
\title{A package for clinical proteomics profiling data analysis}
\description{This package is still under development but it is intended to provide 
a range of tools for analysing clinical genomics, 
methylation and proteomics, data with the non-standard repeated 
expression measurments
arising from technical replicates.
Most of these studies are observational  
case-control by design and the results of analyses must be 
appropriately adjusted for confounding factors and 
imbalances in the data. 
 This regression-type problem is different from the 
regression problem in 
\code{limma}, in which all the \code{covariates} are some kind 
of \code{contrasts}
 and are 
therefore important. Our method is specifically suitable for 
analysing single-channel microarrays and proteomics data with repeated 
probe, or peak 
measurements,  especially 
in the case where
there is no one-to-one correspondence between cases and controls
and the data cannot be analysed as log-ratios.
In the current version (version 0.1.0), 
we are more concerned with the problem of sample size calculations for these data sets.
But some tools for pre-processing of the repeated peaks data, 
including tools for checking for the consistency in the
number 
of replicates across samples, the consistency of 
the peak information
 between replicate 
spectra and tools for data formatting 
and averaging, are included. 
\code{clippda} also implements
 a routine
for evaluating differential-expression between cases and controls, especially for data
in which each sample is assayed more than once, and are obtained from 
studies which are observational, or 
 those for which the data are heterogeneous
(e.g. data for cancer studies in which controls are not directly sampled,
 but are obtained 
from samples from suspected cases that turn out to be benign disease, 
after an operation, for example. 
In this case there could be serious
imbalances in demographics between the cases and controls). 
The test statistics considered are derived from the methods developed by Nyangoma
et al. (2009).  These new methods for evaluating differential-expression 
are compared with the empirical Bayes method in the 
\code{limma} package. 
To limit the number of false positive discoveries, we control
 the tail probability of the proportion of false positives, (TPPFP). 
Further details can be found in the package \code{vignette}.
}
\details{
\tabular{ll}{
Package: \tab anRpackage\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2009-03-25\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
}
This package provides a method for calculating the sample size required when planning 
proteomic profiling studies using repeated peak measurements. At the planning stage, 
an experimenter typically does not yet have information on the heterogeneity of the data expected.
We provide a method which makes it possible to input, and adjust for the effect of, 
the expected
heterogeneity in the sample size 
calculations. The code for calculating sample size is 
the \code{sampleSize} function. It requires the computation of the
 between- and within-sample variations, the differences in mean between 
cases and controls, the intraclass correlations
between duplicate
peak data, and the heterogeneity correction factor. These can be computed 
from  pilot data using the functions:
 \code{betweensampleVariance}, \code{withinsampleVariance},
\code{replicateCorrelations} and \code{FisherInformation}, respectively.
Before the data can be analysed using these functions, it must be adequately 
pre-processed,
and this package provides a number of tools for doing this. 
We provide a grid of the clinically important differences versus
protein variances (with superimposed sample size contours). On this grid, we 
have plotted sample sizes computed using parameters from several real-life 
proteomic data
from a range of cancer-types, fluid-types, cancer stages and experimental 
protocols of SELDI and MALDI. These values provide sample size 
ranges which may be used to estimate the
number of samples required. The example below takes you through some of the processes
of sample size calculations using this package. 
}
\author{
Stephen Nyangoma

Maintainer: S Nyangoma \email{s.o.nyangoma@bham.ac.uk}
}
\references{
Birkner, et al. Issues of processing and multiple testing of 
SELDI-TOF MS Proteomic Data. Stat Appl Genet Mol Biol 2006, 
5.1

Nyangoma SO, Ferreira JA, Collins SI, Altman DG, Johnson PJ, and
Billingham LJ: Sample size calculations for planning clinical
proteomic profiling studies using mass spectrometry. 
Bioinformatics (Submitted)

Nyangoma SO, et al. Multiple Testing Issues in Discriminating 
Compound-Related 
Peaks and Chromatograms from High Frequency Noise, Spikes and Solvent-Based
Noise in LC - MS 
Data Sets. Stat Appl Genet Mol Biol 2007, 6, 1, Article 23

Smyth GK, et al.: Use of within-array replicate spots for assessing differential
expression in microarray experiments. Bioinformatics 2005, 21, 2067 - 75

Smyth GK: Linear models and emperical Bayes methods for assessing differential expression
in microarray experiments. Stat Appl Genet Mol Biol 2004, 3, 1, Article 3
 }
\keyword{package}
\examples{
#########################################################################################
# The routine for calculating sample size required when planning a clinical proteomic
# profiling study is provided in the sampleSize function. First, this functions performs 
# computations for sample size parameters, that include: the biological variance, the
# tecnical variance, the differences to be estimated, the intraclass correlation 
# (if unknown). These computations are done ase follows: 
#########################################################################################

#########################################################################################
# biological variation, difference to be estimated, and the p-values for differential-
# expression are computed using the generic function: betweensampleVariance
# It requires data of a aclinicalProteomicsData class, as input 
########################################################################

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

# we use the log base 2 expression values

LOG_DATA <- log2(JUNK_DATA)

#######################################################################################
# compute biological variation, difference to be estimated, and the p-values 
#######################################################################################

BiovarDiffSig <- betweensampleVariance(OBJECT)

BiovarDiffSig

#####################
# technical variance
#####################

sample_technicalVariance(Data)

#######################################################################################
# heterogeneity correction-factor is the second diagonal element of the output 
# matrix from the fisherInformation function, i.e. from the expected Fisher Information
########################################################################################

Z <- as.vector(fisherInformation(Data)[2,2])/2
Z

###################################################################################
# The outputs of these functions are converted into statistics used in
# the sample size calculations using a wraper function sampleSizeParameters
# it gives the consensus parameter values. You must specify the p-value and the 
# intraclass correation, cutoff. The description of how these parameters
# are chosen is given in Nyangoma, et al. (2009).  
###################################################################################

intraclasscorr  <-  0.60 #cut-off for intraclass correlation

signifcut <- 0.05      #significance cut-off

sampleparameters=sampleSizeParameters(Data,intraclasscorr,signifcut)

#######################################################################################
# SAMPLE SIZE CALCULATIONS
#The function sampleSize calculates the protein variance, difference to be estimated, 
# the technical varaince. These parameters are computed from statistics of peaks with 
# medium biological variation. 
# It also gives sample sizes for beta=c(0.90,0.80,0.70) and alpha = c(0.001, 0.01,0.05)
#######################################################################################

samplesize <- sampleSize(OBJECT,intraclasscorr,signifcut)
samplesize

}
