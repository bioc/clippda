\name{sampleSizeContourPlots}
\alias{sampleSizeContourPlots}
\title{A function to construct a grid with contours for calculating sample size in multi dimensional 
parameter space }
\description{
This function draws a grid for calculating the sample size based on the clinically important
values of 
variances versus differences. 
Based on the analysis of  data from past proteomic
profiling studies of cancer, we define the clinically important 
parameters as the summary statistics 
 of
the intensities of the peaks with medium biological 
variation. On the grid, you may display
the parameter 
values from a wide range of real-life data from past proteomic 
profiling studies, including: 
data from urine 
and serum samples of
early- and 
late-stage colorectal cancer patients; serum samples of colorectal cancer patients
 assayed on four SELDI chip-types (IMAC, H50, Q10 and CM10); 
  plasma samples from Limanda limanda fish; and urine samples of  
colorectal cancer patients analysed using both
SELDI and MALDI sample processing protocols. 
These values may be used as 
guidelines for choosing the sample size calculation parameters.
If your study involves profiling samples from 
late-stage disease
 or sera assayed on the IMAC chip, then the sample
size is probably a value close to that of the outer left contour. 
The urine profiling studies require more samples to detect differences
and 
the value of the contours to the right 
of grid may be used
as bounds.
You may also display parameters and sample size from your pilot study in this grid by 
inputting a vector (\code{observedPara}) of 
consensus values of the variance  and the corresponding difference,
or \code{rbind} several vectors of such parameters into a
matrix/dataframe if you have multiple pilots. 
}
\usage{
sampleSizeContourPlots(Z,m,DIFF,VAR,beta,alpha,observedPara,Indicator)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Z}{ the heterogeneity correction factor.}
  \item{m}{ the number of replicates. }
  \item{DIFF}{ the clinically important difference.}
  \item{VAR}{ the protein variance. }
  \item{beta}{ the power to estimate the clinically important difference. }
  \item{alpha}{ the significance level.}
\item{observedPara}{a vector or a matrix/dataframe (if there is more than one 
pilot study) containing the 
variance(s) and the clinically important difference(s) 
observed from your pilot data. The first element (column) of the vector (matrix) 
contains the observed variances, while the second contains the information on the
clinically important difference(s).}
\item{Indicator}{ an indicator variable. If it is set to 1, then the results of previous
proteomic profiling studies together with the results of your pilot study 
are included in the plot. If it is set to 0, it leads to a plot of only the latter.}
}
\value{
  Plots of grids of variance versus the clinically important differences with sample size 
contours superimposed on it.
}
\references{ 1. Nyangoma SO, Ferreira JA, Collins SI, Altman DG, Johnson PJ, and 
      Billingham LJ: Sample size calculations for planning clinical
      proteomic profiling studies using mass spectrometry. 
      Bioinformatics, 2009, Submitted

2. Nyangoma SO, Collins SI, Douglas GW,  Altman DG, Johnson PJ, and 
      Billingham LJ: Issues in sample size calculations for designing 
cancer proteomic profiling studies. 
      BMC Bioinformatics, 2009, Submitted}
\author{ Stephen Nyangoma }
\examples{

# The plot will be saved in your working directory.
# On the grid, we have plotted a number of sample sizes we computed from real life data.
#From these values you can gauge how many samples you may need.
# Fewer samples than 50, will not result in any meaningful estimation of differences.
# For late-stage cancer you  need the fewest samples, even from a very variable sample such as urine.
# You need more samples, over 200, to estimate differences between early stage cancer and 
#noncancer controls.
#etc.
m <- 2

DIFF <- seq(0.1,0.50,0.01) # 0.01

VAR <- seq(0.2,4,0.1)

beta <- c(0.90,0.80,0.70)

alpha  <-  1 - c(0.001, 0.01,0.05)/2

Corr <- c(0.70,0.90) #intraclass correlation also fixed

Z <- 2.6   # fix at 2.6 or use FisherInformation(???)

# You may input parameters from your pilot study. Suppose they are: 

#observedPara=c(1,0.4) #the variance you computed from pilot data

observedPara <- data.frame(var=c(0.7,0.5,1.5),DIFF=c(0.37,0.33,0.43))

# you may set these values to 0, if you do not have pilot data
#observedVAR=0
#observedDIFF=0
# in this case the values computed from my pilot studies (dotted on the plot) 
# may be used as guidelines.

Indicator <- 0 #1

sampleSizeContourPlots(Z,m,DIFF,VAR,beta,alpha,observedPara,Indicator)

}