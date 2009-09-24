\name{sampleSize3DscatterPlots}
\alias{sampleSize3DscatterPlots}
\title{A function for 3D display of sample size in a multi parameter space}
\description{
 Displays the sample sizes computed using the 
clinically important parameters. This plot
complements the contours plot.
}
\usage{
sampleSize3DscatterPlots(Z,m,DIFF,VAR,beta,alpha,observedDIFF,observedVAR,observedSampleSize,Angle,Indicator)
}
\arguments{
  \item{Z}{ the heterogeneity correction factor.}
  \item{m}{ the number of replicates}
  \item{DIFF}{ the clinically important difference.}
  \item{VAR}{ the protein variance. }
  \item{beta}{ the power to estimated the clinically important difference. }
  \item{alpha}{ the significance level.}
   \item{observedDIFF}{ the clinically important difference from your pilot data.}
  \item{observedVAR}{ the clinically important variance from your pilot data.}
  \item{observedSampleSize}{ the sample size estimated from your pilot data.}
\item{Angle}{ the angle for setting the orientation of the 3D-scatterplot.}
\item{Indicator}{ An indicator variable for controlling items to include in the plot.
If it takes the value 1, then the parameters and sample size of previous proteomic
profiling studies together with the results from your pilot study are plotted as points 
on the sample size calculation grid. If it is set to 0, 
then only the latter will be plotted.}
}
\value{
  It returns a 3D plot of sample size against the variance versus differences.
}
\references{ 1. Nyangoma SO, Ferreira JA, Collins SI, Altman DG, Johnson PJ, and 
      Billingham LJ: Sample size calculations for planning clinical
      proteomic profiling studies using mass spectrometry. 
      Bioinformatics, 2009, Submitted

2. Nyangoma SO, Collins SI, Douglas GW,  Altman DG, Johnson PJ, and 
      Billingham LJ: Issues in sample size calculations for designing 
cancer
      proteomic profiling studies. 
      BMC Bioinformatics, 2009, Submitted}
\author{ Stephen Nyangoma }
\seealso{sampleSizeContourPlots}
\examples{
# the plot will be saved in your working directory

Z <- 2.460018

m <- 2

######

DIFF <- seq(0.1,0.50,0.01) 
VAR <- seq(0.2,4,0.1)
beta <- c(0.90,0.80,0.70)
alpha  <-  1 - c(0.001, 0.01,0.05)/2

####

observedDIFF  <-  0.4
observedVAR  <-  1.0
observedSampleSize  <-  80

#########
# indicator for including results of previous studies on the 3D plot.

Indicator  <-  1

# sets the orientation of the 3D plot.
 
Angle  <-  60    

sampleSize3DscatterPlots(Z,m,DIFF,VAR,beta,alpha,observedDIFF,observedVAR,observedSampleSize,Angle,Indicator)
}