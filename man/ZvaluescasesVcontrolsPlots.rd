\name{ZvaluescasesVcontrolsPlots}
\alias{ZvaluescasesVcontrolsPlots}
\title{
A function for ploting the odds of being a case vs control and their effects on adjustments for confounders
}
\description{
A function for ploting the odds of being a case vs control and their effects on adjustments for confounders. 
It may be useful in cases when it is envisaged that no confounders are expected.
It automatically plots the values of Z for the common experimental designs (e.g. 1:1, 1:3 and 1:4). You input
alarge number of hypothetical ratios of proportions of cases to controls.  It uses another function (ztwo), which
computes the Z values.
}
\usage{
ZvaluescasesVcontrolsPlots(probs)
}
\arguments{
  \item{probs}{
A vector of a large number of hypothetical ratios of proportions of cases to controls.
}
}
\value{
It returns a pdf plot of the odds of being a case vs control and their effects on adjustments for confounders.
}
\references{
Nyangoma SO, Ferreira JA, Collins SI, Altman DG, Johnson PJ, and
Billingham LJ: Sample size calculations for planning clinical
proteomic profiling studies using mass spectrometry.  (Working paper)

}
\author{
Stephen Nyangoma
}
\seealso{
function ztwo
}
\examples{
 # provide hypothetical proportions of cases vs controls
probs=seq(0,1,0.01)
ZvaluescasesVcontrolsPlots(probs)

}
\keyword{ plot }   
