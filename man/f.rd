\name{f}
\alias{f}
\title{
A function to compute adjustments for the effct of covariates (Z values) for an experiment with a binary exposure and 
a binary  confounder
}
\description{
A function to compute the Z values when planning an experiment with a binary exposure and 
a binary  confounder. You input the probabilities of 3-cells of the resulting multinomial
distribution.
}
\usage{
f(x, y, z)
}
\arguments{
  \item{x}{Proportion of elements in cell 1 of a multinomial population with four cells.
}
  \item{y}{
Proportion of elements in cell 2 of a multinomial population with four cells.
}
  \item{z}{
Proportion of elements in cell 1 of a multinomial population with four cells. The z 
here is different from the Z which contains information on the effect of covariates and data imbalance
on sample size.
}
}
\value{
It returns a single real number (greater than or equal 2), representing Z.
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
The function ZvaluesformultinomialPlots
}
\examples{
# for a 1:1:1:1 experiment
x=.25;y=.25;z=.25

# compute Z
Z=f(x,y,z)
Z
## The function is currently defined as
function (x,y,z) {
Z=(1-x-z)*(x+y)/(2*(((1-x-z)*(1-x-y)*(1-y-z))-(1-x-y-z)^2))
Z
  }
}