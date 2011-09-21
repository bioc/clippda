\name{ztwo}
\alias{ztwo}
\title{
A function to compute Z values when there are no covariates other than the cancer class
}
\description{
This function computes the effects of covariates (Z values) needed when designing a study 
in simple cases where the only covariate available is the cancer class. The Z values 
computed may, however,  be used in more complex setups, when additional covariates are expected.
}
\usage{
ztwo(x, y)
}
\arguments{
  \item{x}{expected proportion of cases
}
  \item{y}{
expected proportion of controls
}
}
\value{
produces the Z value
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
function ZvaluescasesVcontrolsPlots
}
\examples{
x=1/3;y=1-x
ztwo(x,y)
}
\keyword{ confounders }
\keyword{ Z values }% __ONLY ONE__ keyword per line
