\name{liver_pheno}
\alias{liver_pheno}
\docType{data}
\title{A dataframe of phenotypic information}
\description{
 A dataframe containing the sample phenotypic information.
}
\usage{data(liver_pheno)}
\format{
  A data frame with 131 observations on the following 3 variables.
  \describe{
    \item{\code{SampleTag}}{a character/numeric vector of sample ID.}
    \item{\code{tumor}}{a factor, with levels \code{c} and \code{n}, describing 
	the  class of the samples.}
    \item{\code{sex}}{a factor, with levels \code{F} and \code{M,} describing the gender of the persons
from which the sample has been taken.}
  }
}
\source{
  Ward DG, Cheng Y, N'Kontchou G, Thar TT, Barget N, Wei W, Billingham LJ, Martin A, Beaugrand M, Johnson PJ: 
	Changes in the serum proteome associated with the development of hepatocellular carcinoma in hepatitis C-related 
cirrhosis. Br J Cancer. 2006, 94(2):287-92.
}
\references{
 Ward DG, Cheng Y, N'Kontchou G, Thar TT, Barget N, Wei W, Billingham LJ, Martin A, Beaugrand M, Johnson PJ: 
	Changes in the serum proteome associated with the development of hepatocellular carcinoma in hepatitis C-related 
cirrhosis. Br J Cancer. 2006, 94(2):287-92.
}
\examples{
data(liver_pheno)
}

