\name{pheno_urine}
\alias{pheno_urine}
\docType{data}
\title{A dataframe of phenotypic information}
\description{
 A dataframe containing the sample phenotypic information.
}
\usage{data(pheno_urine)}
\format{
  A data frame with 167 observations on the following 6 variables.
  \describe{
    \item{\code{SpectrumTag}}{a character/numeric vector of sample ID.}
    \item{\code{Tumor}}{a factor, with levels \code{c} and \code{n}, describing 
	the sample class}
    \item{\code{Age}}{a numeric vector of age.}
    \item{\code{Sex}}{a factor, with levels \code{female} and \code{male}, indicating gender.}
    \item{\code{stage}}{a factor,  with levels \code{early}, \code{late} and \code{normal},
indicating tumor stage.}
    \item{\code{Protein_conc}}{a numeric vector of urine concentration.}
  }
}
\source{
 Ward DG, Nyangoma S, Joy H, Hamilton E, Wei W, Tselepis C, Steven N, Wakelam 
MJ, Johnson PJ, Ismail T, Martin A: Proteomic profiling of urine for the detection of colon 
cancer. Proteome Sci. 2008,  16(6):19.
}
\references{
 Ward DG, Nyangoma S, Joy H, Hamilton E, Wei W, Tselepis C, Steven N, Wakelam 
MJ, Johnson PJ, Ismail T, Martin A: Proteomic profiling of urine for the detection of colon 
cancer. Proteome Sci. 2008,  16(6):19.
}
\examples{
data(pheno_urine)
}