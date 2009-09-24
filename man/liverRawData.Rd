\name{liverRawData}
\alias{liverRawData}
\docType{data}
\title{ A dataframe of the protein expression data, peak information and
 sample information}
\description{
 A dataframe of the protein expression data, peak information,
 sample information (e.g. sample ID, stage, gender, etc.). This is the "raw \code{.csv}" files from the 
Biomarker wizard. 
}
\usage{data(liverRawData)}
\format{
  A data frame with 14098 observations on the following 6 variables.
  \describe{
    \item{\code{SampleTag}}{a numeric vector of sample ID.}
    \item{\code{CancerType}}{a factor, with levels \code{c} and \code{n}, indicating 
      cancer class}
    \item{\code{Spectrum}}{a numeric vector indicating the experimental run.}
    \item{\code{Peak}}{a numeric vector identifying a peak.}
    \item{\code{Intensity}}{a numeric vector of expression values.}
    \item{\code{Substance.Mass}}{a numeric vector indicating the m/z value.}
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
data(liverRawData)
}