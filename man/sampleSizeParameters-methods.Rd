\name{sampleSizeParameters-methods}
\docType{methods}
\alias{sampleSizeParameters-methods}
\alias{sampleSizeParameters,aclinicalProteomicsData,numeric,numeric-method}
\title{ ~~ Methods for Function sampleSizeParameters}
\description{
 Methods for function \code{sampleSize} are defined with: the class object, 
"aclinicalProteomicsData", intraclasscorr, and signifcut,  in the signature.
}
\section{Methods}{
\describe{
\item{Data = "aclinicalProteomicsData", intraclasscorr = "numeric", signifcut = "numeric"}{
\code{aclinicalProteomicsData} is a class object for the mass spectrometry data sets, which 
are in the same format as the raw data from the Biomarkers wizard software. 
It has slots of matrices of raw mass spectrometry and phenotypic data sets, a 
character variable for the classes of all the covariates in the phenotypic data 
matrix, a character variable for the covariates of interest, and numeric value for 
the number of peaks of interest. See also \code{aclinicalProteomicsData-class}.
\code{intraclasscorr} is a numeric variable for intraclass correlation, 
and \code{signifcut} is the
desired significance value for declearing a protein to be differentially expressed. 
}
}}
\keyword{methods}