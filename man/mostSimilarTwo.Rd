\name{mostSimilarTwo}
\alias{mostSimilarTwo}
\title{ A function which indentifies two columns of a matrix, or dataframe,
 with the highest pairwise positive correlations}
\description{
A common practice in the 
analysis of repeated mass spectrometry data is to average the replicate expression 
values, a method which is only valid if there is some coherence 
in the peak information across 
replicates. 
The function \code{mostSimilarTwo} identifies the two columns of a 
matrix (or a dataframe) with the highest pairwise 
positive correlations. 
The most highly correlated  replicates contain the most similar compounds.
This function may also be used to reduce the number of spectra being analysed to two. }
\usage{
mostSimilarTwo(Mat)
}
\arguments{
  \item{Mat}{ A dataframe, with the columns being the variables of interest, for 
example the spectra.}
}
\details{
The main application of this function is in the pre-processing of mass spectrometry data.
In a mass spectrometry experiment, it often happens that there is mislabelling of samples, which results in
 some replicates being assigned to the wrong sample class. 
This function sifts through this data to 
identify the two spectra with the most coherent signal information between them. Thus, its
function has the potential to
help in reducing the number of false-positive discoveries.
Its other application is 
in the reduction of the number of replicates to two, which are then analysed using tools
for duplicate peak (or gene) expression data. 
}
\value{
  It returns a vector with two elements, being the column indices for the two most
correlated variables.
}
\references{ Ward DG, Nyangoma S, Joy H, Hamilton E, Wei W, Tselepis C, Steven N, Wakelam MJ, Johnson PJ, Ismail T, Martin A: Proteomic profiling of urine for 
the detection of colon cancer. Proteome Sci. 2008,  16(6):19}
\author{ Stephen Nyangoma}
\examples{
n <- 10

Mat <- data.frame(x1=rnorm(n, mean = 0, sd = 1),x2=rnorm(n, mean = 0, sd = 3),x3=rnorm(n, mean = 1, sd = 1),x4=
rnorm(n,mean=2,sd=2))

mostSimilarTwo(Mat)

}