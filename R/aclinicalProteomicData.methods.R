setMethod("show","aclinicalProteomicsData",
function(object) {
cat("clinical proteomics data")
cat("Type	:", class(object), "\n")
cat("rawSELDIdata:",  "\n")
cat("        matrix with:", paste(dim(object@rawSELDIdata)[1]),  "rows and",  paste(dim(object@rawSELDIdata)[2]),  "columns", "\n")
cat("phenotypicData:",  "\n")
cat("        matrix:", "containing information on", paste(dim(object@phenotypicData)[1]), "samples", "and", paste(dim(object@phenotypicData)[2]), "variables", "\n")
cat("        varLabels:", paste(dimnames(object@phenotypicData)[[2]]),  "\n")
cat("variableClass:", paste(object@variableClass), "\n")
cat("covariates:", paste(object@covariates), "\n")
cat("no.peaks:", paste(object@no.peaks), "\n")
}
)
