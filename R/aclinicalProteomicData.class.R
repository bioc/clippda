setClass("aclinicalProteomicsData",representation(rawSELDIdata="matrix",phenotypicData="matrix",
variableClass="character",covariates="character",no.peaks="numeric"),
prototype(rawSELDIdata=matrix(0),phenotypicData=matrix(0),variableClass=as.character(0),covariates=as.character(0),no.peaks=0))

