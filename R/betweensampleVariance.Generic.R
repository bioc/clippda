################################################################
################################################################
# between sample variance
###############################################################
###############################################################

setGeneric("betweensampleVariance",function (Data, ...) 
{
    rawData <- proteomicsExprsData(Data)
    no.peaks <- Data@no.peaks
    JUNK_DATA <- sampleClusteredData(rawData, no.peaks)
    JUNK_DATA = negativeIntensitiesCorrection(JUNK_DATA)
    LOG_DATA <- log2(JUNK_DATA)
    PhenoInfo <- proteomicspData(Data)
    averagedData = avedups(LOG_DATA, ndups = 2, spacing = 1, 
        weights = NULL)
    sampleNames = as.character(as.numeric(dimnames(averagedData)[[2]]))
    averagedData = data.frame(averagedData)
    names(averagedData) = sampleNames
    averagedDataNEW = averagedData[, as.character(PhenoInfo$SampleTag)]
    regressionData = data.frame(t(averagedDataNEW))
    betweenSampleVariance = NULL
    Parameter = NULL
    Parameter1 = NULL
    significance = NULL
    covariates <- Data@covariates
    for (i in 1:dim(regressionData)[2]) {
        regressionDataNew = data.frame(PhenoInfo[, covariates], 
            expression = regressionData[, i])
        out <- lm(expression ~ ., data = regressionDataNew)
        SUMMARY = summary(out)
        m = 2
        residual_stdError = SUMMARY$sigma
        Parameter = as.vector(SUMMARY$coefficients[, 1][2])
        Parameter1 = c(Parameter1, Parameter)
        significance = rbind(significance, round(SUMMARY$coefficients[, 
            4][-1], 3))
        betweenSampleVariance = c(betweenSampleVariance, (m * 
            residual_stdError)^2)
    }
    significance = data.frame(significance)
    mzMAT = matrix(rawData$Substance.Mass, no.peaks, dim(rawData)[1])
    mz = round(apply(mzMAT, 1, mean))
    row.names(significance) = mz
    names(betweenSampleVariance) = mz
    names(Parameter1) = mz
    list(betweensamplevariance = betweenSampleVariance, differences = Parameter1, 
        significance = significance)
    standardGeneric("betweensampleVariance")
}
)
