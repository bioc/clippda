setMethod("proteomicspData",signature=signature(Data="aclinicalProteomicsData"),

function (Data, ...) 
{    
varclass <- Data@variableClass
    PhenoInfo <- data.frame(Data@phenotypicData)
       no.variables <- length(varclass)
         for(i in 1:no.variables) {
            if(varclass[i] == "numeric") {
         PhenoInfo[,i]= as.numeric(as.vector(PhenoInfo[,i]))
} 

else

  if(varclass[i] == "character") {
    PhenoInfo[,i] <- as.character(as.vector(PhenoInfo[,i]))
 }

else

  if(varclass[i] == "factor") {
    PhenoInfo[,i] <- PhenoInfo[,i]
}

PhenoInfo <- PhenoInfo
}

PhenoInfo

}

)
