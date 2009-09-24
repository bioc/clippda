`phenoDataFrame` <-
function(PhenoData, variableClass) {

PhenoInfo=PhenoData

for(i in 1:length(variableClass)) { 

             if(variableClass[i] == "numeric")  {PhenoInfo[,i] <- as.numeric(as.vector(PhenoData[,i]))
}
    else       
            if(variableClass[i] == "character") {
    PhenoInfo[,i] <- as.character(as.vector(PhenoData[,i]))
                 } 
else
if(variableClass[i] == "factor") {

     PhenoInfo[,i] <- factor(as.character(as.vector(PhenoData[,i])))           
         } 
   
PhenoInfo = PhenoInfo             
} 

PhenoInfo 
}

