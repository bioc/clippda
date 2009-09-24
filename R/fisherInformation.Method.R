#######
################################################################
################################################################
# Compute Fisher Information # information matrix
###############################################################
###############################################################

#clinicalInformation

setMethod("fisherInformation",signature=signature(Data="aclinicalProteomicsData"),

#`FisherInformation` <-

function(Data,..) {

#pheno data

PhenoInfo <- proteomicspData(Data)

# variables 

variables <- Data@covariates

# to compute b, Tumor is fixed at 1, all other variables varry from 0 to 1 and b=sum(all these marginal proportions)
# WARNING! This really depends on your coding of cases and controls. I have used a defoult codding in which normals 
# were coded as 1

if(length(variables) == 1) {

TOTAL <- length(PhenoInfo[,variables]) 

pi1 <- length(PhenoInfo[,variables[1]][PhenoInfo[,variables[1]]==sort(unique(PhenoInfo[,variables[1]]))[1]])/TOTAL
pi2 <-  length(PhenoInfo[,variables[1]][!(PhenoInfo[,variables[1]]==sort(unique(PhenoInfo[,variables[1]]))[1])])/TOTAL

FisherInfo <- 1/(pi1*pi2)
}
else
{
if(length(variables) == 2) {

TOTAL <- sum(xtabs(~.,PhenoInfo[,variables])) 

b <- sum(xtabs(~.,PhenoInfo[,c(variables[-1],variables[1])])[,2])/TOTAL


# to compute c, Gender is fixed at 1 (males), all other variables varry from 0 to 1 and c=sum(all these marginal proportions)

c <- sum(xtabs(~.,PhenoInfo[,c(variables[-2],variables[2])])[,2])/TOTAL


# to compute d, Protein_conc is fixed at 1, all other variables varry from 0 to 1 and d=sum(all these marginal proportions)

d <- sum(xtabs(~.,PhenoInfo[,c(variables[-1],variables[1])])[2,2])/TOTAL



#data.frame(x1=c(1,'b','c'),x2=c('b','b','d'),x3=c('c','d','c'))
 
Imat <- data.frame(x1=c(1,b,c),x2=c(b,b,d),x3=c(c,d,c))

FisherInfo <- solve(Imat)
}
else
{
TOTAL <- sum(xtabs(~.,PhenoInfo[,variables])) 

b <- sum(xtabs(~.,PhenoInfo[,c(variables[-1],variables[1])])[,,2])/TOTAL


# to compute c, Gender is fixed at 1 (males), all other variables varry from 0 to 1 and c=sum(all these marginal proportions)

c <- sum(xtabs(~.,PhenoInfo[,c(variables[-2],variables[2])])[,,2])/TOTAL


# to compute d, Protein_conc is fixed at 1, all other variables varry from 0 to 1 and d=sum(all these marginal proportions)

d <- sum(xtabs(~.,PhenoInfo[,c(variables[-3],variables[3])])[,,2])/TOTAL

# to compute e, both tumor and gender are fixed at 1, proteinconc varries from 0 to 1 and e=sum(all these marginal proportions)

e <- sum(xtabs(~.,PhenoInfo[,c(variables[-1],variables[1])])[,,2][,2])/TOTAL

#############################

# to compute f, both tumor and proteinconc are fixed at 1, gender varries from 0 to 1 and e=sum(all these marginal proportions)

f <- sum(xtabs(~.,PhenoInfo[,c(variables[-1],variables[1])])[,,2][2,])/TOTAL

# to compute g, both gender and proteinconc are fixed at 1, tumor varries from 0 to 1 and e=sum(all these marginal proportions)

g <- sum(xtabs(~.,PhenoInfo[,c(variables[-2],variables[2])])[,,2][,2])/TOTAL


Imat <- data.frame(x1=c(1,b,c,d),x2=c(b,b,e,f),x3=c(c,e,c,g),x4=c(d,f,g,d))

#data.frame(x1=c(1,'b','c','d'),x2=c('b','b','e','f'),x3=c('c','e','c','g'),x4=c('d','f','g','d'))
 
FisherInfo <- solve(Imat)
}

}

FisherInfo

}
)

########################################################################
########################################################################
###########
########################################################################
########################################################################









