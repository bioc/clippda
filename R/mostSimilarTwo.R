mostSimilarTwo <-
function(Mat) {

Corre=cor(Mat) # calculate pairwise correlations

# i) form a dataframe of the sretched correlation matrix and its rows and columns

CorreNew=data.frame(rows=rep(1:dim(Corre)[1],dim(Corre)[2]),
                                 columns=rep(1:dim(Corre)[2],each=dim(Corre)[1]),
                                 correlation=as.vector(Corre))

#ii) remove correlations between a vector and itself
CorreNew=CorreNew[!(CorreNew$correlation == 1),]

#iii) get the two columns of Mat with highest correlations

VECTS=as.vector(t(CorreNew[CorreNew$correlation==max(CorreNew$correlation),][1,])[-3]) # posive corr
              
VECTS

}

