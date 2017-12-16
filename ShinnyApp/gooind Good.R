dat = read.csv("Datatransformed.csv", header = TRUE)
library(party)
Filedata <-dat
str(dat)

set.seed(1234)#ramdom

ind <- sample(2, nrow(Filedata), replace=TRUE, prob=c(0.7,0.3))
trainData <- Filedata[ind==1,]
testData <- Filedata[ind ==2,]

myFormula <- IsTerrorist ~ MaritalStatus + EducationalAttainment +
  Military + MadrasAtraining + MentalIllness +  Islam + Sex + Age 

myFormula <- IsTerrorist ~ .
terro.ct1 <- ctree(myFormula, data = trainData)

nodes(terro.ct1,1)

p <- predict(terro.ct, newdata = testData, type = "prob")
nrow(terro.ct)
nrow(testData)

d <- testData$IsTerrorist
sapply(p)
cbind(new.airq, predict(air.ct, newdata = new.airq))
table(d,p)



#savinf the model

# Set working directory
#setwd("C:ight")

# Save the tree model
save(terro.ct, file = "Terro.RData")

# Save the training data
save(trainData, file = "train.RData")
#create Server 
server <- function(input,output){
  
}
