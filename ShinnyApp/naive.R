library(e1071)
library(RColorBrewer)


#load Data set
dat = read.csv("Datatransformed.csv", header = TRUE)

#formula for the model
myFormula <- IsTerrorist ~ MaritalStatus + EducationalAttainment +
  Military + MadrasAtraining + MentalIllness +  Islam + Sex + Age 

trainIndex <- 1:800

ro <- dat[1,]


#Creating Model 
model <- naiveBayes(myFormula, data = dat )

#predicting from model 
predict(model_nb, dat[-trainIndex,], type="raw")



str(dat$MentalIllness)
?level


head(dat$IsTerrorist)
head(predict(model_nb, dat[1:5,], type="raw"))

#changing the value of record named "ro"
a <- c("Male")
ro<-relevel(ro$Sex, "Female")
ro$Age <-as.factor(a)
str(ro$Age)
predict(model_nb, ro, type="raw")
summary(dat$Age)
ro <- dat[1,]
a <- "Female"
ro$Sex <-as.factor(a)
predict(model_nb, ro, type="raw")

ro <- dat[1,]
a <- "Female"
ro$Sex <-as.factor(a)

predict(model_nb, ro, type="raw")
plot(model_nb)
mosaicplot(dat) 

a <- c("Male")
ro$MaritalStatus[1] <-as.factor(a) 
str(ro$Sex)


a <- c("Male")
ro$Sex[1] <-as.factor(a) 
str(ro$Sex)

str(dat$Islam)
eapply(ro,typeof)
sapply(dat, class)

#Saving The model
save(model_nb, file = "navivebiTerro.RData")


# Create a color palette
palette <- brewer.pal(3, "Set2")

#Plotting
alpha <- predict(model_nb, dat, type="raw")
alpha[2,1]
plot( alpha[,2],1:912,col= "blue")
#plot( predict(model_nb, dat, type="raw"), 1:100, type="raw")



