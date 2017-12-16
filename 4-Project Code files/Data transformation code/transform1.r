
library(XLConnect)
Database <- readWorksheet(loadWorkbook("Violent_Extremism_C1.xlsx"),sheet=1)
str(Database)
tail(Database)

df <- Database[,colSums(is.na(Database))<nrow(Database)]
tail(df)
tail(Database)

df <- Filter(function(x)!all(is.na(x)), df)
tail(df)

#--------------------------------------------------- is to factor
library(magrittr)
Database$isTerrorist %<>% factor
Database$IsTerrorist %<>% factor
#------------------------------------------------------co25 remoe
Database$Col25
#change column name
colnames(Database)[which(names(Database) == "Col25")] <- "MaritalStatus"
head(Database$MaritalStatus)
##fillinm MatitalStatus
sum(is.na(Database$MaritalStatus))

###########################################

Database <- save1

Database$MaritalStatus[is.na(Database$MaritalStatus)] <- rep( c("Married","unmarried"))
#--------------------------
saves4 <- Database
sum(is.na(Database$Plot))
#Removing plot

Database <- within(Database, rm(Plot))
sum(is.na(Database$Attended.terrorist.training.camp))

##Age
# Using cut
Database$Age <- cut(Database$Age, 
                       breaks = c(-Inf, 17, 30, 45, 60 ,Inf), 
                     #  labels = c("0-5 mnths", "6-11 mnths", "12-23 mnths", "24-59 mnths", "5-14 yrs", "adult"), 
                       right = FALSE)
#Attended.terrorist.training.camp
str(Database$Attended.terrorist.training.camp)
Database$Attended.terrorist.training.camp %<>% factor

levels(Database$Attended.terrorist.training.camp[levels(Pakistan)])
levels(factor(Database$Attended.terrorist.training.camp))  
Database <- save5
Database$Attended.terrorist.training.camp[Database$Attended.terrorist.training.camp == "Pakistan"] <- "Yes"
str(Database)

save6 <- Database
#----------------------------------Gender
sum(is.na(Database$Sex))

Database$Sex %<>% factor
str(Database)

#----------------------------ISlam
sum(is.na(Database$Islam))

Database$Islam %<>% factor
str(Database$Islam)
levels(Database$Islam)
saves7 <- Database
Database$Islam[Database$Islam == "?"] <- "No"
Database$Islam <- levels(droplevels(Database$Islam))
str(Database)
#---------------Ethnicity.National.Origin
is.na(Database$Madrasa.training)
Database$Madrasa.training %<>% factor
levels(Database$Madrasa.training)

Database$Madrasa.training[Database$Madrasa.training == ""] <- "Yes"  #no random
Database  <- Saves8
sum(is.na(Database$Madrasa.training))
summary(Database$Madrasa.training)
Database$Madrasa.training[is.na(Database$Madrasa.training)] <- rep( c("Yes","No")) #random
Database$Madrasa.training <- levels(droplevels(Database$Madrasa.training))
str(Database)
summary(Database$Madrasa.training)

Save10 <- Database
library(xlsx)
write.xlsx(Database, file, sheetName="Sheet1")
write.csv(Da, "filename.csv")
#-------------------------Mental.Illness 
summary(Database$Mental.Illness )

Database$Mental.Illness %<>% factor
levels(Database$Mental.Illness)

Database$Mental.Illness[Database$Mental.Illness == "yes"] <- "Yes"
Database$Mental.Illness <- levels(droplevels(Database$Mental.Illness))

str(Database)
save11 <- Database

#Educational.attainment 


Database$Educational.attainment %<>% factor
summary(Database$Educational.attainment)

Database$Educational.attainment[Database$Educational.attainment == "Less than high school degree"] <- "School"

levels(Database$Educational.attainment)

sum(is.na(Database$Educational.attainment))

summary(Database$Educational.attainment)

save13 <- Database

Database$Educational.attainment<- as.character(Database$Educational.attainment)


#==============
#MaritalStatus
Database$MaritalStatus %<>% factor
summary(Database$MaritalStatus)


Database$MaritalStatus <- as.character(Database$MaritalStatus)
Database$MaritalStatus [Database$MaritalStatus == "Unknown"] <- "Married"
Database$MaritalStatus[is.na(Database$MaritalStatus)] <- "Single"

Save14 <- Database

write.csv(Database, "Datatransformed.csv")

