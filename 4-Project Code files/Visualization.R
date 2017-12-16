test1<- NULL

# Read CSV into R
MyData <- read.csv(file="Dataper.csv", header=TRUE, sep=",")
MyData<-data
dim(MyData)
names(MyData)
str(MyData)
summary(MyData$Age)

hist(newXa)
newXa <- sapply(MyData$Age, as.numeric)
library(plotly)
head(MyData)
str(MyData)
a$IsTerrorist <- NULL



plot(x = MyData$Age, y =MyData$IsTerrorist
     ,col = "skyblue", xlab="Age",
     ylab="Isterorist" )
     
kmeans.result <- kmeans(a, 3)




# Graph between Age and IsTerrorist
barchart(Age ~ IsTerrorist,
         data = MyData, col ="skyblue")



#Visualing 1 Categoratal Data
#pie chart

## freguency barchart
Military <- MyData$Madrasa.training
MyData$Military["No"]

plot(
  x = MyData$IsTerrorist,horiz = FALSE, main="Is Terrorist ",col=c("orange","red","blue","green"))

data$Attended.terrorist.training.camp
table(data$Age)
table(data$MaritalStatus,data$IsTerrorist)

)
# Graph between Have.Weapons and IsTerrorist
ggplot(
  data=MyData,
  
       aes(x=Have.Weapons ,fill=IsTerrorist))+
  geom_bar()+
  ggtitle("                                 Education Level and Terrorist")
  
)
table(MyData)
pie(x = table(data$Educational.attainment),clockwise = TRUE)

#
  ggplot(data=MyData,
         aes(x= "", fill = Educational.attainment))+
  geom_bar()+
  coord_polar(theta = "y")+
  ggtitle("                                  The Education OF People")
  
 # 2 Categorial
#  group Frequency Bar chartr()
 
    ggplot(data=MyData,
   aes(x = Age, fill = IsTerrorist))+
      geom_bar(position = "dodge")+
      ggtitle("a")+
      scale_fill_discrete(labels = c("Noo","Yess"))
    
    #//Graph
    ggplot(data=MyData,
           aes(x = Age, fill = IsTerrorist))+
      geom_bar()+
      ggtitle("a")+
      scale_fill_discrete(labels = c("Noo","Yess"))
    
    # TWO Numric Vrial
    #Bivarte box plot
    ggplot(data=MyData,
           aes(x = Age, fill = IsTerrorist))+
      geom_bar()+
      facet_wrap(
      facets = ~ Sex) + 
      ggtitle("a")
    // couse team timestamp()
    
    
    
    