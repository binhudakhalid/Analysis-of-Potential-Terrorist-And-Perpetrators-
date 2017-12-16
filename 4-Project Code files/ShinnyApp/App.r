#libraries
library(e1071)
library(RColorBrewer)
library(shiny)

#Setting Current Directory or to the Shinnay App folder
setwd("F:/UBIT 7th/TheResearchPaper/myproject/ShinnyApp")

#Loading Naive bayer`s Model
load("navivebiTerro.RData")

#load Data set
dat = read.csv("Datatransformed.csv", header = TRUE)


ro <- dat[1,]
alpha <- predict(model_nb, dat, type="raw")


ui <- fluidPage(
  titlePanel("The Potential Terroroist Evaluator"),
  splitLayout(
    sidebarPanel(
      radioButtons("men", "MentalInnless:",
                   c("No" = "No",
                     "Yes" = "Yes"),
                   selected = "No"),
      
      radioButtons("mill", "Millitary:",
                   c("No" = "No",
                     "Yes" = "Yes"),
                   selected = "No"),
      
      radioButtons("iedu", "EducationalAttainment:",
                   c("Graduate" = "Graduate",
                     "College" = "College",
                     "School" = "School",
                     "No School" = "No School"),
                   selected = "Graduate"),
      
      radioButtons("im", "MaritalStatus:",
                   c("Married" = "Married",
                     "Single" = "Single"),
                   selected = "Single"),
      
      radioButtons("ia", "Age:",
                   c("[-Inf,17)" = "[-Inf,17)",
                     "[17,30)" = "[17,30)",
                     "[30,45)" = "[30,45)",
                     "[45,60)" = "[45,60)",
                     "[60, Inf)" = "[60, Inf)"),
                   selected = "[17,30)"),
      radioButtons("imad", "Madrasa Tranning:",
                   c("Yes" = "Yes",
                     "No" = "No"),
                   selected = "No"),
      radioButtons("ig", "Gender :",
                   c("Female" = "Female",
                     "Male" = "Male"),
                   selected = "Male"),
      radioButtons("ii", "Islam :",
                   c("Yes" = "Yes",
                     "No" = "No"),
                   selected = "No"),
      
      
      
      
      sliderInput(
        inputId = "petal.width",
        label = "Petal Width (cm)",
        min = 0.0,
        max = 2.5,
        step = 0.5,
        value = 3.5)),
    mainPanel(
      textOutput(
        outputId = "text"),
      plotOutput(
        outputId = "plot"))))

# Create server code
server <- function(input, output) {
  
  output$text = renderText({
    
    ro <- dat[1,]
    
    #setting mentallill
    menRaw <- input$men
    men <- c(menRaw)
    ro$MentalIllness[1] <-as.factor(men) 
    
    
    #setting 0Gender
    genderRaw <- input$ig
    gender <- c(genderRaw)
    ro$Sex[1] <-as.factor(gender) 
    
    
    
    #setting Military
    millRaw <- input$mill
    mill <- c(millRaw)
    ro$Military[1] <-as.factor(mill) 
    
    #setting education
    eduRaw <- input$iedu
    edu <- c(eduRaw)
    ro$EducationalAttainment[1] <-as.factor(edu) 
    
    
    #setting Marriage Status
    magRaw <- input$im
    mag <- c(magRaw)
    ro$MaritalStatus[1] <-as.factor(mag) 
    
    #setting Age
    ageRaw <- input$ia
    age <- c(ageRaw)
    ro$Age[1] <-as.factor(age) 
    
    #setting Madtasa
    madRaw <- input$imad
    mad <- c(madRaw)
    ro$MadrasAtraining[1] <-as.factor(mad) 
    
    #setting Islam
    iiRaw <- input$ii
    ii <- c(iiRaw)
    ro$MadrasAtraining[1] <-as.factor(ii) 
    
    
    
    
    
    #Row
    predictors <-ro
    
    
    # predictors <- ro <- dat[1,]
    
    # Make prediction
    prediction = predict(
      object = model_nb,
      newdata = predictors,
      type = "raw")
    
    # Create prediction text
    paste(
      "No & Yes",
      as.character(prediction))
    
    
  })
  
  output$plot = renderPlot({
    
    # Create a scatterplot colored by species
    plot( alpha[,2],1:912,col= "black", main="Terrorist Probalility" ,
          ylab="Persons",
          xlab="Probability")
   
  }
  )
}









shinyApp(
  ui= ui,
  server = server
)