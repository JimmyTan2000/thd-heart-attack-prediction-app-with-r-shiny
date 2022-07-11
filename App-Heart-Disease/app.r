# The User Interface(UI) is defined as below
ui <- fluidPage(
   theme = bs_theme(
    bg = "#101010", 
    fg = "#FDF7F7", 
    primary = "#ED79F9", 
    base_font = font_google("Luxurious Roman", local = TRUE),
    code_font = font_google("Luxurious Roman", local = TRUE)
  ),

  

  # Titel of the app
  titlePanel("Heart Disease Prediction"),

  # Layout for the input and output of the app
  sidebarLayout(

    # Die Definition der Eingabefelder auf der linken Seite
    sidebarPanel(
    
      
      h3("Biological Data:",align="left"),
      hr(style="height: 1px; background: black"),

      # Slider for age input
      # Minimum age is 18 and maximum age is 120
      # Predefined age is 45
      sliderInput(inputId = "age",
                  label = "Age",
                  min = 18,
                  max = 120,
                  value = 45
      ),
      
      # Sex as radio input
      radioButtons("sex", 
                   label="Sex",
                   choices = list("Female" = 0, 
                                  "Male"=1),
                                  selected = 1),

      # Chest pain type(cp) as select box
      selectInput("cp",label="Type of Chest Pain", 
                   choices = list("typical angina" = 0, 
                                  "atypical angina" = 1,
                                  "non-anginal pain" = 2, 
                                  "asymptomatic" = 3), 
                                  selected = 0
      ), 
      
      # Resting blood pressure (trestbps) as numerical input
      # Range from 100 to 200
      numericInput(inputId="trestbps", 
                    label="Resting Blood Pressure:", 
                    value = 150,
                    min=100,max=200,step=1
      ),
      
      # Slider input for serum cholesterol in mg/dl
               sliderInput(inputId = "chols",
                  label = "Serum Cholesterol in mg/dl",
                  min = 100,
                  max = 600,
                  value = 350
      ),
      
      # Radio buttons for fasting blood sugar 
      radioButtons("fbs", 
                   label="Fasting Blood Sugar > 120 mg/dl",
                   choices = list("False"= 0, 
                                  "True" = 1),
                                  selected = 1
      ),
      
      # Resting electrocardiographic results (restecg) as radio input
      radioButtons("restecg",
                   label="Resting Electrocardiographic Results",
                   choices = list("Normal" = 0,
                                  "Having ST-T wave abnormality" = 1,
                                  "Showing probable or definite left ventricular
                                  hypertrophy by Estes' criteria" = 2),
                                  selected = 0
      ),
      
      # Maximum Heart Rate Achieved (thalach) as numeric input
      numericInput(inputId="thalach", 
                   label="Maximum Heart Rate Achieved", 
                   value = 150,
                   min=0,max=280,step=1
      ),
      
      # Exercise Induced Angine (exang) as radio input
      radioButtons("exang",
                   label="Exercise Induced Angine",
                   choices = list("No" = 0,
                                  "Yes" = 1),
                                  selected = 0
      ),
      
      # ST depression induced by exercise relative to rest 
      # (oldpeak) as numeric input
      numericInput(inputId="oldpeak", 
                   label="ST Depression Induced by Exercise Relative to Rest", 
                   value = 0.0,
                   min=0.0,max=5.0,step=0.1
      ),
      
      # The slope of the peak exercise ST segment (slope) as radio input
      radioButtons("slope",
                   label="The Slope of the Peak Exercise ST Segment",
                   choices = list("Upsloping" = 0,
                                  "Flat" = 1,
                                  "Downsloping" = 2),
                                  selected = 0
      ),
      
      # Number of major vessels coloured by flourosopy (ca) as numericInput
      numericInput(inputId="ca", 
                   label="Number of Major Vessels Coloured by Flourosopy", 
                   value = 0,
                   min=0,max=4,step=1
      ),
      
      # Blood disorder called thalassemia (thal) as radio input
      radioButtons("thal",
                   label="Blood Disorder (Thalassemia)",
                   choices = list("NULL" = 0,
                                  "Fixed defect" = 1,
                                  "Normal blood flow" = 2,
                                  "Reversible defect" = 3),
                                  selected = 0
      ),
      ),
#ifelse(input$luxuskueche == FALSE, 0, 1)
      mainPanel(
      # Ausgabe der Prognose
      imageOutput("Images"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      textOutput("Prognose"),
      textOutput("Prob0"),
      textOutput("Prob1")
                )
      )
     ) 
            
 #########################################           
            
server <- function(input, output, session) {

       prognose <- reactive({

          X <- Data[,c("age","sex", "cp","trestbps", "chols", "fbs", "restecg",
                       "thalach", "exang", "oldpeak", "slope", "ca", "thal")] 
          # will add more variables when implemented
          
               
          X[1, "age"] <- input$age
          X[1,"sex"] <- as.factor(input$sex)
          X[1,"cp"] <- as.factor(input$cp)
          X[1,"trestbps"] <- input$trestbps
          X[1,"chols"] <- input$chols
          X[1,"fbs"] <- as.factor(input$fbs)
          X[1,"restecg"] <- as.factor(input$restecg)
          X[1,"thalach"] <- input$thalach 
          X[1,"exang"] <- as.factor(input$exang) 
          X[1,"oldpeak"] <- input$oldpeak 
          X[1,"slope"] <- as.factor(input$slope)
          X[1,"ca"] <- as.factor(input$ca)
          X[1,"thal"] <- as.factor(input$thal)
                
          
                
          prognosevektor <- predict(model,X)
          #prog <- prognosevektor[1]
          
          #prog <- round(prog, digits=0)
                
          #ifelse(prog < 0.5, 1, 0)
          
          prognosevektor
          })
          
        output$Prognose <- renderText({
                prognosevektor <- prognose()
                prog <- prognosevektor[1] 
                value <- ifelse(prog < 0.5, 1, 0)
                ifelse(value == 0, "Angiographic disease status: < 50% diameter narrowing, your heart is most likely healthy", "Angiographic disease status: > 50% diameter narrowing, your heart is most likely unhealthy")
                     
                                      
                                      })
        output$Images <- renderImage({
                prognosevektor <- prognose()
                prog <- prognosevektor[1]
                value <- ifelse(prog < 0.5, 1, 0)
                directory <- getwd()              
                images <- paste(directory, '/www/heart', value, '.gif',sep='' )
                list(src = images , alt = paste("Heart number", prog))
                                      },deleteFile=FALSE )
                                      
        output$Prob0 <- renderText({
                prognosevektor <- prognose()     
                probability0 <- prognosevektor[1]
                output <- paste("Probability for healthy heart is: ",probability0)              
                                   
                                   })
                                   
         output$Prob1 <- renderText({
                prognosevektor <- prognose()     
                probability1 <- prognosevektor[1,] [2]
                output <- paste("Probability for unhealthy heart is: ",probability1)              
                                   
                                   })
        
}


# Aufruf der App-Funktionen
###############

# shinyApp(ui = ui, server = server)
shinyApp(ui, server)
                                     
###############






