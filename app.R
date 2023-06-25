

library(tidyverse)
library(shinydashboard)
library(ordinal)
library(MASS)
library(shiny)


# import data
df <- read.csv("dataset/cleandf.csv",stringsAsFactors = TRUE)
df <- df[,c(2:11,73,74)]

selected <- dplyr::select(df, c(gender,department,college,age,stress_level))


train_set <- read.csv("train.csv", stringsAsFactors = TRUE)
test_set <- read.csv("test.csv", stringsAsFactors = TRUE)

test_set <- dplyr::select(test_set, -C.G.P)

train_set$f2 <- factor(train_set$f2,levels = c("Not Applicable",
                                         "Strongly Agree",
                                         "Agree","Neutral",
                                         "Disagree",
                                         "Strongly Disagree"), ordered = TRUE)

train_set$f3 <- factor(train_set$f3,levels = c("Not Applicable",
                                         "Strongly Agree",
                                         "Agree","Neutral",
                                         "Disagree",
                                         "Strongly Disagree"), ordered = TRUE)

train_set$f1 <- factor(train_set$f1,levels = c("Not Applicable",
                                         "Strongly Agree",
                                         "Agree","Neutral",
                                         "Disagree",
                                         "Strongly Disagree"), ordered = TRUE)

test_set$f2 <- factor(test_set$f2,levels = c("Not Applicable",
                                            "Strongly Agree",
                                            "Agree","Neutral",
                                            "Disagree",
                                            "Strongly Disagree"), ordered = TRUE)

test_set$f3 <- factor(test_set$f3,levels = c("Not Applicable",
                                            "Strongly Agree",
                                            "Agree","Neutral",
                                            "Disagree",
                                            "Strongly Disagree"), ordered = TRUE)

test_set$f1 <- factor(test_set$f1,levels = c("Not Applicable",
                                            "Strongly Agree",
                                            "Agree","Neutral",
                                            "Disagree",
                                            "Strongly Disagree"), ordered = TRUE)

#Importing model

model <- readRDS(file = './model.rda')
y_pred <- predict(model, newdata = test_set)
CCR <- readRDS(file = './CCR.rda')
MPE <- readRDS(file = './mpe.rda')





ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(width = 200,
                   sidebarMenu(menuItem("Data Table",
                                        tabName = "datatable",
                                        icon = icon('database')),
                               menuItem("Visualization",
                                        tabName = "plots",
                                        icon = icon('poll')),
                               menuItem("Model",
                                        tabName = "model",
                                        icon = icon('tachometer-alt'))
                               )),
  dashboardBody(tags$head(tags$style(HTML('.main-header.logo {font-weight:bold;'))),
                tabItems(
                  ## Data tab content starts here ##
                  tabItem(
                    "datatable", # data table,
                    box(
                        helpText('FUNAAB Student Depression Assement Questionairre was used to collect data from undergraduates students from the ten colleges.'), 
                        helpText('The data contains 239 observations and 12 attributes related to stdents demographic info, stress score, stress level and academic performance.'),
                        status = 'primary',
                        solidHeader = TRUE, 
                        width = 12, 
                        title = "FUNAAB Students Depression Assesment Survey Data",
                        "The data can be filtered by the selected variables below"),
                    # Create a new row in the UI for selectInputs
                    fluidRow(
                    column(6,
                        selectInput("gen", 
                                    width = "100%",
                                    "Filter by Gender:",
                                    c("All",
                                    unique(as.character(df$gender))))
                    ),
                    column(6,
                        selectInput("Stress", 
                                    width = "100%",
                                    "Filter by Depression level:",
                                    c("All",
                                    unique(as.character(df$stress_level))))
                     )
                    ),
                    # create a new row for data view Table
                    DT::dataTableOutput("dataTable")
                    ## Data tab content ends here ##
                  ),
                  ## Plots tab content starts here ##
                  tabItem(
                    "plots", # data table,
                    box(
                      "Please choose a variable from the drop-down menu below to see its distribution in a bar chart. You can change the variable at any time and the chart will update automatically. The chart shows the counts of each value of the selected variable. Hover over the bars to see more details."
                     ,
                      status = 'primary',
                      solidHeader = TRUE, 
                      width = 12, 
                      title = "FUNAAB Students Depression Assesment Survey Data",
                      "The data can be filtered by the selected variables below"),
                    column(3,
                                       selectInput("var", "Select variable",
                                                   choices = c( "college", "department", "gender","age", "stress_level")
                                                     )
                    ),
                    column(9,plotOutput("plot"))
                    
                    
                  ),
                  #Prediction tab content
                  tabItem('model',
                          # description box
                          box(status = "primary",
                              title = "Stress factors",
                              width = 12,
                              solidHeader = TRUE,
                              # Filter for stressors
                              fluidRow(
                                column(4,
                                       selectInput("f1", 
                                                   width = "100%",
                                                   "Enviromental Stress:",
                                                 choices =   c("Not Applicable",
                                                     "Strongly Agree",
                                                     "Agree","Neutral",
                                                     "Disagree",
                                                     "Strongly Disagree"),
                                                 )),
                                column(4,
                                       selectInput("f2", 
                                                   width = "100%",
                                                   "Enviromental Stress:",
                                                   c("Not Applicable",
                                                     "Strongly Agree",
                                                     "Agree","Neutral",
                                                     "Disagree"))),
                                 column(4, 
                                        selectInput("f3", 
                                                     width = "100%",
                                                     "Enviromental Stress:",
                                                     c("Not Applicable",
                                                       "Strongly Agree",
                                                       "Agree","Neutral",
                                                       "Disagree",
                                                       "Strongly Disagree")))
                              )
                              ),
                          # filter for demographic independent variables
                          box(title = "Demographic factors",
                              status = "primary",
                              width = 12,
                              solidHeader = TRUE,
                              
                              splitLayout(
                                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                cellWidths = c('10%','35%','10%','35%','10'),
                                         selectInput("gender",
                                                     width = "100%",
                                                     "Gender:",
                                                     c("Male","Female"
                                                     )),
                                div()
                                  ,
                                  
                                         selectInput("stress", 
                                                     width = "100%",
                                                     "Depression level:",
                                                     c("Very little","Mild","Moderate","Serious","Major"
                                                     )),
                                )),
                          #Box to display the Model results
                          box(title = 'Prediction',
                              status = 'success', 
                              solidHeader = TRUE, 
                              width = 6, height = 300,
                              div(h5('Prediction result:')),
                              verbatimTextOutput("value"),
                              div(h5('Predictions Accuracy')),
                              verbatimTextOutput("metrics", placeholder = TRUE),
                              actionButton('cal','Predict C.G.P', icon = icon('calculator'))),
                          #Box to display information about the model
                          box(title = 'Model explanation',
                              status = 'success', 
                              solidHeader = TRUE,
                              width = 6, height = 300,
                              helpText('The following model will predict the academic result of student based on their mental state due to their Gender Mental state, Enviromental, Intrapersonal and Enterpersonal stress factors conditions expirienced in the past 12 months.'),
                              helpText('The dataset used to train the model is the FNAAB Student Depression Assement data, collected from undergraduates students from the ten colleges. 
                     The data contains 239 observations and 12 attributes related to stdents demographic info, mental state and academic performance.'),
                              helpText(sprintf('The prediction is based on an ordinal regression supervised machine learning model. Furthermore the models deliver a prediction accuracy of (CCR) of %s and the mean prediction error is %s (MPE) of %s total number of observations.',
                                               round(CCR*100, digits = 0), round(MPE*100, digits = 0), 180)))
                  ) # end of model tabItem
                ) # end of tabItems
              ) # end of dashboard body
)

  
  
server <- function(input, output, session) {
  # Filter data based on selections
  output$dataTable <- DT::renderDataTable(DT::datatable({
    data <- df
    if (input$gen != "All") {
      data <- df[df$gender == input$gen,]
    }
    if (input$Stress != "All") {
      data <- df[df$stress_level == input$stress,]
    }
    data
  }))
  
  
  
  output$plot <- renderPlot({
    
    
    ggplot(selected, aes(x = get(input$var))) +
      geom_bar(aes(fill = gender)) +
      labs(title = "Frequency Distribution",
           subtitle = paste("You selected:", input$var),
           x = input$var,
           y = "Response") +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Center and increase title size
            plot.title = element_text(hjust = 0.5, size = 20),
            # Center and increase subtitle size
            plot.subtitle = element_text(hjust = 0.5, size = 16),
            # Increase x-axis label size
            axis.title.x = element_text(size = 20),
            # Increase y-axis label size
            axis.title.y = element_text(size = 14)
      )
    
  })
  
  
  #_____________________________
  #Prediction model
  #React value when using the action button
  a <- reactiveValues(result = NULL)
  
  observeEvent(input$cal, {
    #Copy of the test data without the dependent variable
    test_pred <- test_set
    
    # Data frame for the single prediction
    new_values <- data.frame(
      gender = as.factor(input$gender),
      stress = as.factor(input$stress),
      f1 = as.factor(input$f1),
      f2 = as.factor(input$f2),
      f3 = as.factor(input$f3)
    )

    
    #Include the values into the new data
    test_pred <- rbind(test_pred,new_values)
    
    #Single prediction using the randomforest model
    a$result <-  predict(model, 
                         newdata = test_pred[nrow(test_pred),], type = "class")
  })
  
  output$value <- renderPrint({
    #Display the prediction value
    print(a$result)
  })
  
  output$metrics <- renderText({
    #Display the CCR and MPA
    input$cal
    isolate(sprintf('Prediction accuracy is %s percent and mean misclassification error of %s percent', 
                    round(CCR*100, digits = 0),
                    round(MPE*100, digits = 0)))
  })
  
}

shinyApp(ui = ui, server = server)


