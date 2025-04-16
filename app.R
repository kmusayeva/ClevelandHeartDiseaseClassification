library(shiny)
library(tidymodels)

# Load the saved logistic regression model
model <- readRDS("heart_disease_model.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Heart Disease Prediction (Logistic Regression)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Sex", choices = c("female", "male")),
      selectInput("chest_pain_type", "Chest Pain Type",
                  choices = c("typical angina", "atypical angina", 
                              "non-anginal pain", "asymptomatic")),
      numericInput("rest_blood_pressure", "Resting Blood Pressure", value = 120),
      numericInput("cholesterol", "Cholesterol", value = 200),
      selectInput("fasting_blood_sugar", "Fasting Blood Sugar",
                  choices = c("normal", "high")),
      numericInput("max_heart_rate", "Max Heart Rate", value = 150),
      selectInput("exercise_induced_angina", "Exercise Induced Angina",
                  choices = c("no", "yes")),
      numericInput("oldpeak", "Oldpeak (ST Depression)", value = 1.0, step = 0.1),
      numericInput("num_major_vessels", "Number of Major Vessels (0–3)", value = 0, min = 0, max = 3),
      selectInput("nuclear_stress_test", "Nuclear Stress Test",
                  choices = c("normal", "fixed defect", "reversible defect")),
      actionButton("predict", "Predict")
    ),
    
    mainPanel(
      h4("Prediction Result"),
      verbatimTextOutput("prediction"),
      verbatimTextOutput("probability")
    )
  )
)

# Define server
server <- function(input, output, session) {

  observeEvent(input$predict, {
    # Create new data for prediction
    new_data <- tibble(
      sex = as.factor(input$sex),
      chest_pain_type = as.factor(input$chest_pain_type),
      rest_blood_pressure = input$rest_blood_pressure,
      cholesterol = input$cholesterol,
      fasting_blood_sugar = as.factor(input$fasting_blood_sugar),
      max_heart_rate = input$max_heart_rate,
      exercise_induced_angina = as.factor(input$exercise_induced_angina),
      oldpeak = input$oldpeak,
      num_major_vessels = input$num_major_vessels,
      nuclear_stress_test = as.factor(input$nuclear_stress_test)
    )
    
    # Make prediction
    prediction <- predict(model, new_data, type = "prob")
    output$probability <- renderPrint({ paste("Predicted heart disease probability:", round(prediction$.pred_1, 3)) })
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)
