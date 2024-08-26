server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      age = input$age,
      gender = as.numeric(input$gender),
      height = input$height,
      weight = input$weight,
      systolic_bp = input$systolic_bp,
      Diastolic_bp = input$Diastolic_bp,
      cholesterol = as.numeric(input$cholesterol),
      gluc = as.numeric(input$gluc),
      alco = as.numeric(input$alco),
      smoke = as.numeric(input$smoke),
      active = as.numeric(input$active),
      stringsAsFactors = FALSE
    )
    
    # Prediction using the logistic regression model
    prediction <- predict(model, df, type = "response")
    Output <- data.frame(Prediction = ifelse(prediction > 0.5, "At Risk", "Not at Risk"), Probability = round(prediction, 3))
    
    return(Output)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) { 
      isolate(datasetInput()) 
    } 
  })
  
}