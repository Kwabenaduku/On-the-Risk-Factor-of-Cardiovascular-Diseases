library(shiny)
library(shinythemes)
# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-15]

ui <- fluidPage(theme = shinytheme("superhero"),
                
                # Application title
                titlePanel("Cardiovascular Risk Predictor"),
                
                # Sidebar layout with input and output definitions
                sidebarLayout(
                  
                  # Sidebar panel for inputs
                  sidebarPanel(
                    HTML("<h3>Input Parameters</h3>"),
                    
                    numericInput("age", label = "Age (years)", value = 50, min = min(TrainSet$age), max = max(TrainSet$age)),
                    
                    selectInput("gender", label = "Gender", choices = c("Male" = 1, "Female" = 2)),
                    
                    numericInput("height", label = "Height (cm)", value = 170, min = min(TrainSet$height), max = max(TrainSet$height)),
                    
                    numericInput("weight", label = "Weight (kg)", value = 70, min = min(TrainSet$weight), max = max(TrainSet$weight)),
                    
                    numericInput("systolic_bp", label = "Systolic Blood Pressure (mmHg)", value = 120, min = min(TrainSet$systolic_bp), max = max(TrainSet$systolic_bp)),
                    
                    numericInput("Diastolic_bp", label = "Diastolic Blood Pressure (mmHg)", value = 80, min = min(TrainSet$Diastolic_bp), max = max(TrainSet$Diastolic_bp)),
                    
                    selectInput("cholesterol", label = "Cholesterol Level", choices = c("Normal" = 1, "Above Normal" = 2, "Well Above Normal" = 3)),
                    
                    selectInput("gluc", label = "Glucose Level", choices = c("Normal" = 1, "Above Normal" = 2, "Well Above Normal" = 3)),
                    
                    selectInput("alco", label = "Alcohol Intake", choices = c("No" = 0, "Yes" = 1)),
                    
                    selectInput("smoke", label = "Smoking Status", choices = c("No" = 0, "Yes" = 1)),
                    
                    selectInput("active", label = "Physical Activity", choices = c("No" = 0, "Yes" = 1)),
                    
                    actionButton("submitbutton", "Submit", class = "btn btn-primary")
                  ),
                  
                  # Main panel for displaying outputs
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Prediction",
                               tags$label(h3('Status/Output')),
                               verbatimTextOutput('contents'),
                               tableOutput('tabledata')
                      ),
                      tabPanel("FAQ",
                               h3("Frequently Asked Questions"),
                               h4("What is cardiovascular risk?"),
                               p("Cardiovascular risk refers to the likelihood of an individual developing cardiovascular diseases, which include heart attacks, strokes, and other conditions related to the heart and blood vessels."),
                               h4("How is the prediction made?"),
                               p("The prediction is made using a logistic regression model that considers several health-related factors, such as age, blood pressure, and cholesterol levels."),
                               h4("What do the results mean?"),
                               p("The prediction results indicate whether you are 'At Risk' or 'Not at Risk' for cardiovascular disease based on the provided input parameters.")
                      ),
                      tabPanel("About",
                               h3("About"),
                               p(HTML("
<strong><u>On the Risk Factors of Cardiovascular Diseases</u></strong><br><br>
This research project, titled <strong>On the Risk Factors of Cardiovascular Diseases</strong>, was conducted by Kwabena S. Duku and Nishat A. Mozid and presented at the Illinois State University research symposium under the mentorship of Professor Olcay Akman. The study focused on understanding the complex interplay of various factors contributing to cardiovascular diseases, which are among the leading causes of death globally.<br><br>

Our research team conducted an in-depth analysis of a comprehensive dataset comprising 70,000 individuals. This dataset included a wide range of demographic, biological, and health-related variables, such as age, gender, body weight, cholesterol levels, glucose levels, and both systolic and diastolic blood pressure measurements. Additionally, we considered lifestyle factors and pre-existing conditions that could potentially influence cardiovascular health.<br><br>

Using advanced statistical methods, specifically logistic regression, we developed a predictive model aimed at identifying individuals at risk of developing cardiovascular diseases. Our model incorporated the identified risk factors to estimate the likelihood of disease presence, achieving an accuracy rate of 73%. This level of accuracy underscores the model's potential utility in clinical settings for early detection and preventive interventions.<br><br>

The findings from this project contribute to the growing body of research focused on cardiovascular disease prevention and highlight the importance of targeted risk factor analysis in improving health outcomes. The success of this project also demonstrates the value of collaborative research efforts and the application of robust statistical techniques in addressing critical public health issues.<br><br>

For more details, visit our <a href='https://www.researchgate.net/publication/377358778_On_the_Risk_Factors_of_Cardiovascular_Diseases' target='_blank'>ResearchGate page</a>.
"))
                      )
                    )
                  )
                )
)
