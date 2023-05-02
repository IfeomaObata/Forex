# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(rpart)
library(caret)

 
# Build model

model <- glm(BULL_BEAR~.,data=validation, family = binomial(link = "logit" ))


# Save model to RDS file
# saveRDS(model, "model.rds")

# Read in the RF model
#model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # Page header
                headerPanel("A Forex predictive model"),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input Parameters</h3>"),
                  

                  sliderInput("Open", "Open:",
                              min = 1.00, max = 2.00,
                              value = 1.144),
                  sliderInput("High", "High:",
                              min = 1.00, max = 2.00,
                              value = 1.148),
                  sliderInput("Low", "Low:",
                              min = 1.00, max = 2.00,
                              value = 1.140),
                  sliderInput("smaOpen", "smaOpen:",
                              min = 1.00, max = 2.00,
                              value = 1.144),
                  sliderInput("smaLOW", "smaLOW:",
                              min = 1.00, max = 2.00,
                              value = 1.140),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("OPEN",
               "HIGH",
               "LOW",
               "smaOPEN",
               "smaLOW"),
      Value = as.character(c(input$OPEN,
                             input$HIGH,
                             input$LOW,
                             input$smaOPEN,
                             input$smaLOW)),
      stringsAsFactors = FALSE)
    
    BULL_BEAR<- "BULL_BEAR"
    df <- rbind(df, BULL_BEAR)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    
    
    Output <- data.frame(Prediction =  predict(model,test, type= "response"))
                         
    print(Output)
  
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

