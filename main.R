library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Patient Data Analyzer Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload input.csv",
                accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")),
      helpText("Ensure your CSV contains columns named: Gender, Fee, and Age."),
      hr(),
      h4("Analysis Parameters"),
      numericInput("taxRate", "Fee Tax Multiplier:", value = 1.05, step = 0.01),
      numericInput("seniorAge", "Senior Age Threshold:", value = 40, step = 1),
      checkboxInput("removeSecond", "Remove Second Row in Final View", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Original Data", 
                 br(),
                 h4("Original Data Frame Preview"),
                 DTOutput("origTable")
        ),
        tabPanel("Processed Data & Statistics", 
                 br(),
                 h4("Gender Frequency"),
                 verbatimTextOutput("genderFreq"),
                 br(),
                 h4("Updated Data Frame (with Tax & Age Group)"),
                 DTOutput("updatedTable"),
                 br(),
                 h4("Age Group Frequency"),
                 verbatimTextOutput("ageGroupFreq")
        ),
        tabPanel("Modified View", 
                 br(),
                 h4("Data After Row Removal"),
                 DTOutput("filteredTable")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive reader for uploaded CSV
  rawData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
  })
  
  # Reactive calculations and data transformations
  processedData <- reactive({
    df <- rawData()
    validate(
      need(all(c("Gender", "Fee", "Age") %in% names(df)), 
           "Uploaded CSV must contain 'Gender', 'Fee', and 'Age' columns.")
    )
    
    df$FeeWithTax <- round(df$Fee * input$taxRate, 2)
    df$AgeSqrt <- round(sqrt(df$Age), 2)
    df$AgeGroup <- factor(
      ifelse(df$Age >= input$seniorAge, "Senior", "Adult"),
      levels = c("Adult", "Senior")
    )
    return(df)
  })
  
  # Render Original Table
  output$origTable <- renderDT({
    req(rawData())
    datatable(rawData(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Render Gender Frequency
  output$genderFreq <- renderPrint({
    df <- rawData()
    req("Gender" %in% names(df))
    table(df$Gender)
  })
  
  # Render Updated Table
  output$updatedTable <- renderDT({
    df <- processedData()
    datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Render Age Group Frequency
  output$ageGroupFreq <- renderPrint({
    df <- processedData()
    req("AgeGroup" %in% names(df))
    table(df$AgeGroup)
  })
  
  # Render Filtered Table (Removing Second Row)
  output$filteredTable <- renderDT({
    df <- processedData()
    if (input$removeSecond && nrow(df) >= 2) {
      df <- df[-2, , drop = FALSE]
    }
    datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
}

shinyApp(ui, server)