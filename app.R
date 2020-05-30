library(shiny)
library(shinythemes)

library(readxl)
library(tidyverse)

library(DT)

#---------- DEFINE UI ----------

ui <- fluidPage(
  
  # Set custom theme
  theme = shinytheme("readable"),
  
  # App title
  titlePanel("RADHOC Tool v1"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      
      # Fit company logo
      width = 3,
      
      #---------- FILE INPUT ----------
      
      fileInput(inputId = "file1", 
                label = "Choose a CSV File",
                multiple = FALSE,
                accept = c(".csv")),
      
      # Horizontal line
      tags$hr(),
      
      # Company Logo
      img(src = "360insights-logo.png", 
          height = 100, 
          width = 380),
      
      # Horizontal line
      tags$hr(),
      
      # Download data
      downloadButton("download_program_count",          # Program count
                     label = "Download Program Count"),
      
      # 2 breaks to to visually separate download buttons cleanly
      tags$br(),
      
      tags$br(),
      
      # Download data
      downloadButton("download_status_count",          # Status count
                     label = "Download Status Count"),
      
      # Horizontal line
      tags$hr()

    ),
    
    #---------- DISPLAY MAIN OUTPUTS ----------
    
    mainPanel(
      
      # Set tabs
      tabsetPanel(
        
         # 1st tab
        tabPanel("Summary", 
                 
                 tableOutput(outputId = "program_count"),
                 
                 tableOutput(outputId = "status_count"),
                 
                 textOutput("count_records") 
                 
                 ), 
        
        # 2nd tab           
        tabPanel("Table", 
                 
                 DT::dataTableOutput("data_table")
                 
                 )
      
      )
    )
  )
)

#---------- DEFINE SERVER LOGIC ----------

server <- function(input, output) {
  
  #---------- IMPORT DATA ----------
  
  get_data <- reactive({
    
    input_file <- input$file1
    
    req(input_file)
    
    read.csv(input_file$datapath, 
             fileEncoding = "UTF-8-BOM") # Solves header weird characters issue
  
  })
  
  #---------- PROGRAM COUNT TABLE ----------
  
  # Data manipulation
  df_program_count <- reactive({
    get_data() %>% 
      mutate(
        `Program Type - Channel vs Consumer` = if_else(`Program_Type` == "Express Rebates",
                                                       "Consumer", # True
                                                       "Channel")  # False
      ) %>%
      group_by(
        `Program Type - Channel vs Consumer`
      ) %>%
      summarize(
        `Count` = n(),               # Count
        `Sum` = sum(`Claim_Amount1`) # Sum
     )
    
  })
  
  # Output
  output$program_count <- renderTable({
    
    df_program_count()
    
  })
  
  #---------- STATUS COUNT TABLE ----------
  
  # Data manipulation
  df_status_count <- reactive({
    get_data() %>% 
      mutate(
        `Clean_Status` = if_else(str_detect(`Status2`,"Paid"), 
                                 "Paid",    # True
                                 `Status2`) # False
      ) %>%
      group_by(
        `Clean_Status`
      ) %>%
      summarize(
        `Count` = n()
      )
    
  })
  
  # Output
  output$status_count <- renderTable({
    
    df_status_count()
    
  })
  
  #---------- ENTIRE TABLE ----------
  
  output$data_table <- DT::renderDataTable({
    
    get_data()
    
  })
  
  #---------- COUNT RECORDS ----------
  
  output$count_records <- renderText({
    
    #df_count_records <- get_data()
    
    paste("Number of records is:", nrow(get_data()))
    
  })
  
  #---------- DOWNLOAD DATA ----------
  
  # Program count data
  output$download_program_count <- downloadHandler(
    
    filename = function() { 
      
      paste0("RADHOC Program Count Export - ", 
            Sys.Date(),
            ".csv")
    },
    
    content = function(file) {
      
      write.csv(df_program_count(),        # Change here what to download here
                file,
                row.names = FALSE) # remove row index

  })
  
  # Status count data
  output$download_status_count <- downloadHandler(
    
    filename = function() { 
      
      paste0("RADHOC Status Count Export - ", 
             Sys.Date(),
             ".csv")
    },
    
    content = function(file) {
      
      write.csv(df_status_count(),        # Change here what to download here
                file,
                row.names = FALSE) # remove row index
      
    })

}

#---------- CALL APP ----------

shinyApp(
  ui = ui, 
  server = server
)