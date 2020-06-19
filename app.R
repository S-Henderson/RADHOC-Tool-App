#---
# Author: Scott Henderson
# Last Updated: June 19, 2020
# Purpose: Summarize RADHOC query reports
#---

library("shiny")
library("shinythemes")
library("tidyverse")
library("readxl")
library("openxlsx")
library("DT")

#---------- DEFINE UI ----------

ui <- fluidPage(
  
  # Set custom theme -----
  theme = shinytheme("readable"),
  
  # App title -----
  titlePanel("RADHOC Tool v1"),
  
  # Sidebar layout -----
  sidebarLayout(
    
    # Sidebar panel -----
    sidebarPanel(
      
      # To fit company logo -----
      width = 3,
      
      #---------- IMPORT FILE ----------
      
      # File options -----
      fileInput(
        inputId = "import_file", 
        label = "Choose a CSV File",
        multiple = FALSE,
        accept = c(".csv")
      ),
      
      # Horizontal line -----
      tags$hr(),
      
      # Company Logo -----
      img(
        src = "360insights-logo.png", 
        height = 100, 
        width = 380
      ),
      
      # Horizontal line -----
      tags$hr(),
      
      # To download program type count -----
      downloadButton(
        "download_program_type_count",          
        label = "Download Program Type Count"
      ),
      
      # 2 breaks to to visually separate download buttons cleanly -----
      tags$br(),
      
      tags$br(),
      
      # To download status count -----
      downloadButton(
        "download_status_count",          
        label = "Download Status Count"
      ),
      
      # 2 breaks to to visually separate download buttons cleanly -----
      tags$br(),
      
      tags$br(),
      
      # To download client count -----
      downloadButton(
        "download_client_count",          
        label = "Download Client Count"
      ),
      
      # Horizontal line -----
      tags$hr()

    ),
    
    #---------- DISPLAY MAIN OUTPUTS ----------
    
    # Main panel -----
    mainPanel(
      
      # Set tabs -----
      tabsetPanel(
        
        # 1st tab -----
        tabPanel(
          title = "Summary", 
                 
          tableOutput(
            outputId = "program_count"
          ),
         
          tableOutput(
            outputId = "status_count"
          ),
          
          tableOutput(
            outputId = "client_count"
          ),
         
          textOutput(
            outputId = "count_records"
          ) 
         
        ), 
        
        # 2nd tab -----      
        tabPanel(
          title = "Table", 
          DT::dataTableOutput("data_table")
        )
        
      )
    )
  )
)

#---------- DEFINE SERVER LOGIC ----------

server <- function(input, output) {
  
  #---------- IMPORT DATA ----------
  
  # To retrieve data -----
  get_data <- reactive({
    
    input_file <- input$import_file
    
    req(input_file)
    
    read.csv(
      input_file$datapath, 
      fileEncoding = "UTF-8-BOM" # to solve header weird characters issue
    ) 
  
  })
  
  #---------- PROGRAM COUNT TABLE ----------
  
  # Data manipulation -----
  df_program_type_count <- reactive({
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
  
  # Output -----
  output$program_count <- renderTable({
    
    df_program_type_count()
    
  })
  
  #---------- STATUS COUNT TABLE ----------
  
  # Data manipulation -----
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
        `Count` = n(),               # Count
        `Sum` = sum(`Claim_Amount1`) # Sum
      )
    
  })
  
  # Output -----
  output$status_count <- renderTable({
    
    df_status_count()
    
  })
  
  #---------- CLIENT COUNT TABLE ----------
  
  # Data manipulation -----
  df_client_count <- reactive({
    get_data() %>% 
      group_by(
        `Client1`
      ) %>%
      summarize(
        `Count` = n(),               # Count
        `Sum` = sum(`Claim_Amount1`) # Sum
      )
    
  })
  
  # Output -----
  output$client_count <- renderTable({
    
    df_client_count()
    
  })
  
  #---------- COUNT RECORDS ----------
  
  output$count_records <- renderText({
    
    paste0("Number of records is: ", nrow(get_data()))
    
  })
  
  #---------- ENTIRE TABLE ----------
  
  output$data_table <- DT::renderDataTable({
    
    get_data()
    
  })

  #---------- DOWNLOAD DATA ----------
  
  # Program type count data -----
  output$download_program_type_count <- downloadHandler(
    
    # File name -----
    filename = function() { 
      
      paste0("RADHOC Program Type Count Export - ", 
            Sys.Date(),
            format(Sys.time(), " %H_%M_%S"), # prefix space for clean name
            ".csv")
    },
    
    # Write data -----
    content = function(file) {
      
      write.csv(
        df_program_type_count(),
        file,
        row.names = FALSE # to remove row index
      ) 

  })
  
  # Status count data -----
  output$download_status_count <- downloadHandler(
    
    # File name -----
    filename = function() { 
      
      paste0("RADHOC Status Count Export - ", 
             Sys.Date(),
             format(Sys.time(), " %H_%M_%S"), # prefix space for clean name
             ".csv")
    },
    
    # Write data -----
    content = function(file) {
      
      write.csv(
        df_status_count(),
        file,
        row.names = FALSE # to remove row index
      )
      
  })
  
  # Client count data -----
  output$download_client_count <- downloadHandler(
    
    # File name -----
    filename = function() { 
      
      paste0("RADHOC Client Count Export - ", 
             Sys.Date(),
             format(Sys.time(), " %H_%M_%S"), # prefix space for clean name
             ".csv")
    },
    
    # Write data -----
    content = function(file) {
      
      write.csv(
        df_client_count(),
        file,
        row.names = FALSE # to remove row index
      )
      
    })

}

#---------- CALL APP ----------

shinyApp(
  ui = ui, 
  server = server
)