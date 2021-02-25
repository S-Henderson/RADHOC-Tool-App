# Author: Scott Henderson
# Last Updated: June 26, 2020

# Purpose: Summarize and visualize RADHOC query reports in an R Shiny app

# Input: RADHOC csv export
# Output: Summary tables of common data analysis

#--------------- LOAD LIBRARIES ---------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load("tidyverse", "readxl", "openxlsx", "shiny", "shinythemes", "DT")

#---------- DEFINE UI ----------

ui <- fluidPage(
  
  # Set custom theme -----
  
  theme = shinytheme("readable"),
  
  # App title -----
  
  titlePanel("RADHOC Tool v1"),
  
  #---------- SIDEBAR ----------
  
  # Sidebar layout -----
  
  sidebarLayout(
    
    # Sidebar panel -----
    
    sidebarPanel(
      
      # To fit company logo -----
      
      width = 3,
      
      #---------- IMPORT FILE ----------
      
      # File options -----
      
      fileInput(
        inputId = "uploaded_file", 
        label = "Choose a CSV File",
        multiple = FALSE,
        accept = c(".csv")
      ),
      
      # Horizontal line -----
      
      tags$hr(),
      
      # Company Logo -----
      
      img(
        src = "360insights_logo.png", 
        height = 100, 
        width = 380
      ),
      
      # Horizontal line -----
      
      tags$hr(),
      
      # Download button -----
      
      downloadButton(
        "download_summary",          
        label = "Download Summary"
      ),
      
      # Horizontal line -----
      
      tags$hr(),
      
      # Shiny Logo -----
      
      tags$a(
        href = "http://shiny.rstudio.com",
        "Made with Shiny"
      ),
      
      img(
        src = "Shiny_logo.png", 
        height = 70, 
        width = 200
      )
      
    ),
    
    #---------- DISPLAY MAIN OUTPUTS ----------
    
    # Main panel -----
    
    mainPanel(
      
      # Set tabs -----
      
      tabsetPanel(
        
        # Summary tab -----
        
        tabPanel(
          title = "Summary", 
          
          # Summary data -----
          
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
        
        # Table tab -----
        
        tabPanel(
          title = "Table", 
          DTOutput("data_table")
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
    
    input_file <- input$uploaded_file
    
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
  
  output$data_table <- renderDT({
    
    get_data()
    
  })

  #---------- DOWNLOAD DATA ----------
  
  # Summary data -----
  
  output$download_summary <- downloadHandler(
    
    # File name -----
    
    filename = function() { 
      
      paste0("RADHOC Summary Export - ", 
             Sys.Date(),
             format(Sys.time(), " %H.%M.%S"), # Prefix space for clean name
             ".xlsx")
    
    },
    
    # Write data -----
    
    content = function(download_file) {
      
      # Build summary workbook
      source("./scripts/reporting.R", local = TRUE)
      
    }
  )

}

#---------- CALL APP ----------

shinyApp(
  ui = ui, 
  server = server
)
