#---
# Author: Scott Henderson
# Last Updated: June 22, 2020
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
        src = "360insights_logo.png", 
        height = 100, 
        width = 380
      ),
      
      # Horizontal line -----
      tags$hr(),
      
      # To download program type count -----
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
        src = "Shiny_Logo.png", 
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
            format(Sys.time(), " %H.%M.%S"), # prefix space for clean name
            ".xlsx")
    },
    
    # Write data -----
    content = function(download_file) {
      
      #---------- CREATE WORKBOOK ----------
      
      # Create workbook -----
      
      wb <- createWorkbook()
      
      # Add sheets -----
      
      addWorksheet(
        wb, 
        sheetName = "Program Type Count", 
        tabColour = "#E6B8B7"             # Red
      )
      
      addWorksheet(
        wb, 
        sheetName = "Status Count",       
        tabColour = "#D8E4BC"             # Green
      )
      
      addWorksheet(
        wb, 
        sheetName = "Client Count",       
        tabColour = "#FFE699"             # Yellow
      )
      
      # Write data -----
      
      writeData(
        wb, 
        sheet = "Program Type Count", 
        x = df_program_type_count(), # Program Type Count
      )
      
      writeData(
        wb, 
        sheet = "Status Count", 
        x = df_status_count(),       # Status Count
      )
      
      writeData(
        wb, 
        sheet = "Client Count", 
        x = df_client_count(),       # Client Count
      )
      
      # Save workbook -----

      saveWorkbook(
        wb,
        file = download_file
      )

  })

}

#---------- CALL APP ----------

shinyApp(
  ui = ui, 
  server = server
)