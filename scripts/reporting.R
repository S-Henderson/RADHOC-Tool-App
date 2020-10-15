# Build summary workbook
  
# Prep workbook

wb <- createWorkbook()

# Add sheets

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

# Write data

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

# Save workbook

saveWorkbook(
  wb,
  file = download_file
)