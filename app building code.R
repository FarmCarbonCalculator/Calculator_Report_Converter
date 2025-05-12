library(shiny)
library(jsonlite)
library(dplyr)
library(readr)
install.packages("DT")
library(DT)
install.packages("rsconnect")
rsconnect::setAccountInfo(name='calumfct',
                          token='BA92F7B94759D98CA9B239AECF8DF69A',
                          secret='9p8kFp+Pr2+0NafUESgvhiveGzVgS2EyPV1/WrVv')





ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      body {font-family: 'Poppins',sans-serif !important;}
      .app-logo { height: 120px; width: auto; margin-right: 20px; }
      .app-header { display: flex; align-items: center; gap: 20px; margin-bottom: 20px; }
      .app-title h1 { margin: 0; font-size: 28px; font-weight: 600; color: #2C3E50; }
      .app-title h4 { margin: 4px 0 0 0; font-size: 14px; font-weight: 300; color: #666; }
      .form-group { margin-right: 20px; }
      
      /* Style for centering elements */
      .radio-group { 
        display: flex; 
        gap: 20px; 
        justify-content: center;  /* Center the radio buttons horizontally */
        align-items: center;      /* Center the radio buttons vertically */
        flex-wrap: wrap;          /* Ensure they wrap if the window is resized */
      }
      .wellPanel { 
        display: flex; 
        justify-content: center;  /* Center content horizontally */
        align-items: center;      /* Center content vertically */
        flex-direction: column;   /* Stack elements vertically */
      }
    "))
  ),
  
  # Header Section with Logo and Title
  tags$div(class = "app-header",
           tags$img(src = "logo.png", class = "app-logo"),
           tags$div(class = "app-title",
                    tags$h1("Calculator Report Converter"),
                    tags$h4("Convert reports between Main, Equine, and Isle of Man calculators")
           )
  ),
  
  # Center the Sidebar and Main Panel
  fluidRow(
    column(width = 4, offset = 4,  # Centering the sidebar (4 + 4 offset = 12 total columns)
           wellPanel(  # Optional: adds a border around the input section
             
             # Create a div to group the radio buttons horizontally and center them
             div(class = "radio-group",  # Flexbox to group and center the radio buttons
                 radioButtons("input_type", "Input Format:",
                              choices = c("Main" = "main", "Equine" = "equine", "Man" = "man"),
                              selected = "main"
                 ),
                 
                 radioButtons("output_type", "Output Format:",
                              choices = c("Main" = "main", "Equine" = "equine", "Man" = "man"),
                              selected = "main"
                 )
             ),
             
             fileInput("json_file", "Upload JSON File", accept = ".json"),
             downloadButton("download_json", "Download Converted JSON")
           )
    )
  ),
  
  # Centered Output Text
  fluidRow(
    column(width = 8, offset = 2,
           verbatimTextOutput("status")
    )
  )
)



## load reference dataset
ids<- read.csv("ids.csv")

ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      body {font-family: 'Poppins',sans-serif !important;}
      .app-logo { height: 120px; width: auto; margin-right: 20px; }
      .app-header { display: flex; align-items: center; gap: 20px; margin-bottom: 20px; }
      .app-title h1 { margin: 0; font-size: 28px; font-weight: 600; color: #2C3E50; }
      .app-title h4 { margin: 4px 0 0 0; font-size: 14px; font-weight: 300; color: #666; }
      .form-group { margin-right: 20px; }
      
      /* Style for centering elements */
      .radio-group { 
        display: flex; 
        gap: 20px; 
        justify-content: center;  /* Center the radio buttons horizontally */
        align-items: center;      /* Center the radio buttons vertically */
        flex-wrap: wrap;          /* Ensure they wrap if the window is resized */
      }
      .wellPanel { 
        display: flex; 
        justify-content: center;  /* Center content horizontally */
        align-items: center;      /* Center content vertically */
        flex-direction: column;   /* Stack elements vertically */
      }
    "))
  ),
  
  # Header Section with Logo and Title
  tags$div(class = "app-header",
           tags$img(src = "logo.png", class = "app-logo"),
           tags$div(class = "app-title",
                    tags$h1("Calculator Report Converter"),
                    tags$h4("Convert reports between Main, Equine, and Isle of Man calculators")
           )
  ),
  
  # Center the Sidebar and Main Panel
  fluidRow(
    column(width = 4, offset = 4,  # Centering the sidebar (4 + 4 offset = 12 total columns)
           wellPanel(  # Optional: adds a border around the input section
             
             # Create a div to group the radio buttons horizontally and center them
             div(class = "radio-group",  # Flexbox to group and center the radio buttons
                 radioButtons("input_type", "Input Format:",
                              choices = c("Main" = "main", "Equine" = "equine", "Man" = "man"),
                              selected = "main"
                 ),
                 
                 radioButtons("output_type", "Output Format:",
                              choices = c("Main" = "main", "Equine" = "equine", "Man" = "man"),
                              selected = "main"
                 )
             ),
             
             fileInput("json_file", "Upload JSON File", accept = ".json"),
             downloadButton("download_json", "Download Converted JSON")
           )
    )
  ),
  
  # Centered Output Text
  fluidRow(
    column(width = 8, offset = 2,
           verbatimTextOutput("status")
    )
  )
)

server <- function(input, output) {
  modified_json <- reactiveVal(NULL)
  
  observeEvent({
    input$json_file
    input$input_type
    input$output_type
  }, {
    req(input$json_file)
    req(input$input_type)
    req(input$output_type)
    
    # Prevent no-op replacement
    if (input$input_type == input$output_type) {
      output$status <- renderText("⚠️ Input and output formats are the same. No conversion needed.")
      return()
    }
    
    tryCatch({
      # Load JSON
      json_data <- fromJSON(input$json_file$datapath, simplifyVector = FALSE)
      json_text <- toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
      
      # Match input column to output column
      from_col <- input$input_type
      to_col <- input$output_type
      
      # Check for missing values in mapping
      valid_ids <- ids %>% filter(!is.na(.data[[from_col]]), !is.na(.data[[to_col]]))
      
      for (i in seq_len(nrow(valid_ids))) {
        find_str <- valid_ids[[from_col]][i]
        replace_str <- valid_ids[[to_col]][i]
        json_text <- gsub(find_str, replace_str, json_text, fixed = TRUE)
      }
      
      # Convert back to structured JSON
      updated_json <- fromJSON(json_text, simplifyVector = FALSE)
      modified_json(updated_json)
      
      output$status <- renderText(paste0("✔️ Converted from ", from_col, " to ", to_col, " successfully."))
    }, error = function(e) {
      output$status <- renderText(paste("❌ Error:", e$message))
    })
  })
  
  output$download_json <- downloadHandler(
    filename = function() {
      paste0("converted_", input$output_type, ".json")
    },
    content = function(file) {
      req(modified_json())
      write_json(modified_json(), file, pretty = TRUE, auto_unbox = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)

rsconnect::deployApp(appName = "Calculator_Report_Converter", appDir = "C:/Users/calum/Desktop/Calculator Report Converter")
