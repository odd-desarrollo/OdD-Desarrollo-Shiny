#Cargar librerias 
library(shiny)
library(readxl)

ui <- fluidPage(  
  fileInput("file1", "Insertar archivo", accept = c(".xlsx",".xls")),
  textInput("file1sheet","Nombre de la hoja"),
  tableOutput("value")
)

server <- function(input, output) {
  
  sheets_name <- reactive({   
    if (!is.null(input$file1)) {
      return(excel_sheets(path = input$file1$datapath))  
    } else {
      return(NULL)
    }
  })
  
  output$value <- renderTable({
    if (!is.null(input$file1) && 
        (input$file1sheet %in% sheets_name())) {
      return(read_excel(input$file1$datapath, 
                        sheet = input$file1sheet))
    } else {
      return(NULL)
    }
  })
}

shinyApp(ui, server)



ui <- pageWithSidebar(
  headerPanel("CSV Data explorer"),
  sidebarPanel(
    
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    htmlOutput("varselect", inline=TRUE),
    
    selectInput("vars", "Select a variable:", choices=htmlOutput("varselect"),
                multiple = TRUE)
  ),
  
  mainPanel(
    dataTableOutput("table")
  )
)

server <- function(session,input, output) {
  
  Dataset <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  output$varselect <- renderUI({
    
  })
  
  observe({
    if (identical(Dataset(), '') || identical(Dataset(), data.frame()))
      return(NULL)
    
    updateSelectInput(session, inputId="vars", label="Variables to use:",
                      choices=names(Dataset()), selected=names(Dataset()))
  })
  
  output$table <- renderDataTable({
    if (is.null(input$vars) || length(input$vars)==0)
      return(NULL)
    
    return(Dataset()[,input$vars,drop=FALSE])
  })
}

shinyApp(ui, server)



