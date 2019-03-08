#Cargar librerias 
library(shiny)
library(xlsx)
library(stringr)
library(readxl)

#cargar archivo para el men?
menu=read.csv("menus.csv", sep = ";")

ui <- navbarPage(title = "App MINAE",#Titulo de la App
                 navbarPage(title = "Men?",#Esto genera un menu de navegacion
                            tabPanel(title = "Cuadros y Gr?ficos",#Primera pestana del menu
                                     sidebarPanel(
                                       selectInput(inputId = "cat",#titulos amarillos en el anexo estadistico (ARCHIVO PDF)
                                                   label = "Seleccione una categor?a:",
                                                   #c("",levels(...)) funciona para que no aparezca preseleccionada ninguna opcion 
                                                   choices = c("",levels(menu$categoria)),
                                                   selected= NULL,
                                                   multiple = F),
                                       br(),
                                       uiOutput("seg_sel"), #segunda selecci?n
                                       br(), 
                                       uiOutput("ter_sel") #tercera selecci?n
                                       
                                     ),
                                     mainPanel(
                                        tableOutput("cuadros") #esta es la salida para visualizar cuadros 
                                     )
                            )
                           
                 )
)

server <- function(input, output, session) {
  #segunda seleccion
  output$seg_sel <- renderUI({
      selectInput(inputId = "subcat",
                  label = "Seleccione una sub-categor?a:",
                  choices = menu$sub.categoria[menu$categoria %in% input$cat],
                  selected = NULL,
                  multiple = F)})
  #tercera selecci?n
  output$ter_sel <- renderUI({
    selectInput(inputId = "var",
                label = "Seleccione una variable:",
                choices = menu$variable[menu$sub.categoria %in% input$subcat],
                selected = NULL,
                multiple = F)})
  #Cuadros
  output$cuadros<-renderTable({
    req(input$var) #para evitar que read_excel de error
    loc="/cuadros.xlsx" #CAMBIAR UBICACI?N
    nombres=excel_sheets(loc)#obtener nombres de las hojas 
    leven_dist=adist(x = input$var,y = excel_sheets(loc)) #https://bit.ly/2TWGe9z
    num_hoja=which.min(leven_dist)#la distancia m?nima indica el par de nombres m?s similar 
    return(read_excel(loc,sheet =  num_hoja))
  })
    
}

shinyApp(ui,server) 