#Cargar librerias 
library(shiny)
library(stringr)
library(readxl)
library(plotly)
library(rgdal)
library(sp)
library(leaflet)
library(shinythemes)
library(dplyr)
library(raster)
install.packages("sf")
library(sf) ## multipolygon
install.packages("mapview")
library(mapview)#mouse
install.packages("rmapshaper")
library(rmapshaper)
library(htmlTable)

#cargar archivo para el menú
menu=read.csv("menus.csv", sep = ";")

#cargar bases
load("base.Rdata")
load("bosques.Rdata")
load("aconservacion.Rdata")
load("vertederos.Rdata")
load("rellenos.Rdata")
load("areasprotegidas.Rdata")
load("smoking.df.Rdata")

Groupnames <- c("Area protegida", "Area de conservacion", "Bosque","Vertedero", "Relleno")


#Ubicación de cuadros

loc="C:/Users/Observatorio/Desktop/app/cuadros_prueba.xlsx" #CAMBIAR UBICACIÓN

ui <- navbarPage(title = "App MINAE",#Titulo de la App
                 navbarPage(title = "Menú",#Esto genera un menu de navegacion
                            tabPanel(title = "Cuadros y Gráficos",#Primera pestana del menu
                                     sidebarPanel(
                                       selectInput(inputId = "cat",#titulos amarillos en el anexo estadistico (ARCHIVO PDF)
                                                   label = "Seleccione una categoría:",
                                                   #c("",levels(...)) funciona para que no aparezca preseleccionada ninguna opcion 
                                                   choices = c("",levels(menu$categoria)),
                                                   selected= NULL,
                                                   multiple = F),
                                       br(),
                                       uiOutput("seg_sel"), #segunda selección
                                       br(), 
                                       uiOutput("ter_sel") #tercera selección
                                       
                                     ),
                                     mainPanel(
                                       tableOutput("cuadros"), #esta es la salida para visualizar cuadros
                                       br(),
                                       plotlyOutput("plot")
                                     )
                            ),
                            tabPanel("Mapa",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("varInput", label = h3("Variable"),
                                                     c(
                                                       unique(as.character(names(base)[-(1:3)]))
                                                     ),
                                                     selectize=FALSE),
                                         sliderInput("inputYear", "Year",
                                                     min = 2006, max = max(base$Anio),
                                                     value = 2006,step=1)
                                       ),
                                       mainPanel(leafletOutput(outputId = 'map', height = 
                                                                 800) 
                                       )))
                            
                 )
)

server <- function(input, output, session) {
  #segunda seleccion
  output$seg_sel <- renderUI({
    selectInput(inputId = "subcat",
                label = "Seleccione una sub-categoría:",
                choices = menu$sub.categoria[menu$categoria %in% input$cat],
                selected = NULL,
                multiple = F)})
  #tercera selección
  output$ter_sel <- renderUI({
    selectInput(inputId = "var",
                label = "Seleccione una variable:",
                choices = menu$variable[menu$sub.categoria %in% input$subcat],
                selected = NULL,
                multiple = F)})
  
  #Generar identificador
  ID<-reactive({
    nombres=excel_sheets(loc)#obtener nombres de las hojas 
    leven_dist<-adist(x = input$var,y = excel_sheets(loc)) #https://bit.ly/2TWGe9z
    return(which.min(leven_dist))#la distancia mínima indica el par de nombres más similar
  })
  
  #Cuadros
  output$cuadros<-renderTable({
    req(input$var) #para evitar que read_excel de error
    id<-ID()
    return(read_excel(loc,sheet = id))
  })
  
  
  #Plots
  plotselect <- reactive({
    req(input$var)
    id<-ID()
    if(id==1){
      p <- plot_ly(data=read_excel(loc,sheet = id), x = ~Año, y = ~Precipitación, name = 'Precipitación', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = ~Evapotranspiración, name = 'Evapotranspiración', mode = 'lines+markers') 
      return(p)
    }
    if(id==2){
      p<-  plot_ly(data=read_excel(loc,sheet = id), labels = ~Cobertura, values = ~2013, type = 'pie') %>%
        layout(title = '',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      return(p)
    }else NULL
  })
  
  output$plot <- renderPlotly({   
    dataplot = plotselect()
    print(dataplot)
  }) 
  
  ##Mapa
  
  selectedYear <- reactive({
    tmp <- smoking.df[!is.na(smoking.df$Anio), ] 
    tmp[tmp$Anio == input$inputYear, ]
  })
  
  #Paleta de colores para variable numerica
  pal2 <- colorQuantile(palette = "Greens", domain=NULL, n=3)
  
  #Mapa base
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lat=10, lng=-84, zoom=7) %>%
      
      
      #Mapa base
      addPolygons(data=areasprotegidas, 
                  popup = paste0("<strong> Areas: </strong>", 
                                 areasprotegidas$NOMBRE_),
                  fillOpacity = 1,
                  stroke = TRUE,
                  color = "blue",
                  group = Groupnames[1],
                  weight = 1)%>%
      #Capas 
      addPolygons(data=aconservacion, 
                  
                  popup = paste0("<strong> Areas: </strong>", 
                                 aconservacion$NOMBRE),
                  fillOpacity = 0.5,
                  stroke = TRUE,
                  color = "orange",
                  group = Groupnames[2],
                  weight = 1)%>%
      
      
      addPolygons(data=bosques, 
                  
                  popup = paste0("<strong> Bosque: </strong>", 
                                 bosques$NOMBRE),
                  fillOpacity = 1,
                  stroke = TRUE,
                  color = "green",
                  group = Groupnames[3],
                  weight = 1)%>%
      
      addCircles(data = vertederos, 
                 popup = paste0("<strong> Vertedero: </strong>", 
                                vertederos$Nvertedero),
                 fillOpacity = 5,
                 fill=TRUE,
                 stroke = TRUE, 
                 radius = 5000,
                 color = "red",
                 group = Groupnames[4],
                 weight = 5)%>%
      
      addCircles(data = rellenos, 
                 popup = paste0("<strong>Relleno: </strong>", 
                                rellenos$Relleno),
                 fillOpacity = 5,
                 fill=TRUE,
                 stroke = TRUE, 
                 radius = 5000,
                 color = "purple",
                 group = Groupnames[5],
                 weight = 5)%>%
      addMouseCoordinates()      %>% #para que muestre coordenadas 
      addLayersControl( #Seleccionar capas 
        overlayGroups = Groupnames,
        
        options = layersControlOptions(collapsed = TRUE))
    
  } #Para que no muestre las capas al inicio
  %>% hideGroup("Relleno")
  %>% hideGroup("Area protegida")
  %>% hideGroup("Area de conservacion")
  %>% hideGroup("Bosque")
  %>% hideGroup("Vertedero"))
  
  observe({ #Default de mapa base 
    
    mapselect <- input$varInput
    state_popup <- paste0("<strong>State: </strong>", #Popup 
                          selectedYear()$NAME_2,
                          "<br><strong> Year: </strong>",
                          selectedYear()$Anio,
                          "<br><strong> Dato: </strong>",
                          selectedYear()[[mapselect]]
    )
    
    leafletProxy("map", data = selectedYear()) %>%
      clearGroup(c("st.ate")) %>%
      addPolygons(group ="st.ate",fillColor = ~pal2(selectedYear()[[mapselect]]),
                  popup = state_popup,
                  color = "#BDBDC3",
                  fillOpacity = 1,
                  weight = 1
      )
    
  })
}

shinyApp(ui,server) 