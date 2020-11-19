library('dplyr')
library(tidyverse)
library(geodist)
library(leaflet)
library(shiny)
# rm(list=ls(all=TRUE))
# print(timestamp())

#³adowanie danych z wyborem tylko niezbêdnych kolumn
all_data<-read.csv("C:/Shiny_ports/ships.csv",dec=",", sep=",",header=T,stringsAsFactors=F) %>% 
    select(ship_type,SHIPNAME, SHIP_ID, LAT, LON, DATETIME)

# obliczanie odleg³oœci pomiêdzy punktami
all_data <- all_data %>% mutate(odleg = geodist(all_data, sequential = T, pad = T))

# dodanie kolumn zawieraj¹cych wspó³rzêdne geograficzne punktów z nastêopnego wiersza
all_data <- all_data %>% mutate(LAT_lead=lead(LAT),LON_lead=lead(LON))

# stworzenie tabeli unikalnych typów i nazw statków
unique_types_and_names <- unique(all_data[,1:2]) %>% arrange(.[,1],.[,2])


if (interactive()) {
  
ui <- fluidPage(
  
  titlePanel("Przebyte odleg³oœci"),
  
    sidebarLayout(
    
    sidebarPanel(
      
      # Input: Wybierz typ statku
      selectInput(inputId = "dataset",
                  label = "Wybierz typ statku:",
                  choices = unique(all_data$ship_type)),
      
      # Input: Nazwy statków z wybranej grupy
      selectInput(inputId = "obs",
                   label = "Nazwy statków z wybranej grupy:",
                   choices = NA)
          ),

    mainPanel(
      
      # Output: dynamiczny opis na temat przebytej odleg³oœci 
      textOutput('dynamiczny'),
      
      # Output: tabela HTML zawieraj¹ca dane punktów podró¿y (pocz¹tek = "A_...", koniec = "B_...")
      tableOutput("view"),
      
      # Output: mapa pokazuj¹ca punkty A (pocz¹tek podró¿y) i B (koniec)
      leafletOutput("map")#, width = "60%", height = "80%")
    ),
    position = c("right"),
    fluid = TRUE)
)


server <- function(input, output, session) {

  observeEvent(input$dataset, {
    updateSelectInput(session,'obs',choices=unique_types_and_names[,2][unique_types_and_names[,1]==input$dataset])
  })

    # tworzenie dynamicznego zestawu danych

      datasetInput <- reactive({

      temp_3 <- subset(all_data,all_data$SHIPNAME==input$obs) %>% arrange(desc(.[,6]))
      colnames(temp_3)[c(4:5,7:9)] <- c(paste('A_',tolower(colnames(temp_3)[4])),paste('A_',tolower(colnames(temp_3)[5])),'odleglosc','B_lat', 'B_lon')

        # maksymalna odleg³oœæ w ww. zbiorze danych
        odleglosci_max <- max(temp_3$odleglosc)

        # zbiór danych zawieraj¹cych maksymalne odleg³oœci
        max_wszystkie <- subset(temp_3,odleglosc==odleglosci_max)[1,]

  })
  
  # wyœwietlenie tabeli danych
  output$view <- renderTable({
    datasetInput()[,4:9]
  })

  # wyœwietlenie dynamicznwgo opisu
  output$dynamiczny <- renderText({

    paste('Przebyta odleg³oœæ wynios³a:', format(round(datasetInput()[1,7],2),big.mark = ' '), 'm', sep = ' ')
    
  })
  
  # wyœwietlenie mapy z punktami A i B
  output$map <- renderLeaflet({

    leaflet() %>%
    addTiles() %>%
    #dodanie opisów do punktow A i B na mapie
    addMarkers(lng=as.numeric(datasetInput()[1,4]), lat=as.numeric(datasetInput()[1,5]), popup=paste('A',as.numeric(datasetInput()[1,4]),as.numeric(datasetInput()[1,5]),sep=" | ")) %>%
    addMarkers(lng=as.numeric(datasetInput()[1,8]), lat=as.numeric(datasetInput()[1,9]), popup=paste('B',as.numeric(datasetInput()[1,8]),as.numeric(datasetInput()[1,9]),sep=" | "))
  })
}
}

shinyApp(ui, server)
