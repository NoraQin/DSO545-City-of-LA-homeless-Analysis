library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(stringr)
library(lubridate)
library(ggmap)
library(rgdal)
library(tmap)
library(plotly)
library(DT)
library(highcharter)
library(RColorBrewer)
library(rgeos)
library(leaflet)


calls = read.csv("311_calls.csv")
crime = read.csv("crime_clean1.csv")
crime2 = read.csv("crime_clean.csv")
shelter = read.csv("shelters.csv")
hc16 = read.csv("homeless2016tract.csv")
hc17 = read.csv("homeless2017final.csv")
la_map = get_map("Los Angeles County", zoom = 10)


header <- dashboardHeader(
  title = "LA Homelessness"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("LA County", tabName = "la", icon = icon("th-list"),
             menuSubItem("Shleter", tabName = "shelter",icon = icon("home")),
             menuSubItem("Crime", tabName = "crime",icon = icon("exclamation-sign",lib = "glyphicon")),
             menuSubItem("311 calls", tabName = "calls",icon = icon("volume-control-phone", lib = "font-awesome")),
             menuSubItem("Homeless Counts", tabName = "hc",icon = icon("user"))),
    menuItem("Recommendations", tabName = "recommend",icon = icon("star")),
    menuItem("Contact Us", icon = icon("envelope"), 
             href = "https://www.lacity.org/")),
  br(),
  br(),
  br(),
  br(),
  br(),
  img(src ="usc_150.png",hight = 200, width = 220 )
)



body <- dashboardBody(
  tabItems(
    
    # 311 calls tab
    tabItem(
      tabName = "calls",
      fluidRow(
        column(
          width = 7,
          box(
            width = NULL, 
            plotOutput(outputId = "plotCalls1")
          ),
          box(
            width = NULL,
            sliderInput(
              inputId = "hour", label = "Hour",
              min = 0, max = 23, value = 0, step = 1, ticks = F, animate = T
            ),
            sliderInput(
              inputId = "month", label = "Month",
              min = 1, max = 12, value = 1, step = 1, ticks = F, animate = T
            ),
            radioButtons(
              inputId = "filterby", label = "Filtered by",
              choices = c("Hour", "Month"), selected = "Hour"
            )
          )
        ),
        column(
          width = 5,
          box(
            width = NULL,
            height = 350,
            plotOutput(outputId = "plotCalls2")
          ),
          box(
            width = NULL,
            height = 350,
            plotOutput(outputId = "plotCalls3")
          )
        )
      )
    ),
    
    # Homeless count tab
    tabItem(
      tabName = "hc",
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL, 
            plotOutput(outputId = "plotHc1")
          ),
          box(
            width = NULL,
            radioButtons(
              inputId = "year", label = "Year", 
              choices = c(2016, 2017), selected = 2017
            )
          )
        ),
        column(
          width = 6,
          box(
            width = NULL,
            plotOutput(outputId = "plotHc2")
          )
        )
      )
    ),
    
    # Crime tab
    tabItem(
      tabName = "crime",
      fluidRow(
        tabBox(
            width = 12, 
            tabPanel("Crime Type", 
                     width = NULL,
                     plotOutput(outputId = "crimetype"),
            checkboxGroupInput(
              inputId = "type", "TYPE OF CRIME",
              choices = c(
                "Assault",
                "Battery",
                "Intimate partner assault",
                "Theft",
                "Robbery",
                "Threats",
                "Rape",
                "Sexual Assault/Lewd",
                "Vandalism",
                "Burglary"
              ),
              selected = c("Assault",
                           "Battery")
            )
          ),
          tabPanel("Report Delay",
                   width = NULL,
                   plotOutput(outputId = "plotCrime2"),
                   selectInput(
                     inputId = "duration", label = "Reported after x days: ",
                     choices = c(10, 20, 30, 60), selected = 10)
        )
        )
      )
    ),
    
    # Shelter tab
    tabItem(
      tabName = "shelter",
      fluidRow(
        box(leafletOutput(outputId = "map", width="100%", height="100%"),width = 12),
        box(width = 8,
            selectInput(inputId = "shelterdata", 
                        label = "Choose a map to display",
                        choices = list ("Shelter", 
                                        "Shelter & Crime",
                                        "Shelter & 311 Calls"), 
                        selected = "Shelter")
        )
      )
    ),
    
    # Others tab
    tabItem(tabName = "recommend",
            fluidRow(
                     infoBox(h3("Job Placement Support"), 
                             icon = icon("handshake-o", lib = "font-awesome"),
                             width = 12,color = "light-blue", 
                             href = "https://www.usaid.gov/partnership-opportunities/ngo"),
                     infoBox(h3("Housing Placement"), 
                             icon = icon("home"),
                             width = 12,color = "yellow"),
                     infoBox(h3("Mental Health Support"), 
                             icon = icon("medkit", lib = "font-awesome"),
                             width = 12,color = "red",
                             href = "http://dmh.lacounty.gov/wps/portal/dmh"),
                     infoBox(h3("Female Support"),
                             icon = icon("female", lib = "font-awesome"),
                             width = 12,color = "purple",
                             href = "http://www.downtownwomenscenter.org/"),
                     infoBox(h3("Foster Care"),
                             icon = icon("child", lib = "font-awesome"),
                             width = 12,color = "lime")
                     # a(img(src ="NGO1.png",hight = 150, width = 500 ),
                     #   href = "https://www.lacity.org/")
            ))
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body,
  tags$script(HTML("$('body').addClass('sidebar-mini');"))
)



server <- function(input, output, session) {
  
  output$plotCalls1 = renderPlot ({
    plotdata <- reactive({switch (
      input$filterby,
      "Hour" = filter(calls, Hour == input$hour),
      "Month" = filter(calls, Month == input$month)
    )})
    ggmap(la_map) + 
      stat_density2d(data = plotdata(),
                     aes(x = LONGITUDE, y = LATITUDE,
                         fill = ..level..,
                         alpha = ..level..),
                     geom = "polygon") +
      theme(legend.position='none')+
      labs(x = "", y = "")+
      scale_fill_gradient(low = "white", high = "red")
  })
  
  output$plotCalls2 = renderPlot (height = 300, {
    calls %>%
      group_by(Hour) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = Hour, y = count)) +
      geom_line(color = "brown3")+
      scale_x_continuous(breaks = seq(0, 24, 1)) +
      ggtitle("Number of Calls per Hour") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_bw()
  })
  
  output$plotCalls3 = renderPlot (height = 300, {
    calls %>%
      group_by(Month) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = Month, y = count)) +
      geom_line(color = "brown3")+
      scale_x_continuous(breaks = seq(1, 12, 1)) +
      ggtitle("Number of Calls per Month") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_bw()
  })
  
  output$map <- renderLeaflet({
    ################## Assign
    redIcon <- makeIcon(
      iconUrl = "http://truephantom.com/wp-content/uploads/2017/04/blue-home-page-icon-png-16.png",
      iconWidth = 35, iconHeight = 35,
      iconAnchorX = 19, iconAnchorY = 19
    )
    callIcon <- makeIcon(
      iconUrl = "http://cdn.mysitemyway.com/icons-watermarks/flat-circle-white-on-red/bfa/bfa_phone/bfa_phone_flat-circle-white-on-red_512x512.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 19, iconAnchorY = 19
    )
    shel <- shelter %>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addMarkers(lat=shelter$LATITUDE,
                 lng=shelter$LONGITUDE, 
                 icon=redIcon,
                 clusterOptions = markerClusterOptions(),
                 popup= paste(shelter$CITY,
                              "<br><strong>Name: </strong>",shelter$NAME,
                              "<br><strong>Street Info: </strong>",shelter$ADDRLN1,
                              "<br><strong>Description: </strong>", shelter$DESCRIPTION
                 ))
    cal <- shelter %>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addMarkers(lat=shelter$LATITUDE,
                 lng=shelter$LONGITUDE, 
                 icon=redIcon,
                 popup= paste(shelter$CITY,
                              "<br><strong>Name: </strong>",shelter$NAME,
                              "<br><strong>Street Info: </strong>",shelter$ADDRLN1,
                              "<br><strong>Description: </strong>", shelter$DESCRIPTION
                 )) %>%
      addMarkers(lat=calls$LATITUDE,
                 lng=calls$LONGITUD,
                 icon=callIcon,
                 clusterOptions = markerClusterOptions(),
                 popup= paste(calls$NCNAME,
                              "<br><strong>Street Info: </strong>",calls$ADRESS,
                              "<br><strong>Requestsource: </strong>", calls$REQUESTSOURCE,
                              "<br><strong>CreatedDate: </strong>", calls$DATE.OCCURRED,
                              "<br><strong>ServiceDate: </strong>", calls$SERVICEDATE,
                              "<br><strong>Status: </strong>", calls$STATUS
                 ))
    crim <- shelter %>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addMarkers(lat=shelter$LATITUDE,
                 lng=shelter$LONGITUDE, 
                 icon=redIcon,
                 popup= paste(shelter$CITY,
                              "<br><strong>Name: </strong>",shelter$NAME,
                              "<br><strong>Street Info: </strong>",shelter$ADDRLN1,
                              "<br><strong>Description: </strong>", shelter$DESCRIPTION
                 )) %>%
      addMarkers(lat=crime$LATITUDE,
                 lng=crime$LONGITUD,clusterOptions = markerClusterOptions(),
                 popup= paste(crime$AREA.NAME,
                              "<br><strong>Victim_Sex: </strong>",crime$VICTIM.SEX,
                              "<br><strong>Victim_Age: </strong>", crime$VICTIM.AGE,
                              "<br><strong>Occured: </strong>", crime$DATE.OCCURRED,
                              "<br><strong>Reported: </strong>", crime$DATE.REPORTED,
                              "<br><strong>Weapon: </strong>", crime$CRIME.CODE.DESCRIPTION
                 ))
    ############## render plot
    switch(input$shelterdata,
           "Shelter" = shel,
           "Shelter & 311 Calls" = cal,
           "Shelter & Crime" = crim)
    
  })
  
  output$crimetype <- renderPlot ({
    crimedata <- crime2[crime2$crime.type.record %in% input$type,]
    crimedata %>%
      filter(crime.type.record != "NA") %>%
      ggplot(aes(x = TIME.OCCURRED, color = crime.type.record)) +
      geom_freqpoly() +
      labs(x ="Time",y ="Number of Crime", 
           color = "Type",title= "Crime Occurance by Hour")+
      scale_x_continuous(breaks = seq(0, 24, 1))+
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plotCrime2 = renderPlot({
    crime %>%
      filter(duration >= as.numeric(input$duration)) %>%
      arrange(desc(duration)) %>%
      group_by(crime.type.record, VICTIM.SEX) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = reorder(crime.type.record, count, sum), y = count, fill = VICTIM.SEX)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values=c("pink","lightblue")) +
      ggtitle(paste("Reporting after", input$duration, "days of crime occurance")) +
      xlab("Crime type") +
      ylab("Count") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "bottom") +
      scale_y_continuous(limits = c(0, 25)) +
      coord_flip()
  })
  
  ####   hc
  output$plotHc1 = renderPlot ({
    hcData <- reactive({
      switch(input$year, "2017" = hc17, "2016" = hc16)
    })
    
    hcData() %>% 
      arrange(-totPeople) %>%
      head(10) %>%
      gather("totSheltPeople", "totUnsheltPeople", 
             key = "temp", value = "count") %>%
      mutate(shelteredOrNot = ifelse(temp == "totSheltPeople", "Sheltered", "Unsheltered")) %>%
      group_by(Community, shelteredOrNot) %>%
      summarize(sum = sum(count))  %>%
      ggplot(aes(x = reorder(Community, sum), y = sum, fill = shelteredOrNot))+
      geom_bar(stat = "identity", width = 0.4) +
      coord_flip()+
      labs(title="Top 10 Homeless Communities",
           y = "",x = "",fill ="")+
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
            axis.text.y = element_text(size =15),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size=15)) +
      scale_y_continuous(limits = c(0, 4500))
  })
  
  output$plotHc2 = renderPlot({
    hcData <- reactive({
      switch(input$year, "2017" = hc17, "2016" = hc16)
    })
    
    hcData() %>% 
      mutate(youth = totESYouthSingYouth+totTHYouthSingYouth+totSHYouthSingYouth) %>%
      arrange(-totUnsheltPeople) %>%
      head(10) %>%
      gather("youth", "totUnsheltPeople", 
             key = "temp1", value = "count") %>%
      mutate(youthOrNot = ifelse(temp1 == "youth", "Youth", "Total")) %>%
      group_by(Community, youthOrNot) %>%
      summarize(sum = sum(count))  %>%
      ggplot(aes(x = reorder(Community, sum), y = sum, fill = youthOrNot))+
      geom_bar(stat = "identity", width = 0.4) +
      coord_flip()+
      theme_bw() +
      labs(title="Top 10 Unsheltered Homeless Communities",
           y = "",x = "",fill = "")+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
            axis.text.y = element_text(size =15),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size=15)) +
      scale_y_continuous(limits = c(0, 2500))
  })
}

shinyApp(ui, server)
