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

