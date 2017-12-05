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