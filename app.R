library(shiny) 
library(shinydashboard)
library(ggplot2)
library(highcharter)


# Conditional icon for widget.
# Returns arrow up icon if cars left > cars entered 
# returns arrow down icon if cars entered > cars left 
# or returns minus sign when values equal
iconCond <- function(countIn, countOut ) {
  if (countOut > countIn ) {
    return(icon("arrow-circle-up"))
  } else {
    if (countOut < countIn) {
      return(icon("arrow-circle-down"))
    } else {
      return(icon("minus-circle"))
    }
  }
}

# Conditional color for widget
# Returns green color if cars left > cars entered 
# returns red color if cars entered > cars left 
# or returns blue when values equal
colorCond <- function(countIn, countOut ) {
  ifelse(countOut > countIn ,"green", ifelse(countOut < countIn,"red", "aqua") )
}

# Thresholds for ValueBox color
colorThreshold <- c(60,80)

# Conditional color for ValueBox
# Threshold is specified as two values between 0 and 100 (e.g. 60 and 80)
# Returns green when value is lower than first threshold
# yellow when between threshold values and red when above second threshold
colorCond2 <- function(value, threshold) {
 # if (!is.numeric(threshold))  stop("Threshold is wrongly specified. Please correct the mistake.")
  ifelse(value <= threshold[1] ,"green", ifelse(value > threshold[1] & value <= threshold[2],"yellow", "red") )
}


# Max number of spots available 
SpotsAvailable <- c("Parking Hala Stulecia" = 792,
                    "Nowy Targ" = 334,
                    "Renoma" = 612,
                    "ul. sw. Antoniego" = 200
                    )
  
# Get dataset from Wroclaw Open Data 
getDataSet <- function() {
  
  URL <- "http://www.wroclaw.pl/open-data/opendata/its/parkingi/parkingi.csv"
  df <- tryCatch(read.csv2(URL) , warning = function(e) NULL, error = function(e) NULL)
  #df <- read.csv2(URL)
  if (is.null(df)) return(df)
  # modify columns
  df$Nazwa <- as.character(df$Nazwa)
  df$Nazwa <- enc2utf8(df$Nazwa)
  df$Nazwa[grep("Antoniego", df$Nazwa)] <- "ul. sw. Antoniego"
  #df$Nazwa <- iconv(df$Nazwa, from ="", to="ASCII//TRANSLIT")
  #df$Nazwa <- enc2utf8(df$Nazwa)
  df$Czas_Rejestracji <- as.POSIXct(df$Czas_Rejestracji)
  df$Czas_pobrania <- format(Sys.time())
  return(df)
}

# Create header
header <- dashboardHeader(title = "Parkingi - Wroclaw")

# Create sidebar menu
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Przeglad", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Dane", tabName = "rawdata", icon = icon("database")),
    menuItem("Informacja", tabName = "info", icon = icon("info-circle"))
  ),
  br(),
  # Show countdown timer 
  div(style = "text-align:center", h4("Nastepna aktualizacja:"), h2(uiOutput("countDown"))
  )
)

# Create body
body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
      fluidRow(
        
        valueBoxOutput("valueBoxRenoma", 3),
        valueBoxOutput("valueBoxNowyTarg", 3),
        valueBoxOutput("valueBoxAntoniego", 3),
        valueBoxOutput("valueBoxHala", 3),
        infoBoxOutput("infoBoxRenoma", 3),
        infoBoxOutput("infoBoxNowyTarg", 3),
        infoBoxOutput("infoBoxAntoniego", 3),
        infoBoxOutput("infoBoxHala", 3)
      ),
      fluidRow(
        box(
          #plotOutput("chart_gg"),
          highchartOutput("chart_h"),
          width = 12, status = "info", solidHeader = F
        )
      )
    ),
    tabItem("rawdata",
      numericInput("maxrows", "Pokaz wybrana ilosc ostatnich rzedow", 20),
      verbatimTextOutput("rawtable"),
      downloadButton("downloadCsv", "Pobierz plik (CSV)")
    ),
    tabItem("info", "Empty content")
  )  
)

eventTime <- Sys.time() + 5*60


ui <- dashboardPage(
  # skin="black",
    header,
    sidebar,
    body
)
  
server <- function(input, output, session) {
    
  # Reactive timer for fetching data and reseting clock timer (set to 5 minutes = 300 seconds)
  autoInvalidate = reactiveTimer(300 * 1000,session)
  
  # 
  getData <-  reactive({
    autoInvalidate()
    validate(
      need(!is.null(getDataSet()), "Cannot fetch the data. Try again later.")
    )
    getDataSet()
  })
  
  # info box #1 
  output$infoBoxRenoma <- renderInfoBox({
    
    dataset <- getData()
    parking <- "Renoma"
    inCnt <- tail(dataset$Liczba_Poj_Wjezdzajacych[dataset$Nazwa == parking], n = 1)
    outCnt <- tail(dataset$Liczba_Poj_Wyjezdzajacych[dataset$Nazwa == parking], n = 1)
    currentTime <- strftime(tail(dataset$Czas_Rejestracji[dataset$Nazwa == parking], n = 1), format = "%H:%M:%S")
    col <- colorCond(inCnt, outCnt)
    icn <- iconCond(inCnt, outCnt)
    sub <- em(paste("Aktualizacja: ", currentTime))
    val <- paste(inCnt," / ", outCnt)
    
    infoBox(title = "Wjechalo / Wyjechalo", subtitle = sub, value = val, icon = icn, color = col, fill = F)
  })
  
  # info box #2 
  output$infoBoxNowyTarg <- renderInfoBox({
    
    dataset <- getData()
    parking <- "Nowy Targ"
    inCnt <- tail(dataset$Liczba_Poj_Wjezdzajacych[dataset$Nazwa == parking], n = 1)
    outCnt <- tail(dataset$Liczba_Poj_Wyjezdzajacych[dataset$Nazwa == parking], n = 1)
    currentTime <- strftime(tail(dataset$Czas_Rejestracji[dataset$Nazwa == parking], n = 1), format = "%H:%M:%S")
    col <- colorCond(inCnt, outCnt)
    icn <- iconCond(inCnt, outCnt)
    sub <- em(paste("Aktualizacja: ", currentTime))
    val <- paste(inCnt," / ", outCnt)
    
    infoBox(title = "Wjechalo / Wyjechalo", subtitle = sub, value = val, icon = icn, color = col, fill = F)
  })
  
  # info box #3
  output$infoBoxAntoniego <- renderInfoBox({
    
    dataset <- getData()
    parking <- "ul. sw. Antoniego"
    inCnt <- tail(dataset$Liczba_Poj_Wjezdzajacych[dataset$Nazwa == parking], n = 1)
    outCnt <- tail(dataset$Liczba_Poj_Wyjezdzajacych[dataset$Nazwa == parking], n = 1)
    currentTime <- strftime(tail(dataset$Czas_Rejestracji[dataset$Nazwa == parking], n = 1), format = "%H:%M:%S")
    col <- colorCond(inCnt, outCnt)
    icn <- iconCond(inCnt, outCnt)
    sub <- em(paste("Aktualizacja: ", currentTime))
    val <- paste(inCnt," / ", outCnt)
    
    infoBox(title = "Wjechalo / Wyjechalo", subtitle = sub, value = val, icon = icn, color = col, fill = F)
  })
  
  # info box #4
  output$infoBoxHala <- renderInfoBox({
    
    dataset <- getData()
    parking <- "Parking Hala Stulecia"
    inCnt <- tail(dataset$Liczba_Poj_Wjezdzajacych[dataset$Nazwa == parking], n = 1)
    outCnt <- tail(dataset$Liczba_Poj_Wyjezdzajacych[dataset$Nazwa == parking], n = 1)
    currentTime <- strftime(tail(dataset$Czas_Rejestracji[dataset$Nazwa == parking], n = 1), format = "%H:%M:%S")
    col <- colorCond(inCnt, outCnt)
    icn <- iconCond(inCnt, outCnt)
    sub <- em(paste("Aktualizacja: ", currentTime))
    val <- paste(inCnt," / ", outCnt)
    
    infoBox(title = "Wjechalo / Wyjechalo", subtitle = sub, value = val, icon = icn, color = col, fill = F)
  })
  
  # value box #1
  output$valueBoxRenoma <- renderValueBox({
    
    dataset <- getData()
    parking <- "Renoma"
    currentAvailibility <- tail(dataset$Liczba_Wolnych_Miejsc[dataset$Nazwa == parking], n = 1)
    percentageTaken <- round(1 - as.numeric(currentAvailibility / SpotsAvailable[parking]) ,3) * 100
    col <- colorCond2(percentageTaken, colorThreshold ) 
    
    valueBox(subtitle = parking,  icon = icon("car"), value = currentAvailibility, color =  col)
  })
  
  # value box #2
  output$valueBoxNowyTarg <- renderValueBox({
    
    dataset <- getData()
    parking <- "Nowy Targ"
    currentAvailibility <- tail(dataset$Liczba_Wolnych_Miejsc[dataset$Nazwa == parking], n = 1)
    percentageTaken <- round(1 - as.numeric(currentAvailibility / SpotsAvailable[parking]) ,3) * 100
    col <- colorCond2(percentageTaken, colorThreshold ) 
    
    valueBox(subtitle = parking,  icon = icon("car"), value = currentAvailibility, color =  col)
  })
  
  # value box #3
  output$valueBoxAntoniego <- renderValueBox({
    
    dataset <- getData()
    parking <- "ul. sw. Antoniego"
    currentAvailibility <- tail(dataset$Liczba_Wolnych_Miejsc[dataset$Nazwa == parking], n = 1)
    percentageTaken <- round(1 - as.numeric(currentAvailibility / SpotsAvailable[parking]) ,3) * 100
    col <- colorCond2(percentageTaken, colorThreshold ) 
    
    valueBox(subtitle = parking,  icon = icon("car"), value = currentAvailibility, color =  col)
  })
  
  # value box #4
  output$valueBoxHala <- renderValueBox({
    
    dataset <- getData()
    parking <- "Parking Hala Stulecia"
    currentAvailibility <- tail(dataset$Liczba_Wolnych_Miejsc[dataset$Nazwa == parking], n = 1)
    percentageTaken <- round(1 - as.numeric(currentAvailibility / SpotsAvailable[parking]) ,3) * 100
    col <- colorCond2(percentageTaken, colorThreshold ) 
    
    valueBox(subtitle = parking,  icon = icon("car"), value = currentAvailibility, color =  col)
  })
  
  # Current time + 5 min ahead (300 seconds); reset every 5 minutes
  newTime <- reactive({
    autoInvalidate()
    Sys.time() + 300
  })
  
  # Countdown timer 
  output$countDown <- renderUI({
    
    eventTime <- newTime()
    # reactive timer to update Sys.time() each second
    invalidateLater(1000, session)
    dt <- difftime(eventTime, Sys.time(), units = "secs")
    format(.POSIXct(dt, tz = "GMT"), "%H:%M:%S")
    
  })
  
  # chart using ggplot2
  output$chart_gg <- renderPlot({
    df <- getData()
    ggplot(df) + geom_line(aes(x = Czas_Rejestracji, y = Liczba_Wolnych_Miejsc, color = Nazwa)) + facet_wrap(~ Nazwa, nrow = 2)
  })
    
  # chart using highcharts
  output$chart_h <- renderHighchart({
    
    dataset <- getData()
    # add converted variable (datetime -> numeric) and offset for GMT timezone (+ 2 hours, expressed in miliseconds)
    #dataset$time <- datetime_to_timestamp(dataset$Czas_Rejestracji) + 7200 * 1000
    # add converted variable (datetime -> numeric) 
    dataset$time <- datetime_to_timestamp(dataset$Czas_Rejestracji) 
    #dataset$time <- as.numeric(dataset$Czas_Rejestracji)*1000
    # get current time 
    currentTime <- strftime(tail(dataset$Czas_Rejestracji, n = 1), format = "%H:%M:%S")
    parking <- names(SpotsAvailable)
    # Configure chart
    hc <- highchart() %>%
    hc_chart( zoomType = "x") %>%
    hc_title(text = "Wolna liczba miejsc postojowych (wczoraj i dzis)") %>%
    hc_subtitle(text = paste("Aktualizacja", currentTime ) ) %>%
    hc_yAxis(title = list(text = "Liczba wolnych miejsc postojowych")) %>%
    hc_plotOptions(spline = list(marker=list(enabled = F))) %>%
    hc_tooltip(dateTimeLabelFormats = list(day = "%d/%m/%Y", hour = "%H:%M"), headerFormat = '<span style="font-size: 10px">{point.x:%d/%m/%Y %H:%M}</span><br>') %>%
    hc_xAxis(type = "datetime",  title = list(text = "Czas"), dateTimeLabelFormats = list(day = "%d/%m/%Y", hour = "%H:%M")) %>%
    hc_add_series(name = parking[3], marker = list(enabled =F), data = list_parse2(dataset[dataset$Nazwa == parking[3],c("time","Liczba_Wolnych_Miejsc")])) %>%
    hc_add_series(name = parking[2], marker = list(enabled =F), data = list_parse2(dataset[dataset$Nazwa == parking[2],c("time","Liczba_Wolnych_Miejsc")]) ) %>%
    hc_add_series(name = parking[4],  marker = list(enabled =F), data = list_parse2(dataset[dataset$Nazwa == parking[4],c("time","Liczba_Wolnych_Miejsc")]) ) %>%
    hc_add_series(name = parking[1],  marker = list(enabled =F), data = list_parse2(dataset[dataset$Nazwa == parking[1],c("time","Liczba_Wolnych_Miejsc")]))
    hc
  })

  # raw data output
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(getData(), input$maxrows))
    options(orig)
  })
  
  # data download 
  output$downloadCsv <- downloadHandler(
    filename = "WroclawParkingOpenData.csv",
    content = function(file) {
      write.csv(getData(), file)
    },
    contentType = "text/csv"
  )
 
}

# Run shiny app
shinyApp(ui, server)

