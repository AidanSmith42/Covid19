library(data.table)
library(tidyverse)
library(leaflet)
library(httr) 
library(jsonlite)
library(lubridate)
library(ggthemes)
library(tigris)
library(forecast)
library(RcppArmadillo)
#options(shiny.maxRequestSize=30*1024^2)
server <- function(input, output, session){
  output$paper <- renderUI(HTML('<center><object width="1200" height="850" data="5900.pdf"></center></object>'))
  withProgress(message = "Scraping Data...", value=0,{
    df <- fread("https://query.data.world/s/wujfeekpaiczy22e576aw63ehb7l3q", header=TRUE, stringsAsFactors=FALSE)
  })
  df <- df %>% select(Case_Type, Cases, Date, Country_Region, Province_State, Lat, Long) %>% filter(Country_Region %in% c('US', 'Italy', 'United Kingdom'))
  
  
  
  
  Cases <- df %>% filter(Country_Region == "US" & Case_Type=='Deaths') %>% select(Date, Cases)
  Cases$Date <- as.Date(Cases$Date, format = "%m/%d/%Y")
  Cases <- Cases %>% arrange(Date) %>% group_by(Date) %>% summarise(Deaths=sum(Cases)) %>% ungroup()
  
  Cases <- Cases %>% filter(Deaths > 0)  %>% mutate(DeathsPerDay = ifelse(row_number() == 1,
                                                                          0,
                                                                          Deaths - lag(Deaths, 1)))
  
  
  output$death <- renderValueBox({
    
    valueBox((tail(Cases$DeathsPerDay,1)), "Today's Deaths",icon=icon('skull'),color="red")
  })
  
  #smooth this outlier
  Cases[38, 3] <- (Cases[37, 3] + Cases[39,3])/2
  
  
  
  
  Cases2 <- df %>% filter(Country_Region == "US" & Case_Type=='Confirmed') %>% select(Date, Cases)
  Cases2$Date <- as.Date(Cases2$Date, format = "%m/%d/%Y")
  Cases2 <- Cases2 %>% arrange(Date) %>% group_by(Date) %>% summarise(Deaths=sum(Cases)) %>% ungroup()
  
  Cases2 <- Cases2 %>% filter(Deaths > 0)  %>% mutate(DeathsPerDay = ifelse(row_number() == 1,
                                                                            0,
                                                                            Deaths - lag(Deaths, 1)))
  
  
  names(Cases2) <- c("Date", "Cases", "CasesPerDay")
  Cases <- left_join(Cases2, Cases)
  
  observeEvent(c(input$fatal, input$deathTime),{
    
    #   StateData <- df %>% filter(Case_Type=='Deaths' & Country_Region== input$State) %>% select(Date, Cases, Province_State)
    #   
    #  decline <- auto.arima(tail(Cases$DeathsPerDay, 7), d=1)
    #  decadd <- forecast(decline, 2)
    #  x <- as.data.frame(decadd$lower)
    #  
    #  C3 <- c(Cases$DeathsPerDay %>% na.omit(), x$`80%`)
    # model <- auto.arima(tail(C3,30))
    # fcasts <- forecast(model , 60)
    # x <- as.data.frame(fcasts$lower)
    # 
    # 
    # newDF <- data.frame("Date"= as.Date((Cases$Date[nrow(Cases)]+1):(Cases$Date[nrow(Cases)]+60), origin = "1970-01-01"), "Cases"=NA,  "CasesPerDay"=NA , "Deaths" = NA, "DeathsPerDay"=x$`80%`)
    # Cases <- rbind(Cases,newDF)
    
    Italy <- df %>% filter(Case_Type=='Deaths' & Country_Region== 'Italy') %>% select(Date, Cases, Province_State)
    Italy$Date <- as.Date(Italy$Date, format = "%m/%d/%Y")
    Italy <- Italy %>% arrange(Date) %>% group_by(Date) %>% summarise(Deaths=sum(Cases)) %>% ungroup() 
    Italy <- Italy %>% filter(Deaths > 0)  %>% mutate(DeathsPerDay = ifelse(row_number() == 1,
                                                                            0,
                                                                            Deaths - lag(Deaths, 1))) %>% 
      mutate(DeathsPerDay = ifelse(DeathsPerDay < 0, 
                                   (lead(DeathsPerDay,1) +lag(DeathsPerDay,1))/2,
                                   DeathsPerDay))
    
    Italy <- Italy %>% mutate(Index= 1:n(), DailyDeathsPerCapita=DeathsPerDay/60.36)
    Cdf <- data.frame(DPC = c(Italy$DailyDeathsPerCapita, rep(NA, 30)))
    Cdf$Index <- 1:nrow(Cdf)
    model <- lm(DPC ~ poly(Index, 2) , data=Cdf)
    
    Cases <- Cases %>% mutate(CalculatedNewCases = lead(DeathsPerDay, input$deathTime)*(input$fatal*100))
    Cases$TotalImpliedCases <- NA
    Cases[!is.na(Cases$CalculatedNewCases), ] <- Cases[!is.na(Cases$CalculatedNewCases), ] %>% mutate(TotalImpliedCases=cumsum(CalculatedNewCases))
    
    Cases <- Cases %>% mutate(CalculatedDailyDeaths=lag(CalculatedNewCases,input$deathTime) * (input$fatal /100))
    names(Cases)[2:3] <- c('CasesActual', "DailyCasesActual")
    
    Cases <- Cases %>% mutate(CalculatedDailyDeaths= ifelse(CalculatedDailyDeaths<0, 0, CalculatedDailyDeaths),
                              CalculatedNewCases= ifelse(CalculatedNewCases<0, 0, CalculatedNewCases),
                              DeathsPerDay = ifelse(DeathsPerDay<0, 0, DeathsPerDay))
    
    
    observeEvent(input$State,{
      if(!(input$State %in% state.name)) StateData <- df %>% filter(Case_Type=='Deaths' & Country_Region== input$State) %>% select(Date, Cases, Province_State)
      else StateData <- df %>% filter(Case_Type=='Deaths' & Province_State == input$State) %>% select(Date, Cases, Province_State)
      StateData$Date <- as.Date(StateData$Date, format = "%m/%d/%Y")
      
      if(input$State == "US") { 
        StateData <- StateData %>% arrange(Date) %>% group_by(Date, Province_State) %>% summarise(Deaths=sum(Cases)) %>% ungroup() 
        StateData= StateData %>% group_by(Date) %>% summarise(Deaths= sum(Deaths)) %>% ungroup()}
      else {StateData <- StateData %>% arrange(Date) %>% group_by(Date) %>% summarise(Deaths=sum(Cases)) %>% ungroup() }
      
      StateData <- StateData %>% filter(Deaths > 0)  %>% mutate(DeathsPerDay = ifelse(row_number() == 1,
                                                                                      0,
                                                                                      Deaths - lag(Deaths, 1))) %>% 
        mutate(DeathsPerDay = ifelse(DeathsPerDay < 0, 
                                     (lead(DeathsPerDay,1) +lag(DeathsPerDay,1))/2,
                                     DeathsPerDay))
      if(input$State != "US"){
        g <- ggplotly(ggplot(StateData, aes(Date, DeathsPerDay)) + geom_line(color= "indianred3", size=1.5) + 
                        ggtitle(paste0("Deaths Per Day for ", input$State)) + theme_igray() + theme(plot.title = element_text(hjust = 0.5)))
        
        output$StateChart <- renderPlotly(g)}
      
      else {
        
        
        Cases <- Cases %>% filter(!is.na(Deaths))
        Cases <- Cases %>% mutate(Index= 1:n(), DailyDeathsPerCapita=DeathsPerDay/328.2)
        test <- data.frame(Index = 1:(nrow(Cases) + 60))
        forecast <- predict(model, test)
        forecast <- forecast * input$fatal
        DF2 <- data.frame("Date" = as.Date((Cases$Date[nrow(Cases)] + 1):(Cases$Date[nrow(Cases)] +60), origin = "1970-01-01"), "CasesActual"=NA, "DailyCasesActual"=NA, "Deaths"= NA, "DeathsPerDay"=NA,
                          "CalculatedNewCases"= NA, "TotalImpliedCases" =NA, "CalculatedDailyDeaths" =NA, Index = "NA", "DailyDeathsPerCapita"= forecast[(nrow(Cases)+1): length(forecast)])
        Cases <- rbind(Cases, DF2)
        Cases <- Cases %>% mutate(DeathsPerDay = DailyDeathsPerCapita * 328.2)
        Cases <- Cases %>% mutate(DeathsPerDay = ifelse(DeathsPerDay<0, 0, DeathsPerDay))
        g <- ggplotly(ggplot() + geom_line(data= Cases %>% filter(Date < Sys.Date()), aes(Date, DeathsPerDay), color="indianred3", size=1.5) + geom_line(data= Cases %>% filter(Date >= Sys.Date()), aes(Date, DeathsPerDay), color="indianred3", size=1.5, linetype=2) +
                        ggtitle(paste0("Deaths Per Day for ", input$State)) + theme_igray() + theme(plot.title = element_text(hjust = 0.5)))
        
        output$StateChart <- renderPlotly(g)}
    })
    
    output$Table <- renderDT(datatable(Cases %>% select(Date, CasesActual, DailyCasesActual, CalculatedNewCases, TotalImpliedCases, Deaths, DeathsPerDay, CalculatedDailyDeaths), extensions='ColReorder', options=list(colReorder=T, pageLength= 25), rownames = F))
    
  })
  output$cases <- renderValueBox({
    valueBox(tail(Cases2$CasesPerDay,1), "New Cases", icon=icon('diagnoses'), color="yellow")
  })
  
  
  
  #Fatality Chart
  d <- df %>% filter(Case_Type=='Deaths' & Cases !=0 & Country_Region=='US') %>% group_by(Date) %>% summarise(Deaths=sum(Cases)) %>% ungroup()
  d$Date <- as.Date(d$Date, format = "%m/%d/%Y")
  
  g <- ggplotly(ggplot(d, aes(x=Date,y=Deaths, group=1)) + geom_line(color="indianred3", size=1.5)+ ylab("Total Fatalities") + theme_igray() + ggtitle("Total Fatalities for the United States") +theme(plot.title = element_text(hjust = 0.5)))
  output$deaths <- renderPlotly(g)
  
  
  #Map Chart
  mapdata <- df %>% filter(Country_Region == 'US' & Cases !=0 & Case_Type == 'Deaths' & Lat != 0.0000 & (Province_State != "Guam") & Province_State !="Hawaii") %>% group_by(Lat, Long) %>% summarize(Deaths = sum(Cases)) %>% ungroup()
  
  output$map <- renderLeaflet({ 
    leaflet(mapdata)  %>% setView(-99, 38, zoom = 4)  %>% 
      addTiles() %>% addCircles(data = mapdata, lat = ~ Lat, lng = ~ Long, weight = 15, color = "red", radius = ~Deaths*5, popup = ~Deaths, stroke = FALSE, fillOpacity = 0.5) %>% 
      addProviderTiles(providers$CartoDB.DarkMatter)
    
  })
  #Light Map
  output$lightmap <- renderLeaflet({
    leaflet(mapdata)  %>% setView(-99, 38, zoom = 4)  %>% 
      addTiles() %>% addCircles(data = mapdata, lat = ~ Lat, lng = ~ Long, weight = 15, color = "red", radius = ~Deaths*5, popup = ~Deaths, stroke = FALSE, fillOpacity = 0.5) %>% 
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012)
  })
  
  
  
  #read in pouplation data. add abbreviations 
  pop <- read.csv("popdata.csv")
  colnames(pop) <- c("StateName", "population")
  pop$state <- c(state.abb, "DC", "PR")
  
  
  #Get current COVID data from the covid tracking project
  url <- "https://covidtracking.com/api/states"
  DF <- fromJSON(url) %>% as.data.frame
  
  #Clean up the date time
  DF$date <- ymd(substr(DF$dateModified, 1, 10))
  
  recov <- DF$recovered %>% na.omit() %>% sum()
  
  output$recovered <- renderValueBox({
    valueBox(recov, "Recovered", icon=icon('medkit'),color="green")
  })
  
  
  #Select the variables we care about and merge with the population data
  DF <- DF %>% select(state, positive, hospitalized, death, totalTestResults, date)
  DF <- left_join(DF, pop, by = "state") 
  
  #Calculate total positive cases and death per millino
  DF$total.positives.per.million <- (DF$positive / DF$population) * 1000000
  DF$total.deaths.per.million <- (DF$death / DF$population) * 1000000
  
  DF <- DF  %>% select(StateName, population, total.positives.per.million, total.deaths.per.million)
  names(DF) <- c("state", "Population", "CasesPerMillion", "DeathsPerMillion")
  
  observeEvent(input$Metric,{
    
    states <- states(cb=T)
    DF <- geo_join(states, DF, 'NAME','state')
    
    if(input$Metric == "Deaths Per Million") output$Metrics <- renderUI(HTML("<h3><center>Deaths Per Million Citizens</center></h3>"))
    else output$Metrics <- renderUI(HTML("<h3><center>Cases Per Million Citizens</center></h3>"))
    
    if(input$Metric == "Deaths Per Million") pal <- colorNumeric("Reds", domain=DF$DeathsPerMillion)
    else pal <- colorNumeric("Greens", domain=DF$CasesPerMillion)
    if(input$Metric == "Deaths Per Million") DF <- subset(DF, !is.na(DeathsPerMillion))
    else DF <- subset(DF, !is.na(CasesPerMillion))
    if(input$Metric == "Deaths Per Million") popup_sb <- paste0("Deaths Per Million Citizens: ", as.character(round(DF$DeathsPerMillion,2)))
    else popup_sb <- paste0("Cases Per Million Citizens: ", as.character(round(DF$CasesPerMillion,2)))
    if(input$Metric== "Deaths Per Million") {output$chloro <- renderLeaflet({ 
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = DF , 
                    fillColor = ~pal(DF$DeathsPerMillion), 
                    fillOpacity = 0.9, 
                    weight = 0.2, 
                    smoothFactor = 0.1,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label=popup_sb,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal, 
                  values = DF$DeathsPerMillion, 
                  position = "bottomright", 
                  title = "Deaths<br />per 1,000,000<br/>residents")
      
    }) }
    
    else output$chloro2 <- renderLeaflet({ 
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = DF , 
                    fillColor = ~pal(DF$CasesPerMillion), 
                    fillOpacity = 0.9, 
                    weight = 0.2, 
                    smoothFactor = 0.1,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label=popup_sb,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal, 
                  values = DF$CasesPerMillion, 
                  position = "bottomright", 
                  title = "Cases<br />per 1,000,000<br/>residents")
      
    }) 
  })
  
  
}