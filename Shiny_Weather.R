########################################## Install+Load packages

if (!("histogram" %in% rownames(installed.packages()))) install.packages("histogram")
if (!("moments" %in% rownames(installed.packages()))) install.packages("moments")
if (!("corrr" %in% rownames(installed.packages()))) install.packages("corrr")
if (!("viridis" %in% rownames(installed.packages()))) install.packages("viridis")
#if (!("ggplotmap" %in% rownames(installed.packages()))) install.packages("ggplotmap")
if (!("readr" %in% rownames(installed.packages()))) install.packages("readr")
if (!("shiny" %in% rownames(installed.packages()))) install.packages("shiny")
if (!("shinydashboard" %in% rownames(installed.packages()))) install.packages("shinydashboard")
if (!("tidyr" %in% rownames(installed.packages()))) install.packages("tidyr")
if (!("plyr" %in% rownames(installed.packages())))install.packages("plyr")
if (!("dplyr" %in% rownames(installed.packages())))install.packages("dplyr")
if (!("rgdal" %in% rownames(installed.packages())))install.packages("rgdal")
if (!("tmap" %in% rownames(installed.packages())))install.packages("tmap")
if (!("ggmap" %in% rownames(installed.packages())))install.packages("ggmap")
if (!("sf" %in% rownames(installed.packages())))install.packages("sf")
if (!("ggspatial" %in% rownames(installed.packages())))install.packages("ggspatial")
if (!("rlang" %in% rownames(installed.packages())))install.packages("rlang")
if (!("broom" %in% rownames(installed.packages())))install.packages("broom")
if (!("tidyverse" %in% rownames(installed.packages())))install.packages("tidyverse")
if (!("readxl" %in% rownames(installed.packages())))install.packages("readxl")
if (!("purrr" %in% rownames(installed.packages())))install.packages("purrr")
if (!("Census2016" %in% rownames(installed.packages())))install.packages("Census2016")
if (!("sf" %in% rownames(installed.packages())))install.packages("sf")
if (!("plotly" %in% rownames(installed.packages())))install.packages("plotly")
if (!("tidyverse" %in% rownames(installed.packages())))install.packages("tidyverse")
if (!("corrplot" %in% rownames(installed.packages())))install.packages("corrplot")
if (!("magrittr" %in% rownames(installed.packages())))install.packages("magrittr")


library(plyr)
library(dplyr)
#library(ggplotmap)
library(rgdal)
library(tmap)
library(ggmap)
library(sf)
library(ggspatial)
library(rlang)
library(broom)
library(tidyverse)
library(readxl)
library(purrr)
library(Census2016)
library(sf)
library(histogram)
library(moments)
library(corrr)
library(viridis)
library(readr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(corrplot)
library(magrittr)


###################### Load data

Weather<-read.csv("B:/Datasety/australia_weather.csv") #Set your path
AUS_SA2_shp <- read_sf("B:/Datasety","SA2_2016_AUST")#Set your path
ShapeJoined<-read.csv("B:/Datasety/2151_80.csv",dec = ",",sep = ";")#Set your path

Weather$Evaporation<-NULL
Weather$Sunshine<-NULL
Weather$RainTomorrow<-NULL
Weather$Cloud9am<-NULL
Weather$Cloud3pm<-NULL



#Presentation table

Temporary<-Weather[,c(2,5,13,16,17)]
MTable<-aggregate(Temporary$Humidity3pm, list(Temporary$Location), FUN=mean, na.rm=TRUE)
names(MTable)[names(MTable) == "Group.1"] <- "Location"
names(MTable)[names(MTable) == "x"] <- "Average Humidity 3pm"

Tem2<-aggregate(Temporary$Temp9am, list(Temporary$Location), FUN=mean, na.rm=TRUE)
names(Tem2)[names(Tem2) == "Group.1"] <- "Location"
names(Tem2)[names(Tem2) == "x"] <- "Average Temperature 9am"
MTable<-merge(MTable,Tem2,by="Location")

Tem2<-aggregate(Temporary$Temp3pm, list(Temporary$Location), FUN=mean, na.rm=TRUE)
names(Tem2)[names(Tem2) == "Group.1"] <- "Location"
names(Tem2)[names(Tem2) == "x"] <- "Average Temperature 3pm"
MTable<-merge(MTable,Tem2,by="Location")


Tem2<-aggregate(Temporary$Rainfall, list(Temporary$Location), FUN=mean, na.rm=TRUE)
names(Tem2)[names(Tem2) == "Group.1"] <- "Location"
names(Tem2)[names(Tem2) == "x"] <- "Average Rainfall"
MTable<-merge(MTable,Tem2,by="Location")


# Histogram frames 
WeaCreek<-Weather[Weather$Location == 'BadgerysCreek',]
WeaPerth<-Weather[Weather$Location == 'Perth',]
WeaDarwin<-Weather[Weather$Location == 'Darwin',]
WeaAliceSprings<-Weather[Weather$Location == 'AliceSprings',]
heatrev<-rev(heat.colors(20))


################# Map frame

Joined <- inner_join(AUS_SA2_shp,ShapeJoined,by = c("SA2_NAME16" = "SA2_NAME16"))
names(Joined)[names(Joined) == 'x'] <- "Humidity"

# Radar table

Temporary<-Weather[,c(2,5,7,13,16,17)]
RadarTable<-MTable[,c(1,2,4)]

Tem2<-aggregate(Temporary$Rainfall, list(Temporary$Location), FUN=mean, na.rm=TRUE)
names(Tem2)[names(Tem2) == "Group.1"] <- "Location"
names(Tem2)[names(Tem2) == "x"] <- "Average Rainfall"
RadarTable<-merge(RadarTable,Tem2,by="Location")

Tem2<-aggregate(Temporary$WindGustSpeed, list(Temporary$Location), FUN=mean, na.rm=TRUE)
names(Tem2)[names(Tem2) == "Group.1"] <- "Location"
names(Tem2)[names(Tem2) == "x"] <- "Average Wind Gust Speed"
RadarTable<-merge(RadarTable,Tem2,by="Location")

Tem2<-aggregate(Temporary$Temp9am, list(Temporary$Location), FUN=mean, na.rm=TRUE)
names(Tem2)[names(Tem2) == "Group.1"] <- "Location"
names(Tem2)[names(Tem2) == "x"] <- "Average Temperature 9am"
RadarTable<-merge(RadarTable,Tem2,by="Location")


RadarTable<-na.omit(RadarTable)    

maxHum<-max(RadarTable$`Average Humidity 3pm`)
maxTem3<-max(RadarTable$`Average Temperature 3pm`)
maxRai<-max(RadarTable$`Average Rainfall`)
maxWin<-max(RadarTable$`Average Wind Gust Speed`)
maxTem9<-max(RadarTable$`Average Temperature 9am`)

RadarTable$`Average Humidity 3pm`<-(RadarTable$`Average Humidity 3pm`/maxHum)*100
RadarTable$`Average Temperature 3pm`<-(RadarTable$`Average Temperature 3pm`/maxTem3)*100
RadarTable$`Average Rainfall`<-(RadarTable$`Average Rainfall`/maxRai)*100
RadarTable$`Average Wind Gust Speed`<-(RadarTable$`Average Wind Gust Speed`/maxWin)*100
RadarTable$`Average Temperature 9am`<-(RadarTable$`Average Temperature 9am`/maxTem9)*100

MTable$`Average Humidity 3pm`<-round(MTable$`Average Humidity 3pm`,3)
MTable$`Average Temperature 3pm`<-round(MTable$`Average Temperature 3pm`,3)

MTable$`Average Temperature 9am`<-round(MTable$`Average Temperature 9am`,3)
MTable$`Average Rainfall`<-round(MTable$`Average Rainfall`,3)


#Correlation matrix
mcorel <- cor(na.omit(Weather[,c(3,4,5,12,13,14,15,16,17)]))

# Frame for Scatter3D

DFscat<-Weather[,c(1,2,13,11,17)]
DFscat<-DFscat[DFscat$Location == 'Sydney' | DFscat$Location == 'Brisbane'| DFscat$Location == 'Melbourne',]
DFscat$Date = substr(DFscat$Date,1,7)
DFscat<- aggregate(cbind(Humidity3pm,WindSpeed3pm,Temp3pm) ~ Location + Date, data = DFscat, FUN = median, na.rm = TRUE)
DFscat<-DFscat[162:230,]


# Time series data

tabtime<-Weather[,c(1:4)]

tabtime<-na.omit(tabtime)
tabtime$Date<-as.Date(tabtime$Date)


series <- Weather[,c(1,2,17)]


series<-na.omit(series)
series$Date<-as.Date(series$Date)
series<-series[series$Date > "2009-01-01" & series$Date < "2016-01-01",]


remove(maxHum,maxPre,maxRai,maxTem,maxTem3,maxTem9,maxWin,Temporary,Tem2)




################# App


ui <- dashboardPage(
  
  dashboardHeader(title="Australian weather"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table and histograms",
               tabName ="some_tab"),
      menuItem("Map and radar",
               tabName="map_tab"),
      menuItem("Scatter plot 3D",
               tabName="scat_tab"),
      menuItem("Time series", 
               tabName = "time_tab")
      
    )),
  
  
  dashboardBody(
    tabItems(
      
      
      tabItem(tabName="some_tab",
              box(dataTableOutput('table'),  HTML(
                paste(
                  
                  h4(style="text-align: justify;","This table allows you to sort by the selected column by clicking on the column. 
                     There are two search options, the field in the upper right corner is for searching the entire table, 
                     and the fields below the table to specific columns. In the upper left corner you can change the number of displayed rows.")
                )
              )), 
              box(height=660,sliderInput(inputId = "bins",
                              label = "Number of bins:",
                              min = 1,
                              max = 20,
                              value = 10),
                  plotOutput("plothis"),HTML(
                    paste(
                      
                      h4(style="text-align: justify;","Above are histograms, charts showing how often the
                         values in a given range have appeared. The data taken 
                         into account is the temperature measured at 3pm. Baderys Creek 
                         to the southeast, Darwin to the north, Alice Springs, located around the 
                         center of Australia and Perth to the southwest were taken into account.")
                    
                  ))),
            
),
      
      
      
      
      tabItem(tabName = "map_tab",fluidRow(box(height=550,title = "Weather conditions in individual locations.",
                                               sidebarPanel(
                                                 selectInput(inputId = "indv",
                                                             label = "Location",
                                                             choices = RadarTable$Location, 
                                                             selected = 'Adelaide')
                                               ),
                                               
                                               plotlyOutput('radar'),
                                               HTML(
                                                 paste(
                                                   h4("The values range from 0-100, with 100 being the highest value among all the locations included."),
                                                   h4("Values below 100 are the percentage of the highest value.")
                                                      )
                                               )
                                               
      )
      
      , box(height=550,plotOutput("plotmap"), HTML(
        paste(
          
          h4("*"),
          h4("The data used to create this map was generated from several samples in order to demonstrate the tool.")
          
        )
      )
      )
      )
      
      
      ),
      tabItem(tabName = "scat_tab",box(height=760, plotOutput("plotcol"),HTML(
        paste(
          
          h4(style="text-align: justify;","The graph above is a correlogram. 
          The displayed numbers are the calculated Pearson correlation 
          coefficient values. It determines how closely the individual
          features are related to each other. Negative values mean that
          as one of the features increases, the value of the other decreases.
          Positive values mean that an increase in one feature means an increase
          in another, similarly a decrease. Conventional intervals that we can assume are for the absolute value of the coefficient:")
           
            ,h4("| r | = 0 - no correlation")
,h4("0.0 <| r | ??? 0.1 - very weak correlation")
              
,h4("0.1 <| r | ??? 0.3 - weak correlation")
,h4("0.3 <| r | ??? 0.5 - average correlation")
,h4("0.5 <| r | ??? 0.7 - high correlation")
,h4("0.7 <| r | <0.9 - very high correlation")
,h4("0.9 <| r | ??? 1 - almost full correlation")
,h4("| r | = 1 - full correlation")

        )
      )), box(height=760,plotlyOutput('scat'),HTML(
        paste(
          h4("."),h4("."),h4("."),h4("."),h4("."),
          h4(style="text-align: justify;","The scatterplot above was based 
             on aggregated data on the monthly medians of weather conditions at 3 p.m. 
             in Brisbane, Melbourne and Sydney. The X axis is humidity, the Y axis is
             wind speed, and the Z axis is temperature. The graph is interactive, it 
             allows you to rotate, zoom in and out, and also to check the value of specific points."))))
  ),
tabItem(tabName="time_tab",box(width = 10,height=750,fluidRow(
  mainPanel(
    selectInput(inputId = "cho_series",
                label = "Location",
                choices = RadarTable$Location, 
                selected = 'Nuriootpa')
  )
),fluidRow( column(12, align="center",plotlyOutput('series'))), HTML(
  paste(
    
    h4("The graph above shows the temperature values at 3pm in selected measuring
       locations in Australia. 3 trend lines (line-green, square-red and cubic-purple) 
       have been added to the chart. Graphs can be zoomed in by selecting the selected
       fragment and scrolled using the toolbox. The individual values of 
       both the temperature readings and the trend line values can be read 
       by hovering the mouse over.")
    ))
))
)))

server <- function(input, output, session) {
  
  
  #Histograms
  output$plothis <- renderPlot({
    
    x1<-na.omit(WeaCreek$Temp3pm)
    x2<-na.omit(WeaAliceSprings$Temp3pm)
    x3<-na.omit(WeaDarwin$Temp3pm)
    x4<-na.omit(WeaPerth$Temp3pm)
    
    
    bins <- seq(min(x1), max(x1), length.out = input$bins + 1)
    bins2 <- seq(min(x2), max(x2), length.out = input$bins + 1)
    bins3 <- seq(min(x3), max(x3), length.out = input$bins + 1)
    bins4 <- seq(min(x4), max(x4), length.out = input$bins + 1)
    
    
    par(mfrow=c(2,2))
    
    hist( x1, cex.main=1.45, cex.lab=1.4,cex.sub=1.45,cex.axis=1.45,breaks = bins, col=heatrev, border = "black",
          xlab = "Temperature",  ylab="Frequency",
          main = "Badgerys Creek")
    
    hist( x2, cex.main=1.45, cex.lab=1.4,cex.sub=1.45,cex.axis=1.45, breaks = bins2, col=heatrev, border = "black",
          xlab = "Temperature",   ylab="Frequency",
          main = "Alice Springs")
    hist( x3, cex.main=1.45, cex.lab=1.4,cex.sub=1.45,cex.axis=1.45, breaks = bins3, col=heatrev, border = "black",
          xlab = "Temperature", ylab="Frequency",
          main = "Darwin")
    hist( x4, cex.main=1.45, cex.lab=1.4,cex.sub=1.45,cex.axis=1.45, breaks = bins4, col=heatrev, border = "black",
          xlab = "Temperature", ylab="Frequency",
          main = "Perth")
    
  })
  
  
  
  #TABLE
  output$table <- renderDataTable(MTable, options=list(  pageLength = 10,lengthMenu=c(5,10,15)))
  

  #MAP
  output$plotmap<-renderPlot({
    
    
    
    ggplot() +
      geom_sf(data = Joined,
              aes(fill = Humidity)) +
      ggtitle("Average humidity at 3 p.m.") +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_bw() +
      theme(
        legend.position = "right",
        legend.title = element_text("Average humidity at 3 p.m."))
    
  })
  
  #Correlations
  output$plotcol<-renderPlot({ 
    mcorel <- cor(na.omit(Weather[,c(3,4,5,12,13,14,15,16,17)]))
    
    
    col <- colorRampPalette(c("#030385", "#2e8dd1", "#FFFFFF", "#993639", "#ed262c"))
    corrplot(mcorel, method="color", col=col(200),  
             
             addCoef.col = "black", # Add coefficient of correlation
             tl.col="black", tl.srt=30, 
            
             
            
             diag=F 
    )
    
  })
  
  
  #Scatter plot
  output$scat <- renderPlotly({ 
    DFscat$Location <- as.factor(DFscat$Location)
    
    fig2 <- plot_ly(DFscat, x = ~Humidity3pm, y = ~WindSpeed3pm, z = ~Temp3pm, color = ~Location, colors = c('Red', 'Blue', "Green"))
    fig2 <- fig2 %>% add_markers()
    fig2 <- fig2 %>% layout(title = '3D Scatter plot containing monthly medians over 2 years',autosize = F, width = 650, height = 550,scene = list(xaxis = list(title = 'Humidity'),
                                                                                                           yaxis = list(title = 'Wind'),
                                                                                                           zaxis = list(title = 'Temperature')))
 
    fig2
    
  })
  
  #Time series
  output$series<-renderPlotly({
    series1 <- filter(series, Location == input$cho_series)
    
    figs <- plot_ly(series1, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~Date, y = ~Temp3pm)%>%
      layout(showlegend = F)
    figs <- figs %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6')
    line.fmt = list(dash="solid", width = 1.5, color=NULL)
    ti = 1:length(series1$Date)
    m1 = lm(series1$Temp3pm~ti) #Linear
    m2 = lm(series1$Temp3pm~ti+I(ti^2)) #Quadratic
    m3 = lm(series1$Temp3pm~ti+I(ti^2)+I(ti^3)) #Cubic
    
    figs = add_lines(figs,x=~Date,y=predict(m1), line=line.fmt, name="Linear")
    figs = add_lines(figs,x=~Date,y=predict(m2), line=line.fmt, name="Quadratic")
    figs = add_lines(figs,x=~Date,y=predict(m3), line=line.fmt, name="Cubic")
    figs
    
    
  })
  
  
  #Radar plot
  output$radar <- renderPlotly({
    pkmn <- filter(RadarTable, Location == input$indv)
    
    r <- map_dbl(pkmn[, 2:6], ~.x)
    nms <- names(r)
    
    
    fig <- plot_ly(
      type = 'scatterpolar',
      r = r,
      theta = nms,
      fill = 'toself',
      mode = 'markers'
    ) 
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,max(r))
          )
        ),
        showlegend = F
      )
  })
  
}


shinyApp(ui = ui, server = server)
