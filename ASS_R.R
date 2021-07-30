library(lubridate)
library(dplyr)
library(ggplot2)
library(leaflet)
library(viridis)
library(shiny)
library(shinyWidgets)

data <- read.csv("PE2_R_Tree_Data.csv")

summary(data)
#data cleaning
data$Genus <- as.factor(data$Genus)

data$Located.in <- as.factor(data$Located.in)

data$Date.Planted <- as.Date(parse_date_time(data$Date.Planted,"dmy"))

data$TrueYear <- year(data$Date.Planted)

data$Year.Planted <- year(data$Date.Planted)

summary(data)

dataNew <- data %>%
  group_by(Genus) %>%
  summarise(count = n()) %>%
  top_n(n = 5, wt = count)

dataNew

merged <- inner_join(dataNew[1], data[1:8], by = 'Genus')

merged

# VIS 1
ggplot(data = merged, aes(x = Genus))+
  geom_bar(stat = 'count', fill = "pink1")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = 0.5)+
  ggtitle("The Top 5 Trees") +
  theme(plot.title = element_text(h = 0.5))

# VIS 2
ggplot(data = merged, aes(x = Genus, y = Useful.Life.Expectancy.Value)) + 
  geom_boxplot(fill = 'cyan') + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, option = "A") +
  theme(legend.position = "none",
        plot.title = element_text(h = 0.5)) + 
  ggtitle("Boxplot of Trees' expectancy life")  
  
 
# VIS 3
colorTree <- colorFactor(c('forestgreen','deeppink','orange','purple','deepskyblue'),domain = merged$Genus)

leaflet(merged) %>%
  addTiles() %>%
  addCircles(~Longitude, ~Latitude, color = ~colorTree(Genus),
             label = paste('Genus:', merged$Genus),
             radius = ~Diameter.Breast.Height/20)%>%
  addLegend("topright", pal = colorTree, values = ~Genus, opacity = 0.7)

# shiny app

ui <- fluidPage(
  headerPanel("Common Trees around Fitzroy Gardens"),
  fluidRow(
    column(5,
           h4("The description of the top 5 trees, They are Corymbia, Ficus, Platanus, Quercus and 
              Ulmus, and the top one is Ulmus 349. From the map, we could find the Ulmus are planted
               in the Treasury Gardens, and Fitaroy Gardens, most of ficus are in the Treasury Gardens 
              and Lansdowne Str. The Platanus are in the left part of the Fitaroy Gardens and St Andrew place."),
           fluidRow(plotOutput("VIS1"))),
    column(7,
           leafletOutput('map'),
           absolutePanel(top = 10, left = 70,
                         pickerInput("Genus", label = "Genus of Tree",
                                     multiple = T,
                                     choices = list("Corymbia" = "Corymbia",
                                                    "Ficus" = "Ficus",
                                                    "Platanus" = "Platanus",
                                                    "Quercus" = "Quercus",
                                                    "Ulmus" = "Ulmus"),
                                     selected = c("Corymbia" = "Corymbia",
                                                  "Ficus" = "Ficus",
                                                  "Platanus" = "Platanus",
                                                  "Quercus" = "Quercus",
                                                  "Ulmus" = "Ulmus")
                                     )))
  
  
    ),
  fluidRow(
    column(5, h4("From the Trees' expectancy life, the median of Corymbia is 60 years, and there are
           potential outliers below and above. The interquarile range of Ficus is from 22 to 60, and the 
           median is 30 years, and the 75th percentile is 60, and 25th percentile is 22.The Platanus range of Platanus is from 5 to 20, and the 
           median is 10 years, and the 75th percentile is 20, and 25th percentile is 5.The interquarile range of Quercus is from 60 to 80, and the 
           median is 60 years, and the 75th percentile is 80.The interquarile range of Ulmus is from 10 to 30, and the 
           median is 20 years, and the 75th percentile is 30, and 25th percentile is 10.")),
    column(7, plotOutput("VIS2"))
  )
)


server <- function(input, output){
  output$VIS1 = renderPlot(
    ggplot(data = merged, aes(x = Genus))+
      geom_bar(stat = 'count', fill = "pink1")+
      geom_text(stat = 'count', aes(label = ..count..), vjust = 0.5)+
      ggtitle("The Top 5 Trees") +
      theme(plot.title = element_text(h = 0.5))
  )

  output$VIS2 = renderPlot(
    ggplot(data = merged, aes(x = Genus, y = Useful.Life.Expectancy.Value)) +
      geom_boxplot(fill = 'cyan') +
      scale_fill_viridis(discrete = TRUE, alpha = 0.6, option = "A") +
      theme(legend.position = "none",
            plot.title = element_text(h = 0.5)) +
      ggtitle("Boxplot of Trees' expectancy life")
  )
  
  output$map = renderLeaflet({
    leaflet(merged[merged$Genus == input$Genus, ]) %>%
      addTiles() %>%
      addCircles(~Longitude, ~Latitude, color = ~colorTree(Genus),
                 label = paste('Genus:', merged$Genus),
                 radius = ~Diameter.Breast.Height/10)%>%
      addLegend("topright", pal = colorTree, values = ~Genus, opacity = 0.7)
  })
}

shinyApp(ui, server)








