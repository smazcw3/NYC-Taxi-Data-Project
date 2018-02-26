#################################################################################################

########Loading Packages
library(data.table)
library(lubridate)
library(dplyr)
library(datadr)
library(maptools)
library(ggplot2)
library(RColorBrewer)
library(tigris)
library(leaflet)
library(sp)
library(ggmap)
library(broom)
library(scales)
library(httr)
library(rgdal)
library(shiny)
library(datasets)

##Reading Green Taxi April 2014 data
gt_april_14 <- as.data.frame(fread("CSE587-Term Project-Data/new_green_tripledata_2014-04.csv", sep = ","))
gt_april_14 <- gt_april_14[, -c(1)]

##Reading Green Taxi April 2015 data
gt_april_15 <- as.data.frame(fread("CSE587-Term Project-Data/new_green_tripledata_2015-04.csv", sep = ","))
gt_april_15 <- gt_april_15[, -c(1)]

##Reading Green Taxi April 2016 data
gt_april_16 <- as.data.frame(fread("CSE587-Term Project-Data/new_green_tripledata_2016-04.csv", sep = ","))
gt_april_16 <- gt_april_16[, -c(1)]


#####Start of Tab2
gt_april_14_exp <- as.data.frame(fread("CSE587-Term Project-Data/exp_green_tripledata_2014-04.csv", sep = ","))
gt_april_14_exp <- gt_april_14_exp[, -c(1)]
gt_april_14_exp <- select(gt_april_14_exp, PickUpDateTime, PassengerCount, TripDistance, TotalAmount)
Hour <- format(as.POSIXct(strptime(gt_april_14_exp$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")
Date <- format(as.POSIXct(strptime(gt_april_14_exp$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m/%d")
gt_april_14_exp$Date <- Date
gt_april_14_exp$Hour <- Hour
gt_april_14_exp$PickUpDateTime <- as.POSIXct(gt_april_14_exp$PickUpDateTime)


gt_april_15_exp <- as.data.frame(fread("CSE587-Term Project-Data/exp_green_tripledata_2015-04.csv", sep = ","))
gt_april_15_exp <- gt_april_15_exp[, -c(1)]
gt_april_15_exp <- select(gt_april_15_exp, PickUpDateTime, PassengerCount, TripDistance, TotalAmount)
Hour <- format(as.POSIXct(strptime(gt_april_15_exp$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")
Date <- format(as.POSIXct(strptime(gt_april_15_exp$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m/%d")
gt_april_15_exp$Date <- Date
gt_april_15_exp$Hour <- Hour
gt_april_15_exp$PickUpDateTime <- as.POSIXct(gt_april_15_exp$PickUpDateTime)


gt_april_16_exp <- as.data.frame(fread("CSE587-Term Project-Data/exp_green_tripledata_2016-04.csv", sep = ","))
gt_april_16_exp <- gt_april_16_exp[, -c(1)]
gt_april_16_exp <- select(gt_april_16_exp, PickUpDateTime, PassengerCount, TripDistance, TotalAmount)
Hour <- format(as.POSIXct(strptime(gt_april_16_exp$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")
Date <- format(as.POSIXct(strptime(gt_april_16_exp$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m/%d")
gt_april_16_exp$Date <- Date
gt_april_16_exp$Hour <- Hour
gt_april_16_exp$PickUpDateTime <- as.POSIXct(gt_april_16_exp$PickUpDateTime)
#####End of Tab2

#####Start of Tab3
gt_april_14_v5 <- as.data.frame(fread("CSE587-Term Project-Data/lm_green_tripledata_2014-04.csv", sep = ","))
gt_april_14_v5 <- select(gt_april_14_v5, TripDistance, TipAmount, PassengerCount)
gt_april_14_v5$pred.SC <- predict(lm(TipAmount ~ TripDistance, data = gt_april_14_v5))
#ggplot(gt_april_14_v5, aes(x=TripDistance, y=TipAmount)) + geom_point() + geom_line(aes(y = pred.SC), color= "blue", size = 1) + geom_abline(slope = 0.45, intercept = 0.11, color='red', size = 1)


gt_april_15_v5 <- as.data.frame(fread("CSE587-Term Project-Data/lm_green_tripledata_2015-04.csv", sep = ","))
gt_april_15_v5 <- select(gt_april_15_v5, TripDistance, TipAmount, PassengerCount)
gt_april_15_v5$pred.SC <- predict(lm(TipAmount ~ TripDistance, data = gt_april_15_v5))
#ggplot(gt_april_15_v5, aes(x=TripDistance, y=TipAmount)) + geom_point() + geom_line(aes(y = pred.SC), color= "blue", size = 1) + geom_abline(slope = 0.60, intercept = 0.33, color='red', size = 1)


gt_april_16_v5 <- as.data.frame(fread("CSE587-Term Project-Data/lm_green_tripledata_2016-04.csv", sep = ","))
gt_april_16_v5 <- select(gt_april_16_v5, TripDistance, TipAmount, PassengerCount)
gt_april_16_v5$pred.SC <- predict(lm(TipAmount ~ TripDistance, data = gt_april_16_v5))
#ggplot(gt_april_16_v5, aes(x=TripDistance, y=TipAmount)) + geom_point() + geom_line(aes(y = pred.SC), color= "blue", size = 1) + geom_abline(slope = 0.60, intercept = 0.30, color='red', size = 1)

#####End of Tab3


#####
nw <- list(lat = 40.917577, lon = -74.259090)
se <- list(lat = 40.477399, lon = -73.700272)

trans <- function(x) {
    # convert to POSIXct time
    x$PickUpDateTime <- fast_strptime(as.character(x$PickUpDateTime), format = "%Y-%m-%d %H:%M:%S", tz = "EST")
    x$PickUpDateTime <- fast_strptime(as.character(x$PickUpDateTime), format = "%Y-%m-%d %H:%M:%S", tz = "EST")
    
    # set coordinates outside of NYC bounding box to NA
    ind <- which(x$DropOffLongitude < nw$lon | x$DropOffLongitude > se$lon)
    x$DropOffLongitude[ind] <- NA
    ind <- which(x$PickUpLongitude < nw$lon | x$PickUpLongitude > se$lon)
    x$PickUpLongitude[ind] <- NA
    ind <- which(x$DropOffLatitude < se$lat | x$DropOffLatitude > nw$lat)
    x$DropOffLatitude[ind] <- NA
    ind <- which(x$PickUpLatitude < se$lat | x$PickUpLatitude > nw$lat)
    x$PickUpLatitude[ind] <- NA
    x
}
##Transformation on green taxi april 2014
gt_april_14 <- trans(gt_april_14)
gt_april_14 <- na.omit(select(gt_april_14, PickUpLatitude, PickUpLongitude))
gt_april_14 <- group_by(gt_april_14, PickUpLatitude, PickUpLongitude)
gt_april_14 <- summarise(gt_april_14, num_pickups = n())


##Transformation on green taxi april 2015
gt_april_15 <- trans(gt_april_15)
gt_april_15 <- na.omit(select(gt_april_15, PickUpLatitude, PickUpLongitude))
gt_april_15 <- group_by(gt_april_15, PickUpLatitude, PickUpLongitude)
gt_april_15 <- summarise(gt_april_15, num_pickups = n())

##Transformation on green taxi april 2016
gt_april_16 <- trans(gt_april_16)
gt_april_16 <- na.omit(select(gt_april_16, PickUpLatitude, PickUpLongitude))
gt_april_16 <- group_by(gt_april_16, PickUpLatitude, PickUpLongitude)
gt_april_16 <- summarise(gt_april_16, num_pickups = n())

theme_map_dark <- function(palate_color = "Greys") {
    palate <- brewer.pal(palate_color, n=9)
    color.background = "black"
    color.grid.minor = "black"
    color.grid.major = "black"
    color.axis.text = palate[1]
    color.axis.title = palate[1]
    color.title = palate[1]
    
    font.title <- "Source Sans Pro"
    font.axis <- "Open Sans Condensed Bold"
    
    theme_bw(base_size=5) +
        theme(panel.background=element_rect(fill=color.background, color=color.background)) +
        theme(plot.background=element_rect(fill=color.background, color=color.background)) +
        theme(panel.border=element_rect(color=color.background)) +
        theme(panel.grid.major=element_blank()) +
        theme(panel.grid.minor=element_blank()) +
        theme(axis.ticks=element_blank()) +
        theme(legend.background = element_rect(fill=color.background)) +
        theme(legend.text = element_text(size=3,colour=color.axis.title,family=font.axis)) +
        theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal") +
        theme(legend.key.width=unit(1, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm")) +
        theme(plot.title=element_text(colour=color.title,family=font.title, size=5)) +
        theme(axis.text.x=element_blank()) +
        theme(axis.text.y=element_blank()) +
        theme(axis.title.y=element_blank()) +
        theme(axis.title.x=element_blank()) +
        theme(plot.margin = unit(c(0.0, -0.5, -1, -0.75), "cm")) +
        theme(strip.background = element_rect(fill=color.background, color=color.background),strip.text=element_text(size=7,colour=color.axis.title,family=font.title))
    
}

##########################     Visualization      ######################

r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

nyc_neighborhoods_df <- tidy(nyc_neighborhoods)


####Setting the limits for Latitude/Longitude for NYC map 
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

### Tab1
plot1 <- ggplot(gt_april_14, aes(x=PickUpLongitude, y=PickUpLatitude)) +
    geom_point(color="white", size=0.06) + scale_x_continuous(limits=c(min_long, max_long)) + 
    scale_y_continuous(limits=c(min_lat, max_lat)) + theme_map_dark()

##Working to add other plots as well on dashboard
plot2 <- ggplot(gt_april_15, aes(x=PickUpLongitude, y=PickUpLatitude)) +
    geom_point(color="white", size=0.06) + scale_x_continuous(limits=c(min_long, max_long)) + 
    scale_y_continuous(limits=c(min_lat, max_lat)) + theme_map_dark()

plot3 <- ggplot(gt_april_16, aes(x=PickUpLongitude, y=PickUpLatitude)) +
    geom_point(color="white", size=0.06) + scale_x_continuous(limits=c(min_long, max_long)) + 
    scale_y_continuous(limits=c(min_lat, max_lat)) + theme_map_dark()



###
options(warn=-1)

ui <- fluidPage(
    titlePanel(title=h4("NYC Green taxi visualization in April over the years (2014-2016)", align="center")),
    sidebarPanel(
           conditionalPanel(condition="input.conditionedPanels==1",
                    sliderInput("num_pickups", "Number of pickUps:",min = 1, max = 34,step=1,value=c(5,15))
            ),
          conditionalPanel(condition="input.conditionedPanels==2",
                           selectInput("Date", "Select the Date below:", choices=unique(unlist(gt_april_14_exp$Date))),
                           hr(),
                           helpText("The above format is in: mm/dd"),
                           helpText("Note:: The above choice of Dates comprises of the 1st week of April")
            ),
          conditionalPanel(condition="input.conditionedPanels==3",
                            checkboxGroupInput('selection', label = 'TrendLines to show:',
                                                choices = list("Show lm models" = 1), selected = 1),
                           hr(),
                           helpText("Note:: The results displays the information for 1st day of April")
         )), 
    
    
    
    mainPanel(tabsetPanel(tabPanel("Plot",
                                   fluidRow(
                                       column(12, plotOutput("plot1")),
                                       column(12, plotOutput("plot2")),
                                       column(12, plotOutput("plot3"))), value = 1), 
                          tabPanel("Plot2",
                                   column(12, plotOutput("g1")),
                                   column(12, plotOutput("g2")), 
                                   column(12, plotOutput("g3")), value = 2),
                          
                          tabPanel("Plot3",
                                   column(12, plotOutput("np1")),
                                   column(12, plotOutput("np2")), 
                                   column(12, plotOutput("np3")), value = 3)
                          
                          ,id = "conditionedPanels"
                          )))

server <- function(input,output,session){
    
    dat_1 <- reactive({
        test1 <- gt_april_14[gt_april_14$num_pickups %in% seq(from=min(input$num_pickups),to=max(input$num_pickups),by=1),]
    })
    
    output$plot1<-renderPlot({
        ggplot() + geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group)) +
            geom_point(data = dat_1(), aes(x=PickUpLongitude, y=PickUpLatitude), colour="green") + scale_x_continuous(limits=c(min_long, max_long)) + 
            scale_y_continuous(limits=c(min_lat, max_lat)) + ggtitle("Green taxi pickups in April 2014") +
            theme(plot.title = element_text(hjust = 0.5))})


    dat_2 <- reactive({
        test2 <- gt_april_15[gt_april_15$num_pickups %in% seq(from=min(input$num_pickups),to=max(input$num_pickups),by=1),]
    })
    

    output$plot2<-renderPlot({
        ggplot() + geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group)) +
            geom_point(data = dat_2(), aes(x=PickUpLongitude, y=PickUpLatitude), colour="green") + scale_x_continuous(limits=c(min_long, max_long)) + 
            scale_y_continuous(limits=c(min_lat, max_lat)) + ggtitle("Green taxi pickups in April 2015") +
            theme(plot.title = element_text(hjust = 0.5))})

    
    dat_3 <- reactive({
        test3 <- gt_april_16[gt_april_16$num_pickups %in% seq(from=min(input$num_pickups),to=max(input$num_pickups),by=1),]
    })
    
    
    output$plot3<-renderPlot({
        ggplot() + geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group)) +
            geom_point(data = dat_3(), aes(x=PickUpLongitude, y=PickUpLatitude), colour="green") + scale_x_continuous(limits=c(min_long, max_long)) + 
            scale_y_continuous(limits=c(min_lat, max_lat)) + ggtitle("Green taxi pickups in April 2016") +
            theme(plot.title = element_text(hjust = 0.5))}) 

    
     output$g1 <- renderPlot({ggplot(gt_april_14_exp[gt_april_14_exp$Date == input$Date,], aes(x = PickUpDateTime, y = TripDistance)) + 
         geom_line(aes(color = PassengerCount)) + scale_x_datetime(labels = date_format("%H:%M")) + ggtitle("Trip distance against time Green taxi in April 2014") + 
             theme(plot.title = element_text(hjust = 0.5))})
     
     output$g2 <- renderPlot({ggplot(gt_april_15_exp[gt_april_15_exp$Date == input$Date,], aes(x = PickUpDateTime, y = TripDistance)) + 
             geom_line(aes(color = PassengerCount)) + scale_x_datetime(labels = date_format("%H:%M")) + ggtitle("Trip distance against time Green taxi in April 2015") +
             theme(plot.title = element_text(hjust = 0.5))})
     
     output$g3 <- renderPlot({ggplot(gt_april_16_exp[gt_april_16_exp$Date == input$Date,], aes(x = PickUpDateTime, y = TripDistance)) + 
             geom_line(aes(color = PassengerCount)) + scale_x_datetime(labels = date_format("%H:%M")) + ggtitle("Trip distance against time Green taxi in April 2016") + 
             theme(plot.title = element_text(hjust = 0.5))})



    barplottest <- reactive({
        if (1 %in% input$selection) return(ggplot(data = gt_april_14_v5, aes(x=TripDistance, y=TipAmount)) +
                                             geom_point() + geom_line(aes(y = pred.SC), color= "blue", size = 1) +
                                             geom_abline(slope = 0.45, intercept = 0.11, color='red', size = 1) +
            ggtitle("Tip Amount against trip distance Green taxi in April 2014") + theme(plot.title = element_text(hjust = 0.5)))
        
        return(ggplot(data = gt_april_14_v5, aes(x=TripDistance, y=TipAmount)) + geom_point() +
                       ggtitle("Tip Amount against trip distance Green taxi in April 2014") + theme(plot.title = element_text(hjust = 0.5))) 
        
    })
    
    output$np1 <- renderPlot({barplottest()})


    barplottest2 <- reactive({
    if (1 %in% input$selection) return(ggplot(data = gt_april_15_v5, aes(x=TripDistance, y=TipAmount)) +
                                            geom_point() + geom_line(aes(y = pred.SC), color= "blue", size = 1) +
                                            geom_abline(slope = 0.60, intercept = 0.33, color='red', size = 1) +
                                           ggtitle("Tip Amount against trip distance Green taxi in April 2015") + theme(plot.title = element_text(hjust = 0.5)))
    
    return(ggplot(data = gt_april_15_v5, aes(x=TripDistance, y=TipAmount)) + geom_point() +
               ggtitle("Tip Amount against trip distance Green taxi in April 2015") + theme(plot.title = element_text(hjust = 0.5)))
    })
    
    output$np2 <- renderPlot({barplottest2()})


    barplottest3 <- reactive({
    if (1 %in% input$selection) return(ggplot(data = gt_april_16_v5, aes(x=TripDistance, y=TipAmount)) +
                                            geom_point() + geom_line(aes(y = pred.SC), color= "blue", size = 1) +
                                            geom_abline(slope = 0.60, intercept = 0.30, color='red', size = 1) +
                                           ggtitle("Tip Amount against trip distance Green taxi in April 2016") + theme(plot.title = element_text(hjust = 0.5)))
    
    return(ggplot(data = gt_april_16_v5, aes(x=TripDistance, y=TipAmount)) + geom_point()+
               ggtitle("Tip Amount against trip distance Green taxi in April 2016") + theme(plot.title = element_text(hjust = 0.5)))
    })
    
    output$np3 <- renderPlot({barplottest3()})}

shinyApp(ui, server)

