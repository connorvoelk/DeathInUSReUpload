library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)

#install.packages("devtools")
library(devtools)

if("fiftystater" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("wmurphyrd/fiftystater")
} 
library(fiftystater)

data <- read.csv("./data/NCHS_Leading_Causes_of_Death_United_States.csv", stringsAsFactors = FALSE)

df <- read.csv(file = "./data/AllTimeStateDeathsWLocation.csv", sep = ",", stringsAsFactors = FALSE)
df <- rename(df, state = State) # rename the State column to state
df$state <- stringr::str_trim(df$state) #remove trailing space
row.names(df) <- df$state # change row names to capitalized state names
df$state <- tolower(df$state) # turn all contents of state to lowercase

shinyServer(function(input, output) {
  attach(data)
  sort_years <- data[order(-Year),]
  detach(data)
  
  # Creates chart for age adjusted death rates for disease in a given year
  output$chart <- renderPlotly({
    dataframe <- data.frame(
      select(sort_years, Age.adjusted.Death.Rate, Year, Cause.Name, State) %>% filter(Year == input$year) %>% filter(Cause.Name == input$cause) %>% transmute(Age.adjusted.Death.Rate, State), stringsAsFactors = FALSE)
    plot_ly(dataframe, x = dataframe$State, y = dataframe$Age.adjusted.Death.Rate, type = 'bar', text = dataframe$Age.adjusted.Death.Rate, textposition = 'auto', marker = list(color = "#305f72")) %>% 
      layout(title = paste0("Age Adjusted Death Rates per State"), xaxis = list(title = "States"), yaxis = list(title = "Age Adjusted Death Rates (Per 100,000)"))
  })
  
  # Filter data for selected year range as well as cause name
  chosen_data <- reactive({
    data %>% 
      group_by(Year,Cause.Name) %>% 
      summarise(Total = sum(Deaths)) %>% 
      filter(Cause.Name %in% input$chosenCauses,
             Year >= input$chosenYears[1],
             Year <= input$chosenYears[2])
  })
  
  # Creates line graph of cause of death on a national level over time.
  output$lineGraph <- renderPlot({
    chosen_data() %>% 
      ggplot(aes(x = Year, y = Total, color = Cause.Name)) +
      geom_line() +
      geom_point() +
      labs(title = "Cause of Death on a National Level over time",
           x = "Year",
           y = "Total Deaths per year")
  })
  
  
  output$fifty_map <- renderPlot({
    # data("fifty_states") # this line is optional due to lazy data loading
    # map_id creates the aesthetic mapping to the state name column in your data
    #takes away the useless, it may have been causing issues
    #turns all state names to lowercase
    
    
    state <- map_data("state")
    # full_join(state, df, by = c("region" = "state")) %>% View
    if(input$radio == 1){
      p <- ggplot(df, aes(map_id = state)) +
        #   map points to the fifty_states shape data
        geom_map(aes(fill=Cancer/AllCauses*100), map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map() +
        ggtitle("Relative Percentage of Cancer Deaths per State of All Time")+
        fifty_states_inset_boxes() +
        scale_fill_gradient(low ='blue', high ='red') +
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
      p$labels$fill <- "Relative Percentage of Cancer Deaths"
    } else if(input$radio == 5) {
      p <- ggplot(df, aes(map_id = state)) +
        #   map points to the fifty_states shape data
        geom_map(aes(fill=Stroke/AllCauses*100), map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        fifty_states_inset_boxes() +
        scale_fill_gradient(low ='blue', high ='red') +
        coord_map() +
        ggtitle("Relative Percentage of Stroke Deaths per State of All Time")+
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
      p$labels$fill <- "Relative Percentage of Stroke Deaths"
    } else if(input$radio == 3) {
      p <- ggplot(df, aes(map_id = state)) +
        #   map points to the fifty_states shape data
        geom_map(aes(fill=Suicide/AllCauses*100), map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        fifty_states_inset_boxes() +
        scale_fill_gradient(low ='blue', high ='red') +
        coord_map() +
        ggtitle("Relative Percentage of Suicide Deaths per State of All Time")+
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
      p$labels$fill <- "Relative Percentage of Suicide Deaths"
    } else if(input$radio == 2) {
      p <- ggplot(df, aes(map_id = state)) +
        #   map points to the fifty_states shape data
        geom_map(aes(fill=HeartDisease/AllCauses*100), map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        fifty_states_inset_boxes() +
        scale_fill_gradient(low ='blue', high ='red') +
        coord_map() +
        ggtitle("Relative Percentage of Heart Disease Deaths per State of All Time")+
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
      p$labels$fill <- "Relative Percentage of Heart Disease Deaths"
    } else if(input$radio == 4) {
      p <- ggplot(df, aes(map_id = state)) +
        #   map points to the fifty_states shape data
        geom_map(aes(fill=KidneyDisease/AllCauses*100), map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map() +
        fifty_states_inset_boxes() +
        scale_fill_gradient(low ='blue', high ='red') +
        ggtitle("Relative Percentage of Kidney Disease Deaths per State of All Time")+
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
      p$labels$fill <- "Relative Percentage of Kidney Disease Deaths"
    } else if(input$radio == 6) {
      p <- ggplot(df, aes(map_id = state)) +
        #   map points to the fifty_states shape data
        geom_map(aes(fill=CLRD/AllCauses*100), map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        fifty_states_inset_boxes() +
        scale_fill_gradient(low ='blue', high ='red') +
        coord_map() +
        ggtitle("Relative Percentage of CLRD (Respitory) Deaths per State of All Time")+
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
      p$labels$fill <- "Relative Percentage of CLRD Deaths"
    } else if(input$radio == 7 ) {
      p <- ggplot(df, aes(map_id = state)) +
        #   map points to the fifty_states shape data
        geom_map(aes(fill=Cancer/AllCauses*100), map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map() +
        fifty_states_inset_boxes() +
        scale_fill_gradient(low ='blue', high ='red') +
        ggtitle("Relative Percentage of Unintentional Injury Caused Deaths per State of All Time")+
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
      p$labels$fill <- "Relative Percentage of Injury Related Deaths"
    } else if(input$radio == 8) {
      p <- ggplot(df, aes(map_id = state)) +
        #   map points to the fifty_states shape data
        geom_map(aes(fill=AlzheimersDisease/AllCauses*100), map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map() +
        fifty_states_inset_boxes() +
        scale_fill_gradient(low ='blue', high ='red') +
        ggtitle("Relative Percentage of Alzheimer's Disease Deaths per State of All Time")+
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
      p$labels$fill <- "Relative Percentage of Alzheimer's Disease Deaths"
    } else if(input$radio == 9) {
      p <- ggplot(df, aes(map_id = state)) +
        #   map points to the fifty_states shape data
        geom_map(aes(fill=InfluenzaPneumonia/AllCauses*100), map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map() +
        fifty_states_inset_boxes() +
        scale_fill_gradient(low ='blue', high ='red') +
        ggtitle("Relative Percentage of Influenza and Pneumonia Deaths per State of All Time")+
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
      p$labels$fill <- "Relative Percentage of Influenza/Pneumonia Deaths"
    } else if(input$radio == 10) {
      p <- ggplot(df, aes(map_id = state)) +
        #   map points to the fifty_states shape data
        geom_map(aes(fill=Diabetes/AllCauses*100), map = fifty_states) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map() +
        fifty_states_inset_boxes() +
        scale_fill_gradient(low ='blue', high ='red') +
        ggtitle("Relative Percentage of Diabetes Deaths per State of All Time")+
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
      p$labels$fill <- "Relative Percentage of Diabetes Deaths"
    }
    p
  })
})
