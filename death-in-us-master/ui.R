library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)

#install.packages("devtools")
library(devtools)

if("fiftystater" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("wmurphyrd/fiftystater")
}
library(fiftystater)
# Read in CSV Data
data <- read.csv("./data/NCHS_Leading_Causes_of_Death_United_States.csv", stringsAsFactors = FALSE)

# Sorting data by years in descending order (for the select input in the first bar chart)
attach(data) 
sort_years <- data[order(-Year),] 
detach(data) 

# Define UI for application that draws a histogram
my_ui <- shinyUI(navbarPage(
  
  #pagetheme
  theme=shinytheme("cosmo"),
  
  # Application title
  p(strong("Leading Causes of Death in the United States")),
  
  tabPanel(
    p(strong("Summary of our Project")),
    sidebarLayout(
      sidebarPanel(
        h3(strong("About Us")),
        "We are current students at the University of Washington enrolled in INFO201. 
        This webpage consists of our final project for the course.",
        tags$style(".well{background-color:lightblue;}"),
        hr(),
        h3(strong("Contact Us")),
        p("Kin Vong: ", a(href="mailto:klaivong@gmail.com", "klaivong@gmail.com")),
        p("Connor Voelk:", a(href="mailto:connorvoelk@gmail.com", "connorvoelk@gmail.com")),
        p("Yan Zhe Ong:", a(href="mailto:ongyanzhe@gmail.com", "ongyanzhe@gmail.com")),
        p("Andrew Kats:", a(href="mailto:akats98@gmail.com", "akats98@gmail.com")) 
      ),
      
      #Summary of Analysis
      mainPanel(
        img(src = "uw.jpg", width = 925, height = 275),
        br(),
        br(),
        h3(strong("Our Data")),
        p("We are working with data from the National Center for Health Statistics (NCHS)."),
        p("This data set contains data on the leading causes of death along with the corresponding total number of deaths as well as the 
            age adjusted death rate in each state from 1999 to 2016."),
        p("Our target audience are individuals who are interested in learning about the leading 
            causes of deaths in the United States, whether it be by year, states, or as a nation.
            We are hoping these individuals have a goal of using this data in order to figure out 
            how they can better maintain their physical well-being"),
        hr(),
        h3(strong("Goal Questions:")),
        p("1) What is the age adjusted death rate in each state for a specific cause of death in a given year?"),
        p("2) How do causes of death (on national level) change over time (if any)?"),
        p("3) What are the regional trends and outliers for different causes of death in each state?")
        )
        )
  ), 
  
  
  #Panel to show histogram that compares how many state have had a specific cause as their leading cause of death per year
  tabPanel(
    #title of panel
    p(strong("Causes Per State")),
    strong("This bar chart compares age adjusted death rates for 
           every state for specific causes of death in a given year"),
    hr(),
    sidebarLayout(
      
      # Creates widget to allow the user to sort by causes of death
      sidebarPanel(
        selectInput(
          inputId = "cause",
          label = "Cause of Death",
          selected = "All causes",
          choices = sort_years$Cause.Name
        ),
        
        # Creates widget to allow user to sort by year
        selectInput(
          inputId = "year",
          label = "Year",
          selected = "1999",
          choices = sort_years$Year
        )
      ),
      mainPanel(
        plotlyOutput("chart"),
        hr(),
        h3(strong("Analysis")),
        h3("What is the age adjusted death rate for a cause of death for each state in a given year?"),
        br("This bar chart allows analysis of the age adjusted death rate (per 100,000) for a cause of death 
           for every state in a given year. This bar chart allows you to compare adjusted death rates for every 
           state for a cause of death in a specific year and allows you to see the change of age adjusted death 
           rates over time. For example, the age adjusted death rate in Alabama for cancer in 1999 was 211 deaths 
           per 100,000 people. But in 2016, it was down to just 174 deaths per 100,000 people. This can help provide 
           insight into things like the overall impact of healthcare improvement and how successful improvements of healthcare 
           have been in each individual state. "),
        hr(),
        h3("Where are these numbers coming from?"),
        br("This chart analyzes age adjusted death rates for the top 10 leading causes of death in every state from 1999 
           to 2016. The age adjusted death rate is the death rate for individual causes of death when age as a confounder 
           is considered. The reason why the age adjusted death rate is so much smaller than the total deaths for these 
           causes in a lot of cases (as you can see in the graph under the National Deaths by Induvial Causes Overtime tab), 
           is because age is an important factor for a lot of these causes of death. ")
      )
    )
    ),
  
  #Panel to show line graph that shows the national deaths by individuals leading causes overtime
  tabPanel(
    #title of panel
    p(strong("National Deaths by Individual Causes Overtime")),
    strong("This line graph shows the national deaths by individual leading causes of death overtime."),
    hr(),
    sidebarLayout(
      sidebarPanel(
        selectInput("chosenCauses",
                    label = "Causes to view:",
                    choices = unique(data$Cause.Name)[unique(data$Cause.Name) != "All causes"],
                    selected = unique(data$Cause.Name)[unique(data$Cause.Name) != "All causes"],
                    multiple = TRUE),
        
        sliderInput("chosenYears",
                    "Year",
                    min = min(data$Year),
                    max = max(data$Year),
                    value = c(min(data$Year), max(data$Year)),
                    sep = "",
                    step = 1)  
      ),
      mainPanel(
        plotOutput("lineGraph"),
        hr(),
        h3(strong("Analysis")),
        h3("How do causes of death (on national level) change over time (if any)?"),
        br("Cancer and Heart Diseases are the obvious top two causes of death and both indicate worrying recent trends.
           Deaths by cancer has been constantly on the rise for the past decade and a half and 
           this makes the search for a cure even more pressing. Deaths by heart diseases showed a dip throughout the 2000s,
           but have been back on the rise in this decade."),
        br("All other causes have generally been on the rise throughout the time period,
           which does not bode well for American Health.
           It is interesting to note that Deaths by Stroke also took a dip throughout the whole 2000s,
           but is back on the rise again this decade, similar to the number of deaths due to heart disease.
           This confirms what we already know that deaths by heart diseases and stroke is often correlated."),
        hr(),
        h3("Where are these numbers coming from?"),
        br("Depending on the date range chosen by the user, the data will change.
           The user can has delete certain causes that are distorting the graph, for example, cancer and heart diseases,
           and focus on causes that they are more interested in.")
      )
    )
  ),
  
  #Panel to show map that shows total amount of deaths caused by specific causes of death in each state
  tabPanel(
    #title of panel
    p(strong("Total Deaths by Causes Per State")),
    strong("This map shows the total amount of deaths caused by specific causes of death in each state."),
    hr(),
    sidebarLayout(
      sidebarPanel(
        radioButtons("radio", label = h3("Cause of Death"),
                     choices = list("Suicide" = 3, "Heart Disease" = 2,
                                    "Cancer" = 1, "Kidney Disease" = 4, 
                                    "Stroke" = 5, "CLRD (Respitory)" = 6, "Unintentional Injuries"
                                    = 7,
                                    "Alzheimer's Disease" = 8, 
                                    "Influenza and Pneumonia" = 9, 
                                    "Diabetes" = 10), 
                     selected = 3),
        
        fluidRow(column(3, verbatimTextOutput("value")))
        
      ),
      mainPanel(
        plotOutput("fifty_map"),
        hr(),
        h3(strong("Analysis")),
        h3("What are the regional trends and outliers for different casuses of death in each state?"),
        br("This map allows insight to regional trends in cause of death. There are some interesting 
           regions that can be seen in each map. For instance, the Suicide Map shows an area just West
           of the middle of the country that has very high suicide rates, once this trend reaches the 
           West coast it stops."),
        br("Another notable trend is that states with very large or very small populations (outliers)
           tend to be the most different and stand out in the maps. For example, on the Suicide Map 
           the large states of California and New York have very low relitve percentages of suicides, 
           while the small population of Alaska is quite large."),
        hr(),
        h3("Where are these numbers coming from?"),
        br("All cases of the top ten leading causes of death and all causes of death in the United States from 1999 to 2016 
           were added up to get the largest sample available. From these totals a percentage is found 
           for every state regarding each leading cause. For example, the relative percentages for the Suicide Map is found by
           taking the (total suicide deaths of the state) / (total deaths from the state) * 100, 
           and this is done for every state. It is worth noting that some of these rates can be quite
           small (less than 1%) while others can be large (Above 30%), based on the map being shown.")
        )
        )
        ),
  
  #changes background color
  background_color <- setBackgroundColor("lightblue")
  
      ))