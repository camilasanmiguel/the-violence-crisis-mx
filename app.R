# welcome! :-)

# ------------------------------------------------- #

#### Libraries ####
library(readr)
library(readxl)
library(googlesheets)
library(shiny)
library(styler)
library(ggrepel)
library(gapminder)
library(lubridate)
library(ggplot2)
library(fs)
library(sf)
library(devtools)
library(tidyr)
library(transformr)
library(dplyr)
library(janitor)
library(shinythemes)
library(mxmaps)
library(lubridate)
library(rsconnect)
library(mxmaps)
library(RCurl)
library(leaflet)
library(mapview)
library(plotly)
library(tidyverse)
# ------------------------------------------------- #

#### SETUP ####
# ------------------------------------------------- #
#### PREPPING DATA ####

dir_create("raw-data")

# ------------------------------------------------- #

download.file("https://data.diegovalle.net/elcrimen/nm-estatal-victimas.csv.gz",
              destfile = "./raw-data/nm-estatal-victimas-2.csv")

crimes <- read.csv("./raw-data/nm-estatal-victimas-2.csv") %>%
  clean_names()

# ------------------------------------------------- #

download.file("https://docs.google.com/spreadsheets/d/e/2PACX-1vQaa3rKFHyZFgQiFBSw8On71yL8yw4e82fCYGsGhz4yQiUB2GJaIAbFG6qiEGlTMQ/pub?output=xlsx",
              destfile = "./raw-data/INEGI perception of insecurity.xlsx")

perception <- read_xlsx("./raw-data/INEGI perception of insecurity.xlsx")
# this data is from 2011 to 2019 in each state and in country as a whole

# ------------------------------------------------- #

download.file("https://docs.google.com/spreadsheets/d/e/2PACX-1vSEp6QCr7FTUBHvApwjf9_XZk5Lfs-VmymRcoEkW_WYYcqzy-XPh3XaEfFf-1nrb7JsjnfI90ICWY2d/pub?output=csv",
              destfile = "./raw-data/family_child_total_monthly_2000_2018.csv")

bp_fc <- read.csv("./raw-data/family_child_total_monthly_2000_2018.csv")

bp_fc <- bp_fc %>%
  # this data is specifically for families and 
  # unaccompanied children 
  clean_names() %>%
  filter(sector == "southwest border") %>%
  filter(month == "yearly total") %>%
  filter(fiscal_year %in% c('2014', '2015', '2016', '2017', '2018'))
# ------------------------------------------------- #

#### MY UI ####
ui <- navbarPage(
  title = "The Cartel Violence Crisis 
                in Mexico: Public Perceptions of Safety 
                Alongside Border Apprehensions",
  theme = shinytheme("sandstone"),
  
  # ------------------------------------------------- #
  #### first tab (map) ####
  
  tabPanel(title = "Crime in Mexico Over Time",
           sidebarLayout(
             # problem below in sidebar panel...
             sidebarPanel(
               selectInput("crime", "Select type of crime",
                           choices = c("Homicide" = "Homicide",
                                       "Human Trafficking" = "Human Trafficking",
                                       "Sexual Trafficking of Children" = "Sexual Trafficking of Children",
                                       "Assaults" = "Assaults",
                                       "Feminicide" = "Feminicide",
                                       "Hostage Situation" = "hostage_situation",
                                       "Extortion" = "Extortion"),
                           selected = "Human Trafficking"),
               sliderInput("year", "Select year",
                           min = 2015, max = 2019, value = 2019, sep=""),
               helpText("Violence Over Time. Data From: CONAPO, Mexican government agency on
                                  crime and law enforcement. All crime data used is taken from the publicly
                                  available compilation spreadsheet updated regularly with data by each state's
                                  government.")),
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   title = "Map of Violence Over Time",
                   plotOutput("map1")))
               
             )
           )
  ),
  # ------------------------------------------------- #
  #### second tab (perceptions of insecurity) ####
  tabPanel(
    title = "Perceptions of Insecurity",
    sidebarLayout(
      sidebarPanel(
        selectInput("state", "Choose a state",
                    choices = c("Estados Unidos Mexicanos",
                                "Aguascalientes", "Baja California", "Baja California Sur", 
                                "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", 
                                "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", 
                                "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán de Ocampo", 
                                "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", 
                                "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", 
                                "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", 
                                "Yucatán", "Zacatecas"),
                    selected = "Ciudad de México"),
        h6("Over time, the drug war and its effects have brought the vast majority of Mexicans, 
        especially in the states most affected by violence, to feel unsafe. Data courtesy of INEGI.")),
      mainPanel(
        title = "Percentage of  citizens over 18 who feel unsafe in their state in Mexico",
        plotlyOutput("plot_perception")),
      
    )
    # above parentheses closes sidebar layout
  ),
  # ------------------------------------------------- #
  #### third tab (bp apprehensions) ####
  tabPanel(
    title = "Border Patrol Apprehensions on the Southwest U.S. Border by Year",
    sidebarLayout(
      sidebarPanel(
        # BELOW IS WHERE ERROR IS COMING FROM I THINK
        selectInput("apprehensions", "Choose apprehension subjects",
                    choices = c("Unaccompanied Children",
                                "Total Apprehensions",
                                "Family Units")),
        h6("Source: Border Patrol and UMich data people")),
      # useful to remember: sidebar panel is closed by double parentheses after h6
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Border Patrol Apprehensions",
            plotlyOutput("plot_bp")
          )
        )
      )
    )
  )
  # this final parentheses is the navbar closing
)

# ------------------------------------------------- #
#### fourth tab (regression) ####
# 
#                tabPanel(
#                    title = "Relationship Between Public Perceptions of Violence
#                    and Attempts at Illegal Immigration",
#                 mainPanel(
#                     tabsetPanel(
#                        tabPanel(
#                          title = "Relationship Between Public Perceptions of Violence
#                          and Attempts at Illegal Immigration",
#                          plotlyOutput("plot_regression")
#                        )
# ggplotly regression, geompoint, method=lm
# )))

# i just finished going through and finding exactly
# where i was missing a parentheses and that took so stupidly long im embarrassed.       


#### fifth tab (about) ####


# ------------------------------------------------- #
#### MY SERVER ####


server <- function(input, output) { 
  
  # output$summary <- renderUI({
  #     str1 <- ("There are many reasons around cartel violence and its effects 
  #              that result in rises of illegal immigration, particularly in women and children.")
  #     str2 <- paste("Violence is an intrinsic feature of the trade in illicit drugs. Traffickers use it to settle disputes, 
  #                                   and a credible threat of violence maintains employee discipline and a semblance of order with suppliers, 
  #                                   creditors, and buyers. This type of drug trafficking-related violence has occurred routinely and 
  #                                   intermittently in U.S. cities since the early 1980s.")
  #     str3 <- paste("The violence now associated with drug trafficking 
  #                                   organizations (DTOs) in Mexico is of an entirely different scale. In Mexico, the violence is not only 
  #                                   associated with resolving disputes or maintaining discipline but also has been directed toward the 
  #                                   government, political candidates, and the media. The excesses of some of 
  #                                   Mexican cartels' displays of violence through abductions, tortures, and executions are considered exceptional by the typical standards of organized crime.")
  #     str4 <- paste("Overall, the Latin America region has a significantly higher homicide levels than 
  #                                 other regions worldwide. According to the U.N.’s Global Study on Homicide 
  #                                 published in July 2019, with 13% of the world’s population in 2017, Latin America 
  #                                 had 37% of the world’s intentional homicides.")
  #     str5 <- ("Data Sources")
  #     str6 <- paste("Kaplan, Jacob. U.S. Customs and Border Protection Statistics and Summaries. Ann Arbor, MI: 
  #                                 Inter-university Consortium for Political and Social Research distributor, 2019-04-30. 
  #                                 https://doi.org/10.3886/E109522V2 Kaplan, Jacob. U.S. Customs and Border Protection Statistics 
  #                                   and Summaries: family_child_total_monthly_2000_2018.zip. Ann Arbor, MI: Inter-university Consortium 
  #                                   for Political and Social Research distributor, 2019-04-30. https://doi.org/10.3886/E109522V2-19923")
  #     str7 <- paste("INEGI, Mexican government agency that performs surveys")
  #     str8 <- paste("ok")
  #     str9 <- paste("ok")
  #     str10 <- paste("ok")
  #     str11 <- paste("test")
  #     str12 <- paste("test")
  #     str13 <- paste("Source: test")
  #     
  #     HTML(paste(h1(str1), p(str2), p(str3), p(str4), p(str5), p(str6), p(str7), p(str8), p(str9), p(str10), p(str11), p(str12), h6(str13)))
  # })
  
  # ------------------------------------------------- #
  #### OUTPUT FOR MAP ####
  
  output$map1 <- renderPlot({
    
    # only intentional homicides, from 2015 until now
    # if else statement
    # choose crime
    if(input$crime == "Homicide") {
      map_cartel_data_1 <- crimes %>%
        filter(subtipo == "HOMICIDIO DOLOSO") }
    
    else if(input$crime == "Feminicide") {
      map_cartel_data_1 <- crimes %>%
        filter(subtipo == "FEMINICIDIO") }
    
    else if(input$crime == "Sexual Trafficking of Children") {
      map_cartel_data_1 <- crimes %>%
        filter(subtipo == "CORRUPCIÓN DE MENORES") }
    
    else if(input$crime == "Assaults") {
      map_cartel_data_1 <- crimes %>%
        filter(subtipo == "LESIONES DOLOSAS") }
    
    else if (input$crime == "Human Trafficking") {
      map_cartel_data_1 <- crimes %>%
        filter(subtipo == "TRATA DE PERSONAS") }
    
    else if(input$crime == "Extortion") {
      map_cartel_data_1 <- crimes %>%
        filter(subtipo == "EXTORSIÓN") }
    
    else {
      map_cartel_data_1 <- crimes %>%
        filter(tipo == "SECUESTRO") }
    
    # for more options: another else if
    map_cartel_data_1 <- map_cartel_data_1 %>%
      group_by(state_code, state, date) %>%
      summarise(total = sum(count, na.rm = TRUE)) %>%
      arrange(state_code, state, date) %>%
      mutate(date = substr(date, 1, 4)) %>%
      # made the date just the year...
      group_by(state_code, date) %>%
      # group_by again with new date
      mutate(newtotal = sum(total)) %>%
      # make new values
      select(- total) %>%
      unique() %>%
      ungroup() %>%
      # changing column names to necessary ones for mxmaps package
      mutate(region = state_code) %>%
      mutate(value = newtotal) %>%
      select(- newtotal, - state_code)
    
    map_homicide_data_2019 <- map_cartel_data_1 %>%
      filter(date == toString(input$year)) %>%
      # to make it year selected by user
      select( - state, - date)
    
    
    map1 <- mxstate_choropleth(map_homicide_data_2019,
                               title = "Total by state")
    
    map1})
  
  # ------------------------------------------------- #
  #### OUTPUT FOR PERCEPTION PLOT ####
  
  output$plot_perception <- renderPlotly({
    
    perception_data <- perception %>%
      filter(state == input$state)
    
    plot_perception <- plot_ly(perception_data, 
                               x = ~year, 
                               y = ~percent, 
                               type = 'scatter', 
                               mode = 'lines', 
                               text = "Percentage of citizens over 18 who feel unsafe in their state and country")
    
    
    ggplotly(plot_perception)
  })
  
  
  # ------------------------------------------------- #                         
  #### OUTPUT FOR BP APPREHENSIONS PLOT ####
  
  output$plot_bp <- renderPlotly({
    
    # creating and defining apprehensions, what will be dependent variable in plotly:
    
    if(input$apprehensions == "Unaccompanied Children") {
      bp_fc <- bp_fc %>% mutate(apprehensions = unaccompanied_child_apprehension) }
    else if(input$apprehensions == "Total Apprehensions") {
      bp_fc <- bp_fc %>% mutate(apprehensions = total_apprehensions) }
    else { bp_fc <- bp_fc %>% mutate(apprehensions = family_apprehensions) }
    
    # making the actual plot here
    
    plot_bp <-
      plot_ly(bp_fc, x = ~fiscal_year, y = ~apprehensions,
              name = 'Border Patrol apprehensions', type = 'scatter', mode = 'lines')
    
    ggplotly(plot_bp)
    
  })
}


# ------------------------------------------------- #           
#### OUTPUT FOR REGRESSION TAB ####
# 
# set.seed(955)
# 
# bp_fc_regression <- bp_fc %>%
#   filter(month == "yearly total") %>%
#   filter(sector == "southwest border") %>%
#   select(unaccompanied_child_apprehension)
# 
# data_regression <- data.frame(year = unique(bp_fc$fiscal_year), 
#                               x = bp_fc_regression,
#                               y = homicides)
# 
# plot_regression <- ggplot(data_regression, aes(x=xvar, y=yvar)) +
#   geom_point(shape=1) +    # Use hollow circles
#   geom_smooth(method=lm)   # Add linear regression line
# 
# 
# plot_regression <- ggplotly(plot_regression)



# ------------------------------------------------- #
#### RUNNING APP ####
###########################################################################
## Run the application...im scared im scared...this is really the existence i live
##    maybe it works maybe it doesn't only god and preceptor could guess .... ##
# i am feeling existential now
###########################################################################

shinyApp(ui = ui, server = server)
