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

crimes <- read.csv("./raw-data/nm-estatal-victimas-2.csv") 

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

# images from a static, public google drive
dir_create("graphics")
# 
# download.file("http://drive.google.com/file/d/1d5beiQii7VfYri15w2CJNRPLlS27oyVb/view?usp=sharing", 
#               destfile = "./graphics/profile.JPG")

# download.file("http://drive.google.com/file/d/1kh84LPvwTyXXoToMwNqVQEXii7dR57__/view?usp=sharing", 
#               destfile = "./graphics/joseluis.JPG")

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
                           selected = "Homicide"),
               sliderInput("year", "Select year",
                           min = 2015, max = 2019, value = 2019, sep=""),
               helpText("Violence Over Time. Data From: CONAPO, Mexican government agency on
                                  crime and law enforcement. All crime data used is taken from the publicly
                                  available compilation spreadsheet updated regularly with data by each state's
                                  government.")),
             mainPanel(
               h2("Map of Violent Crimes in Mexico Over Time"),
               h5("We can track trends in cartel wars and high-profile crimes
               and their consequential effects on public perceptions of 
                  insecurity; see next tab."),
               tabsetPanel(
                 tabPanel(
                   title = "Map of Violence Over Time",
                   plotOutput("map1"))),
               h5("Mexico is suspended in a blood bath that has spanned the last two decades, perpetuated by
organized crime, infiltrating the police and the press, and terrorizing Mexican citizens. Most commonly
characterized by violent crimes including executions, abductions and disappearances of hostages, human
trafficking, and torture; drug trafficking organizations (DTOs) have plagued the country and maintained a
great level of control over the police and the press for a combative period spanning over the last twenty
years."),
h5("Grisly public displays of torture are one of the hallmarks of DTOs, with Mexico’s intentional
homicide rate setting new records each year, with widely-publicized beheadings, public hangings of
corpses, car bombs, and murders of journalists and public officials, often with messages attached.
The press has been notably gagged, many times targeted by cartels in similar grisly public
displays of torture."),
h5("The institutions of law enforcement and of the press are similarly characterized by severe human
rights violations, many times corrupt or otherwise compromised. The 2014 disappearance of 43 students,
attacked by police officers that were connected to a cartel, is one example of a trauma that deeply
mangled the public’s perception of law enforcement and safety."),
               h5("The gagging of the press has made it difficult to track exactly why perceptions of danger
fluctuate so much; a large part of crime committed by DTOs goes unreported.
The most commonly-accepted assumption among different analysts maintains that up to 50% of
total intentional homicides in Mexico are connected to organized crime, with the violence reaching a new
scale as DTOs direct their efforts further than simply disputing or disciplining, but also towards the
government, the media, and politicians."),
h5("The shifts on the choropleth map visualizing the spikes and falls in homicides, abductions, and other
violent crimes allow us to track the movement of the Mexican drug war over time. For instance, the state
with the highest murder rate in 2019, Guanajuato (see heat map) was precisely where two massive drug
cartels, Jalisco New Generation and Santa Rosa de Lima, have been combatting for control since 2016."),
               
             )
           )
  ),
  # ------------------------------------------------- #
  #### second tab (perceptions of insecurity) ####
  tabPanel(
    title = "Public Perceptions of Insecurity in Mexico",
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
        h5("Public Perceptions of Insecurity in Mexico. Data from INEGI.")),
      mainPanel(
        h2("Public Perceptions of Insecurity in Mexico"),
        h5("Over time, the drug war and its effects have brought the vast majority of Mexicans, 
        especially those living in border states most affected by high-profile crimes, to 
           feel unsafe."),
        plotlyOutput("plot_perception"),
        h5("This plot displays the percentage of Mexican 
           citizens over 18 who feel unsafe in their state as surveyed each year.
           This graph showcases the stark
differences between the most problematic states, like border states such as Tamaulipas and Sonora, and
less problematic states in undisputed territories. It also showcases the very marked spikes and drops in
public perceptions of danger across a majority of states, enabling us to pinpoint years like 2012 (a change
in administration) as specific points in time when citizens felt hopeful for the regime change and
consequently felt more confident in their safety.",),
        h5("The public perception of crime in Mexico is a difficult thing to study; according to the National
Institute of Statistics and Geography, 92 percent of crimes go unreported in Mexico, largely due to the
time-consuming, potentially dangerous, and deterrent requirements for filing a criminal complaint.
These crimes are also strategically orchestrated to be especially grisly and merciless and public, in order
to stoke fear and send messages to politicians or journalists. Furthermore, the communication of these
crimes is largely tied to social media including Whatsapp, which has such large influences in Latin
America that it can even sway elections."),
h5("In the deadliest year yet for Mexico according to the National Public Security System, we can
look back and try to identify why 17,608 people were murdered in Mexico in just the first six months.9
Why has the murder rate tripled since 2007, the beginning of Calderon’s presidency? Analysts cite the
rate of political turnover in various levels of government, as well as the factionalism of splintered cartels
as DTOs expand into other criminal ventures and strengthen their hold over the government.
Combined with the particular social landscape in Mexico -- the problem of massive inequality
and an inefficient governing body, which have aided corruption and driven recruits to cartels -- the
country has little infrastructure to enforce laws to begin with. A low tax rate (18.4 percent) and low
minimum wage (80 pesos, or $4.17, a day) translates into poorly-paid and very easily corruptible law
enforcement."),
h5("When studying this, it can be useful to look at towns such as Morelia, Michoacan, a state that is
similarly a hotbed for cartel violence -- this city has been profiled for its mystifying ability to remain
remarkably peaceful and only improving while in the center of bitterly contested land. Upon questioning
the police force of the town, which is largely made up of civilians, the New York Times found that the
“broken windows” strategy of cracking down on lower-level neighborhood crimes was effective,
introducing new civil courts for misdemeanors. Overhauling their local police force and subsidizing changes within the city are, however, claimed to be
unreproducible by experts who highlight the forces riddled with problems already co-opted by drug
gangs, and the impossibility of reform. Experts speculate that the only path towards reform begins with
first creating social stability."),
        )
      
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
        h2("Border Patrol Apprehensions on the Southwest U.S. Border by Year"),
        tabsetPanel(
          tabPanel(
            title = "Border Patrol Apprehensions",
            plotlyOutput("plot_bp")
          )
        )
      )
    )
  ),
  
  
  
  # ------------------------------------------------- #
  #### fourth tab (regression) ####
  
  tabPanel("Causal Relationship",
           h3("Relationship Between Public Perceptions of Violence 
                   and Attempts at Illegal Immigration"),
           plotOutput(outputId = "plot_regression")
  ),
  
  
  
  #### fifth tab (about) ####
  
  tabPanel("About",
           h3(a("Camila Sanmiguel Anaya", href="https://www.camilasanmiguel.com")),
           h5("I am a student at Harvard graduating in 2022 and studying
                            History & Literature, captivated with data science
                            and Latin American law and history."),
           imageOutput("profile", width = "100%", height = "100%"),
           h5("Contact me at camilasanmiguel@college.harvard.edu or find me on my website", 
              a("here.", href="https://www.camilasanmiguel.com")),
           h5("Find me on LinkedIn",
              a("here.", href="https://www.linkedin.com/u/camilasanmiguel")),
           h6("These numbers are easy to treat coldly. It is important to remember their magnitude; that there are real
humans and families behind all the violence and the cosmos of different perceptions, and that there is a
country of millions in desperate need of drastic political aid and reform from the outside as well as the
inside."),
           h6("This project is a tribute to Jose Luis Anaya."),
           imageOutput("joseluis", width = "100%", height = "100%")
  ),
  
  tabPanel("References",
           mainPanel(
             h2("References"),
             h4("Data Sources"),
             h6("“Encuesta Nacional de Victimización y Percepción Sobre Seguridad Pública (ENVIPE) 2019.” Accessed December 15, 2019. https://www.inegi.org.mx/programas/envipe/2019/default.html#Tabulados."),
             h6("Kaplan, Jacob. U.S. Customs and Border Protection Statistics and Summaries. Ann Arbor, MI:
                                   Inter-university Consortium for Political and Social Research distributor, 2019-04-30.
                                   https://doi.org/10.3886/E109522V2 Kaplan, Jacob. U.S. Customs and Border Protection Statistics
                                   and Summaries: family_child_total_monthly_2000_2018.zip. Ann Arbor, MI: Inter-university Consortium
                                   for Political and Social Research distributor, 2019-04-30. https://doi.org/10.3886/E109522V2-19923"),
             h4("Research and News"),
             h6("Ahmed, Azam. “In Mexico, ‘It’s Easy to Kill a Journalist.’” The New York Times, April 29, 2017, sec. World. https://www.nytimes.com/2017/04/29/world/americas/veracruz-mexico-reporters-killed.html."),
             h6("Ahmed, Azam. “Mexico’s Deadliest Town. Mexico’s Deadliest Year.” The New York Times, August 4, 2017, sec. World. https://www.nytimes.com/2017/08/04/world/americas/mexicos-drug-killings.html."),
             h6("Beittel, June S. “Mexico: Organized Crime and Drug Trafficking Organizations,” n.d., 35.
“Criminal Violence in Mexico.” Global Conflict Tracker. Accessed December 18, 2019. https://cfr.org/interactive/global-conflict-tracker/conflict/criminal-violence-mexico."),
             h6("Dalby, Chris. “Why Are More People Being Killed in Mexico in 2019?” InSight Crime(blog), August 8, 2019. 
                            https://www.insightcrime.org/news/analysis/why-are-more-mexicans-being-killed-2019/."),
             h6("Fisher, Max, and Amanda Taub. “Mexico’s Record Violence Is a Crisis 20 Years in the Making.” The New York Times, October 28, 2017, sec. World. https://www.nytimes.com/2017/10/28/world/americas/mexico-violence.html."),
             h6("“In Mexico, Violence Flares Up Again in the Border City of Nuevo Laredo.” Stratfor. Accessed December 15, 2019. https://worldview.stratfor.com/article/mexico-violence-flares-border-city-nuevo-laredo-cdn."),
             h6("Martin, Laura. “Research Guides: Mexico’s Mass Disappearances and the Drug War (Ayotzinapa: The Missing 43 Students): Drug War Timeline 1930-2015.” Accessed December 18, 2019. https://researchguides.library.wisc.edu/c.php?g=560513&p=3904772."),
             h6("Mosqueda, Priscila. “Mexican Drug Cartels Are Using Social Media Apps to Commit Virtual Kidnappings.” Vice(blog), September 17, 2014. https://www.vice.com/en_us/article/jmbkek/mexican-cartels-are-using-social-media-apps-to-commit-virtual-kidnappings-917."),
             h6("Villegas, Paulina. “As Violence Soared in Mexico, This Town Bucked the Trend.” The New York Times, September 1, 2018, sec. World. https://www.nytimes.com/2018/09/01/world/americas/mexico-violence-police.html."),
             h6("“‘We’re Going to Find You.’ Mexican Cartels Turn Social Media into Tools for Extortion, Threats and Violence.” Accessed December 15, 2019. https://www.desertsun.com/in-depth/news/2019/02/27/mexican-drug-cartels-use-social-media-for-extortion-threats-violence-facebook-whatsapp-youtube/2280756002/."),
             h6("Woody, Christopher. “Mexico’s Rising Violence and a Showdown between Cartels Are Driving a Grisly Trend in the Country’s 2nd-Biggest City.” Business Insider. Accessed December 15, 2019. https://www.businessinsider.com/cartels-violence-driving-homicides-disappearances-in-guadalajara-2019-9.")
             
             
           )),
  tabPanel("Info PDF",
           HTML('<iframe src=\"https://drive.google.com/file/d/1mVFFlJ8cQlSHHEbZ6s_mrYX1lxflWbGx/preview\" 
                         width=\"900\" height=\"600\"></iframe>')
  )
  
  
  # this final parentheses is the navbar closing
)                      

# ------------------------------------------------- #
#### MY SERVER ####


server <- function(input, output) { 
  
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
    
    # for more options, just insert another else if above
    # 
    # 
    # if(map_cartel_data_1$count == 0) {
    #   map_cartel_data_1 <- crimes %>%
    #     mutate(count == sum(map_cartel_data_1$count, 1)) }
    
    map_cartel_data_1 <- map_cartel_data_1 %>%
      mutate(date = substr(date, 1, 4)) %>%
      group_by(state_code, state, date) %>%
      # ifelse argument needs 3 arguments, test, truth value, false value
      mutate(count = ifelse(count == 0, 10, count)) %>%
      summarise(total = sum(count, na.rm = TRUE)) %>%
      arrange(state_code, state, date) %>%
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
                               text = "Percentage of Mexican citizens over 18 who feel unsafe in their state",
                               xaxis_title = "Year",
                               yaxis_title = "Percent of citizens who feel unsafe"
    )
    
    
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
              name = 'Border Patrol Apprehensions Along the Southwest Border', type = 'scatter', mode = 'lines', sep="",
              xlab = 'Year', ylab = 'Number of apprehensions')
    
    ggplotly(plot_bp)
    
  })
  
  
  
  # ---------------------------------------------------- #           
  #### OUTPUT FOR REGRESSION TAB ####
  
  
  # QUESTION I WANT TO ANSWER:
  # how accurate are perceptions and consequently,
  # what is effect on attempts to leave, based on crime #s over the years? 
  
  
  # x = dependent variable = TARGET = perception
  # y = independent variables = PREDICTORS = crime
  
  
  # scratch disregard this bit
  # x_1 <- perception %>%
  #   filter(state == "Estados Unidos Mexicanos") %>%
  #   # the crisis has been historically concentrated in border states...
  #   # these are states with high insecurity perception rates.
  #   filter(year >= 2010) %>%
  #   select(percent)
  
  x_1 <- bp_fc %>%
    filter(month == "yearly total") %>%
    filter(sector == "southwest border") %>%
    filter(fiscal_year >= 2010) %>%
    select(total_apprehensions, unaccompanied_child_apprehension)
  
  y_1 <- crimes %>%
    filter(subtipo %in% c("HOMICIDIO DOLOSO")) %>%
    mutate(date = substr(date, 1, 4)) %>%
    group_by(state_code, state, date) %>%
    # ifelse argument needs 3 arguments, test, truth value, false value
    mutate(count = ifelse(count == 0, 10, count)) %>%
    summarise(total = sum(count, na.rm = TRUE)) %>%
    arrange(state_code, state, date) %>%
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
    select(- newtotal, - state_code, - region, - state) %>%
    head(9)
  
  # the below is necessary or else it just doesn't work 
  x_1 <- data.frame(words = unlist(words))
  y_1 <- data.frame(words = unlist(words))
  
  
  reg_perception <- data.frame(x_1, y_1)
  
  
  output$plot_regression <- renderPlot({
    # making a very simple regression model
    
    ggplot(reg_perception, aes(x = x_1$total_apprehensions, y= y_1$value)) +
      geom_point(shape=1) +    
      geom_smooth(x = x_1$total_apprehensions, y= y_1$value, 
                  method=lm, se=FALSE,
                  color ="brown4", fill = "deepskyblue4") +
      labs(title = "Relationship Between Violent, High-Profile Crimes in Mexico
         and Perceptions of Danger",
           subtitle = "There are numerous reasons that Mexicans' perceptions
              of danger can grow disproportionately to the growth of actual crime; 
              high-profile, grisly fear tactics/crimes
              used by cartels, along with the gagging of the press, has
              colored the public's perception in different ways over the years.",
           x = "Border Patrol Apprehensions Along the Southern Border",
           y = "Numbers of Violent, High-Profile Crimes",
           caption = "Statistical regression analysis, data from
         INEGI government census surveys, the
         Mexican Justice System, and U.S. CBP")
    
    
    
  })
  
  
  
  # ------------------------------------------------- #
  #### OUTPUT FOR IMAGES ####
  
  output$profile <- renderImage({
    # Return a list containing the filename and alt text
    list(src = './graphics/profile.JPG',
         height = 300,
         style="display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE
  )
  
  output$joseluis <- renderImage({
    list(src = './graphics/joseluis.jpg',
         height = 300,
         style="display: block; margin-left: auto; margin-right: auto;")
  }
  )
  
  # ------------------------------------------------- #
  
  # below is final bracket for whole output
}

# ------------------------------------------------- #

#### RUNNING APP ####
## Run the application...im scared im scared...this is really the existence i live
## maybe it works maybe it doesn't only god and preceptor could guess ....
# i am feeling existential now

# ------------------------------------------------- #

shinyApp(ui = ui, server = server)
