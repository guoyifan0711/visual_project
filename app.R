library(tidyverse)
library(shiny)
library(shinydashboard) 
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(DT)
library(plotly)
library(RColorBrewer)
library(mapproj)
library(mapdata)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tm)
library(colourpicker)
library(NLP)

######################################preload data#########################
country_code = read_csv("country_code.csv")
data = read_csv("globalterrorismdb_0522dist.csv")
all_country = data %>%
  distinct(country_txt) %>%
  pull(country_txt)

raw_map4_data <- data
data_2 = data %>% 
  select(iyear,imonth,iday,country_txt,latitude,longitude,nkill,nwound,propvalue) %>% 
  mutate(Date = str_c(iyear,'-',imonth,'-',iday) %>% as.Date()) %>% 
  select(-iyear,-imonth,-iday) %>% 
  rename('Country' = 'country_txt',
         'Lat' = 'latitude',
         'Lng' = 'longitude',
         'Kill' = 'nkill',
         'Wound' = 'nwound',
         'Property' = 'propvalue') %>% 
  filter(!is.na(Date),
         !is.na(Lat),
         !is.na(Lng),
         abs(Lng)<200) %>% 
  group_by(Date,Country) %>% 
  mutate(Count = n()) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(Kill,Wound,Property),
               names_to = 'Type',
               values_to = 'Value') %>% 
  filter(!is.na(Value))

world_map = map_data('world')

data_2 = data_2 %>%
  mutate(Country = case_when(
    Country=='United States' ~ 'USA',
    Country=='United Kingdom' ~ 'UK',
    Country=='West Germany (FRG)' ~ 'Germany',
    Country=='East Germany (GDR)' ~ 'Germany',
    Country=='South Yemen' ~ 'Yemen',
    Country=='West Bank and Gaza Strip' ~ 'Israel',
    Country=='South Vietnam' ~ 'Vietnam',
    Country=='Zaire' ~ 'Democratic Republic of the Congo',
    Country=="People's Republic of the Congo" ~ 'Republic of Congo',
    # Country=='Yugoslavia' ~ '',
    Country=='North Yemen' ~ 'Yemen',
    Country=='Trinidad and Tobago' ~ 'Tobago',
    Country=='Rhodesia' ~ '',
    Country=='Soviet Union' ~ 'Russia',
    Country=='Hong Kong' ~ 'China',
    Country=='New Hebrides' ~ 'UK',
    Country=='Vatican City' ~ 'Vatican',
    Country=='Czechoslovakia' ~ 'Slovakia',
    Country=='Republic of the Congo' ~ 'Republic of Congo',
    Country=='Bosnia-Herzegovina' ~ 'Bosnia and Herzegovina',
    Country=='Slovak Republic' ~ 'Slovakia',
    # Country=='Macedonia' ~ '',
    Country=='St. Kitts and Nevis' ~ 'Saint Kitts',
    Country=='Macau' ~ 'China',
    Country=='East Timor' ~ 'Timor-Leste',
    Country=='St. Lucia' ~ 'Saint Lucia',
    # Country=='International' ~ '',
    Country=='Serbia-Montenegro' ~ 'Serbia',
    T ~ Country))
##################################################################################

ui <- dashboardPage(
  
  dashboardHeader(title = "Global Terrorism"),
  dashboardSidebar(
    
    sidebarMenu(
      # part 1
      menuItem("About",
               tabName = "General Description", icon = icon("chart-pie"), selected = T,
               menuItem("General Information", tabName = "Analysis1", icon = icon("chart-line")),
               
               menuItem("About us", tabName = "Analysis3", icon = icon("chart-bar"))
      ),
      # part 2
      menuItem("Map", tabName = "map", icon = icon("map-o"),
               menuItem("Casualty Map", tabName = "map1", icon = icon("map-o")),
               menuItem(text = "Point Map", tabName = "map2", icon = icon("map-o")),
               menuItem(text = "Heat Map", tabName = "map3", icon = icon("map-o")),
               menuItem(text = "Region Map", tabName = "map4", icon = icon("map-o"))),
      # part 3
      menuItem("Event Search",tabName = "page3",icon = icon("folder",lib = "font-awesome")
      ),
      # part 4
      menuItem("Charts", tabName = "page4", icon = icon("area-chart")
      ),
      # part 5
      menuItem("Text analysis and dataset", tabName = "Analysis4",icon = icon("fas fa-book")
      )
    )),
  
  dashboardBody(
    
    tabItems(
#####################################################lzy ui part#############################################
      tabItem(
        tabName = "Analysis1",
        titlePanel("Global Terrorism Database"),
        br(),br(),
        
        fluidRow(
          box(
            title = "About the Project", solidHeader = TRUE,
            status = "success", width = 12, collapsible = TRUE,
            column(12, 
                   "Video: International Relations - The Challenge of Global Terrorism",
                   HTML(
                     '<iframe width="100%" height="600"
                  src="https://www.youtube.com/embed/WznOvObDk68"
                  frameborder="0" allowfullscreen></iframe>'
                   ),
                   tags$div(
                     tags$span(
                       p("This Dataset,",tags$strong("The Global Terrorism DatabaseT (GTD),"), 
                         " is an open-source database including information on terrorist events around the world from 1970 through 2020 (with annual updates planned for the future). Unlike many other event databases, the GTD includes systematic data on domestic as well as international terrorist incidents that have occurred during this time period and now includes more than 200,000 cases."),
                       
                       p("Also worth mentioning", tags$strong("in our databases"), 
                         ", Data through 2020 are now available. GTD Program Manager Dr. Erin Miller provides a virtual lecture exploring the most recent terrorism trends found in the Global Terrorism Database (GTD). Twenty years after University of Maryland researchers began developing the GTD in 2002, Miller presents trends from the upcoming publication of new GTD data for 1970 to 2020. Topics include patterns of terrorism in the United States and around the world during the first year of the COVID-19 pandemic, developments in Afghanistan leading up to the 2021 collapse of the Afghan government, and the evolving geographic footprint of Islamic State-related terrorism.For this time, we also used it."),
                       
                     )
                   )
            )
          )
        ),
        fluidRow(
          box(
            title = "About the research questions", solidHeader = TRUE,
            status = "success", width = 12, collapsible = TRUE,
            column(12, 
                   tags$div(
                     tags$span(
                       p(". How do we find the killed and wounded locations and the specific location of property damage over years?"),
                       p(". How do we search for the number of terrorism in different regions?"),
                       p(". How do we search for the overview of any terrorism event including country, city, number of death, attack type, target type and the attacker?"),
                       p(". What are the terrorist attacks in different regions of the world and in different years?"),
                       p(". What are the top 20 countries attacked mostly by terrorism?"),
                       p(". which year are the most attacked year by terrorist attacks?"),
                       p(". which attacks type have the highest number of global terrorism ?"),
                       p(". which target type have the highest number of global terrorism ?"),
                       p(". which weapon type will terrorists use most in global terrorism ?"),
                     )
                   )
            )
          )
        ),
        fluidRow(
          box(
            title = "About the Dataset", solidHeader = TRUE,
            status = "primary", width = 12, collapsible = TRUE,
            column(12, 
                   tags$div(
                     tags$span("Data from 1970 through 2020"),
                     br(),
                     tags$li(tags$strong("Source: "),tags$a(href = "https://www.kaggle.com/datasets/START-UMD/gtd", "GTD Data")),
                     tags$li("Twenty years after University of Maryland researchers began developing the GTD in 2002, Miller presents trends from the upcoming publication of new GTD data for 1970 to 2020. ",tags$strong("4,802"), "rows (in ", tags$strong("23"), "columns) ."),
                     br(),
                     tags$span(
                       "This database,",tags$strong("GTD,"), 
                       "Topics include patterns of terrorism in the United States and around the world during the first year of the COVID-19 pandemic, developments in Afghanistan leading up to the 2021 collapse of the Afghan government, and the evolving geographic footprint of Islamic State-related terrorism.", tags$strong("8"), "factors, including:"),
                     br(),
                     fluidRow(column(6, tags$li("iyear"), tags$li("counrty_txt"), tags$li("region_txt"), tags$li("city"), tags$li("nkill"), tags$li("nwound")),
                              column(6, tags$li("latitude"), tags$li("longtitude"), tags$li("summary"), tags$li("attacktype1_txt"),tags$li("targtype1_txt"),tags$li("weaptype1_txt")))
                   )
            )))),

      
      tabItem(
        tabName = "Analysis3",
        titlePanel("About US and References"),
        br(),br(),
        fluidRow(
          box(
            title = "About US", solidHeader = TRUE,
            status = "success", width = 12, collapsible = TRUE,
            column(12, 
                   tags$div(
                     tags$span(
                      p(". Yifan Guo",
                         tags$strong(a(href="https://www.linkedin.com/in/yifan-guo0711", "Linkedin")), " is a graduate student from Carey Business School of John Hopkins Univeristy."), ),
                     p(". Zhaoyu Li",
                       tags$strong(a(href="https://www.linkedin.com/in/zhaoyulidata/", "Linkedin")), 
                       " is a graduate student from Krieger School of Art and Science of John Hopkins Univeristy."),
                    p(". Siyi Peng", "is a graduate student from Carey Business School of John Hopkins Univeristy."), ),
                     p(". Yizhen Shen", 
                       " is a graduate student from Carey Business School of John Hopkins Univeristy."),
                     p(". Ying Wei", 
                       " is a graduate student from Carey Business School of John Hopkins Univeristy.")
                     
            )
          )),
        
        fluidRow(
          box(
            title = "References", solidHeader = TRUE,
            status = "success", width = 12, collapsible = TRUE,
            column(12, 
                   tags$div(
                     tags$span(
                       
                       p(". https://www.kaggle.com/datasets/START-UMD/gtd"),
                       p(".https://www.start.umd.edu/start-in-the-news?combine=Global+Terrorism+Database")
                       
                       ))))
        )),

      tabItem(
        tabName = "Analysis4",
        titlePanel("Text Mining & Analysis of Heavily-attacked Country Datasets"),
        br(),br(),
        tabsetPanel(
          tabPanel(
            "Wordcloud",
            sidebarLayout(
              sidebarPanel(
                radioButtons(
                  inputId = "source",
                  label = "Word source",
                  choices = c(
                    "Our Terrorism dataset"="book" ,
                    "Use your own words" = "own",
                    "Upload a file" = "file"
                  )
                ),
                hr(),
                # Add the selector for the language of the text
                selectInput(
                  inputId = "language",
                  label = "Remove stopwords in",
                  choices = c("Danish", "Dutch", "English", "Finnish", "French", "German", "Hungarian", "Italian", "Norwegian", "Portuguese", "Russian", "Spanish", "Swedish"),
                  multiple = FALSE,
                  selected = "English"
                ),
                conditionalPanel(
                  condition = "input.source == 'own'",
                  textAreaInput("text", "Enter text", rows = 7)
                ),
                # Wrap the file input in a conditional panel
                conditionalPanel(
                  # The condition should be that the user selects
                  # "file" from the radio buttons
                  condition = "input.source == 'file'",
                  fileInput("file", "Select a file")
                ),
                hr(),
                checkboxInput("remove_words", "Remove specific words?", FALSE),
                conditionalPanel(
                  condition = "input.remove_words == 1",
                  textAreaInput("words_to_remove1", "Words to remove (one per line)", rows = 1)
                ),
                conditionalPanel(
                  condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
                  textAreaInput("words_to_remove2", "", rows = 1)
                ),
                conditionalPanel(
                  condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
                  textAreaInput("words_to_remove3", "", rows = 1)
                ),
                conditionalPanel(
                  condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
                  textAreaInput("words_to_remove4", "", rows = 1)
                ),
                conditionalPanel(
                  condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
                  textAreaInput("words_to_remove5", "", rows = 1)
                ),
                conditionalPanel(
                  condition = "input.remove_words == 1 && input.words_to_remove5.length > 0",
                  textAreaInput("words_to_remove6", "", rows = 1)
                ),
                conditionalPanel(
                  condition = "input.remove_words == 1 && input.words_to_remove6.length > 0",
                  textAreaInput("words_to_remove7", "", rows = 1)
                ),
                conditionalPanel(
                  condition = "input.remove_words == 1 && input.words_to_remove7.length > 0",
                  textAreaInput("words_to_remove8", "", rows = 1)
                ),
                conditionalPanel(
                  condition = "input.remove_words == 1 && input.words_to_remove8.length > 0",
                  textAreaInput("words_to_remove9", "", rows = 1)
                ),
                conditionalPanel(
                  condition = "input.remove_words == 1 && input.words_to_remove9.length > 0",
                  textAreaInput("words_to_remove10", "", rows = 1)
                ),
                hr(),
                numericInput("num", "Maximum number of words",
                             value = 100, min = 5
                ),
                hr(),
                colourInput("col", "Background color", value = "white"),
                hr(),
                HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/word-cloud/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/word-cloud/blob/master/app.R">code</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a>.</p>')
              ),
              mainPanel(
                wordcloud2Output("cloud"),
                # br(),
                # br(),
                # tags$a(href="https://www.antoinesoetewey.com/", "Back to www.antoinesoetewey.com"),
                br(),
                br()
                ),),),
            tabPanel("Datatable", dataTableOutput("myTable", width="100%", height = 500))
          )
      ),
####################################################wy & psy ui##########################################
      tabItem(tabName = "map1",
              
              dateRangeInput(inputId = 'daterange1',
                             label = 'Date Range:',
                             start = '2005-01-01',
                             end = '2010-12-31',
                             min = min(data_2$Date),
                             max = max(data_2$Date)),
              
              radioButtons(inputId = 'type1',
                           label = 'Type:',
                           choices = c('Killed' = 'Kill',
                                       'Wounded' = 'Wound',
                                       'Property Damage' = 'Property'),
                           selected = 'Kill'),
              
              leafletOutput(outputId = "map1", width = "100%",height = "600")),
      
      tabItem(tabName = "map2",
              
              dateRangeInput(inputId = 'daterange2',
                             label = 'Date Range:',
                             start = '2005-01-01',
                             end = '2010-12-31',
                             min = min(data_2$Date),
                             max = max(data_2$Date)),
              
              radioButtons(inputId = 'type2',
                           label = 'Type:',
                           choices = c('Killed' = 'Kill',
                                       'Wounded' = 'Wound',
                                       'Property Damage' = 'Property'),
                           selected = 'Kill'),
              
              plotlyOutput(outputId = "map2", width = "100%",height = "600")),
      
      
      tabItem(tabName = "map3",
              
              dateRangeInput(inputId = 'daterange3',
                             label = 'Date Range:',
                             start = '2005-01-01',
                             end = '2010-12-31',
                             min = min(data_2$Date),
                             max = max(data_2$Date)),
              
              plotlyOutput(outputId = "map3", width = "100%",height = "750")),
      
      tabItem(tabName = "map4",
              # sliderInput("region", "Region:", min = 1, max = 12, value = 1, 
              #             step = 1, animate = animationOptions(interval = 5000, loop = TRUE)),
              radioButtons("region", "Region:",
                           c("Australasia & Oceania" = 1,
                             "Central America & Caribbean" = 2,
                             "Central ASia" = 3,
                             "East Asia" = 4,
                             "Eastern Europe" = 5,
                             "Middle East & North Africa" = 6,
                             "North America" = 7,
                             "South America" = 8,
                             "South Asia" = 9,
                             "Southeast Asia" = 10,
                             "Sub-Saharan Africa" = 11,
                             "Western Europe" = 12
                           )
              ),
              numericInput("year_start", "Begin Year[1970 - 2020]:", value = 1970),
              numericInput("year_end", "End Year[1970 - 2020]:", value = 2020),
              #textInput("txt","Enter the text to display below:"),
              textOutput("map4_text"),
              verbatimTextOutput("map4_verb"),
              leafletOutput("map4", width="100%")
      ),# end map4
########################################################gyf ui part#########################################      
        tabItem(tabName = "page3",
                h1("Global Terrorism Event Search"),
                fluidRow(
                  column(width = 3, 
                         box(title = h5("Find a Event",style = "font-size: 32px"),
                             solidHeader = TRUE,
                             collapsible = FALSE,
                             width = "100%",
                             status = "warning",
                             
                             selectInput(
                               inputId = "Country_search",
                               label = "Country",
                               choices = all_country
                             ),
                             
                             selectInput(
                               inputId = "Date_search",
                               label = "Date",
                               choices = ""
                             ),
                             selectInput(
                               inputId = "Event_id_search",
                               label = "Event ID",
                               choices = ""
                             )
                         ),
                         fluidRow(valueBoxOutput("summary",width = 12))
                  ),
                  
                  column(width = 5, 
                         fluidRow(box(title=h5("Event Overview",style = "font-size: 28px"),
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      width = 12,
                                      background = "blue",
                         )),
                         fluidRow(valueBoxOutput("loc",width = 12)),
                         fluidRow(column(width = 4, valueBoxOutput("event_death",width = "100%")),
                                  column(width = 8,valueBoxOutput("event_city",width = "100%"))),
                         fluidRow(valueBoxOutput("attack_type",width = 12)),
                         fluidRow(valueBoxOutput("target_type",width = 12)),
                         fluidRow(valueBoxOutput("attacker",width = 12)),
                         fluidRow(column(width = 6, valueBoxOutput("event_motive",width = "100%")),
                                  column(width = 6,valueBoxOutput("event_damage",width = "100%")))
                  ),
                  column(width = 4, 
                         fluidRow(uiOutput("country_flag")),
                         br(),
                         h3("Location View"),
                         leafletOutput("event_loc", width="100%"))
                  
                ))
      ,
###############################################syz ui part##################################################      
    tabItem(tabName = "page4",
            h2("Explore the Details of Global Terrorism"),
            fluidRow(
              box( 
                title = "How to Use", solidHeader = TRUE,
                status = "primary", width = "100%", collapsible = TRUE, collapsed = FALSE,
                h5(strong("*It may take a few seconds to load the chart"),
                h5(strong("six parts:")),
                h5(
                  "1. Terrorist Attacks in Different Rigions of the World"
                ),
                h5(
                  "2. Terrorist Attacks in Different Years"
                ),
                h5(
                  "3. Countries and Years Ranked by Total Terrorism"
                ),
                h5(
                  "4. Terrorism in Selected Countries and the Number of Successful Terrorist Attacks"
                ),
                h5(
                  "5. Number of Global Terrorist Attacks with Different Types"
                ),
                h5(
                  "6. Number of Global terrorist Attacks With Different Targets"
                  ),
                h5(
                  "7. Number of Global terrorist Attacks Using Different Weapons"
                   )
            )),
            fluidRow(
              box( 
                title = "Terrorist Attacks in Different Regions of the World", solidHeader = TRUE,
                status = "warning", width = 12, collapsible = TRUE, collapsed = FALSE,
                sliderInput("year", "Year:", min = 1970, max = 2020, value = 1970, 
                            step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                plotOutput("TotalTerrorisminEachRigion")
                )),
            fluidRow(
              box( 
                title = "Terrorist Attacks in Different Years", solidHeader = TRUE,
                status = "warning", width = 12, collapsible = TRUE, collapsed = FALSE,
                plotOutput("RegionTerrorisminEachYear")
              )),
            fluidRow(
              box(title = "Countries Ranked by Total Terrorism", solidHeader = TRUE,
                  status="danger", width=6, collapsible = FALSE, collapsed = FALSE,
                  h5("Iraq, Afghanistan, and Pakistan are the three countries with the highest total terrorism."),
                  plotOutput("CountriesRankedbyTotalTerrorism")),
              box(title = "Years Ranked by Total Terrorism", solidHeader = TRUE,
                  status="danger", width=6, collapsible = FALSE, collapsed = FALSE,
                  h5("2014, 2015, and 2016 are the three years with the most terrorist attacks."),
                  plotOutput("YearsRankedbyTotalTerrorism")
                  )),
            fluidRow(
              box(
                title = "Terrorism in Selected Countries and the Number of Successful Terrorist Attacks",
                solidHeader = TRUE,
                status = "danger",
                width = 12,
                collapsible = TRUE,
                fluidRow(column(2),
                         column(4,
                                fluidRow(
                                  selectInput(
                                    "selectregion",
                                    "Region:",
                                    choices = unique(data$region_txt)
                                  )
                                )),
                         column(4,
                                fluidRow(
                                  selectInput("selectcountry", "Country:", choices = all_country)
                                ))),
                plotlyOutput("terrorism")
                )),
            fluidRow(
              box(
                title = "Number of Global Terrorist Attacks with Different Types",
                solidHeader = TRUE,
                status = "danger",
                width = 12,
                collapsible = TRUE,
                plotlyOutput("attack")
                )),
            fluidRow(
              box(title = "Number of Global Terrorist Attacks with Different Targets",
                  solidHeader = TRUE,
                  status = "danger",
                  width = 12,
                  collapsible = TRUE,
                  plotlyOutput("target")
                )),
            fluidRow(
              box(title = "Number of Global Terrorist Attacks using Different Weapons",
                  solidHeader = TRUE,
                  status = "danger",
                  width = 12,
                  collapsible = TRUE,
                  plotlyOutput("weapon")
              )),
    ),
    ))
  ))

server <- function(input, output, session) {
####################################################pre-processing#################################################   
  data <- data %>% 
    mutate(weaptype1_txt = recode(weaptype1_txt, 'Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)'= 'Vehicle'))
  data$date = paste0(data$imonth,"/",data$iday,"/",data$iyear)
  data$date = as.Date(data$date,format = "%m/%d/%Y")
  
  data <- data%>%
    filter(!is.na(data$date))
  data <- data%>%
    filter(!is.na(data$latitude))
  data <- data%>%
    filter(!is.na(data$longitude))
################################################syz server part####################################################
  countryname = reactive({
    data %>% 
      filter(region_txt == input$selectregion) %>% 
      distinct(country_txt)
  })
  
  observe({
    updateSelectInput(session, "selectcountry", choices = countryname())
  })
  ##Total Terrorism Attack in Each Country
  country=data %>% 
    group_by(country_txt,iyear) %>%
    summarise(NC = n())
  ##Total Success Attack in Each Country
  countryS=data %>% 
    group_by(country_txt,iyear) %>%
    summarise(NS = sum(success))
  ##combine two datasets
  NC = country$NC
  countryS$NC = NC
 
  ##plot total T in each region each year
  output$TotalTerrorisminEachRigion = renderPlot({
   data %>% 
      group_by(region_txt,iyear) %>%
      summarise(N = n()) %>%
      filter(iyear == input$year) %>%
      ggplot(mapping = aes(x = region_txt, y = N))+
      geom_col(fill = "orange") +
      coord_flip()+
      ylim(0,8000)+
      geom_text(aes(label=N),vjust=-0.25, color="black")+
      labs(
        title = "Total Terrorism in Each Region",
        x = "Region",
        y = "Total Terrorism")
  })
  ##plot total T in each year of each region 
  output$RegionTerrorisminEachYear = renderPlot({
    data %>% 
      group_by(region_txt,iyear) %>%
      summarise(N = n()) %>%
      ggplot(mapping = aes(x = iyear, y = N)) +
      geom_line(aes(color = region_txt)) +
      theme(
        plot.title = element_text(size = rel(1), hjust = 0.5),
        legend.title = element_text(size = rel(0.8), face = "bold"),
        text = element_text(family = "Helvetica Neue"),
        panel.background = element_blank()
      ) +
      labs(x = NULL, y = NULL, title = "Terrorist Attacks in Different Years", color = "Region:  ") 
  })
  ##plot top20 terro country   
  output$CountriesRankedbyTotalTerrorism <- renderPlot({
    data %>% 
      group_by(country_txt) %>% 
      summarise(NTC = n()) %>% 
      arrange(desc(NTC)) %>% 
      slice(1:20) %>% 
      ggplot(mapping = aes(x = reorder(country_txt, NTC), y = NTC, fill=country_txt)) +
      geom_bar(stat="identity") + 
      scale_fill_manual(values=c("#143A51","#FFD66D","#143A51","#C49A68","#FFD66D",
                                 "#143A51","#143A51","#746C6B","#143A51","#746C6B",
                                 "#746C6B","#C49A68","#C49A68","#FFD66D","#FFD66D",
                                 "#C49A68","#C49A68","#746C6B","#FFD66D","#746C6B"))+
      coord_flip() +
      labs(
        title = "Top 20 Countries",
        x = NULL,
        y = "Total Terrorism",
        fill = "Country:  "
      )
  })
  ##plot top20 terro year
  output$YearsRankedbyTotalTerrorism <- renderPlot({
    data %>% 
      group_by(iyear) %>% 
      summarise(NTY = n()) %>% 
      arrange(desc(NTY)) %>% 
      slice(1:20) %>%
      ggplot(aes(x = reorder(iyear,NTY), y = NTY, fill = factor(iyear))) +
      geom_bar(stat="identity") + 
      scale_fill_manual(values=c("#FFD66D","#FFD66D","#FFD66D","#FFD66D","#C49A68",
                                 "#C49A68","#FFD66D","#C49A68","#C49A68","#C49A68",
                                 "#746C6B","#746C6B","#143A51","#143A51","#143A51",
                                 "#143A51","#143A51","#746C6B","#746C6B","#746C6B")) +
      coord_flip() + 
      labs(
        title = "Top 20 Years",
        x = NULL,
        y = "Total Terrorism",
        fill = "Year:  "
      )
  })
  ##plot selected country terrorism and success-year
  output$terrorism <- renderPlotly({
    countryS %>%
      filter(country_txt == input$selectcountry) %>%
      ggplot(mapping = aes(x = iyear)) +
      geom_line(aes(y = NC, color ='Terrorism')) + 
      geom_line(aes(y = NS, color='Success'), linetype="twodash") +
      labs(x = "Year", y = "N", title = "Total Terrorism and success", color="Category:") +
      scale_color_manual(values = c("red", "steelblue")) +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = rel(1), hjust = 0.5),
        legend.title = element_text(size = rel(0.8), face = "bold")
        )
  })
  ##plot attack type
  output$attack <- renderPlotly({
    data %>% 
      group_by(attacktype1_txt) %>%
      summarise(NAT = n())%>%
      ggplot(mapping = aes(x = attacktype1_txt,y = NAT)) +
      geom_col(fill = "#143A51") +
      coord_flip()+
      geom_text(aes(label=NAT),vjust=-0.25, color="black",size=2.5)+
      labs(
        title = "Attack Type",
        x = NULL,
        y = NULL)
  })
  ##plot target type
  output$target <- renderPlotly({
    data %>% 
      group_by(targtype1_txt) %>%
      summarise(NTA = n()) %>%
      ggplot(mapping = aes(x = targtype1_txt,y = NTA)) +
      geom_col(fill = "#143A51") +
      coord_flip()+
      geom_text(aes(label=NTA),vjust=-0.25, color="black", size= 2.5)+
      labs(
        title = "Target Type",
        x=NULL,
        y=NULL)
  })
  ##plot weapon type
  output$weapon <- renderPlotly({
    data %>% 
      group_by(weaptype1_txt) %>%
      summarise(NWE = n()) %>%
      ggplot(mapping = aes(x = weaptype1_txt,y = NWE)) +
      geom_col(fill = "#143A51") +
      coord_flip()+
      geom_text(aes(label=NWE),vjust=-0.25, color="black", size=2.5)+
      labs(
        title = "Weapon Type",
        x=NULL,
        y=NULL)
  })
  
  #########################################gyf server part###########################################
  
  date_search = reactive({
    data %>%
      filter(country_txt == input$Country_search) %>%
      distinct(date)}%>%
      pull(date))
  
  observe({updateSelectInput(session = session,
                             inputId = "Date_search",
                             choices = date_search())})
  
  event_id_search = reactive({data%>%
      filter(country_txt == input$Country_search)%>%
      filter(date == input$Date_search)%>%
      distinct(eventid)%>%
      pull(eventid)})
  
  observe({
    updateSelectInput(session = session,
                      inputId = "Event_id_search",
                      choices = event_id_search())
  })
  
  
  output$event_loc = renderLeaflet({
    
    m=data %>%
      filter(country_txt == input$Country_search)%>%
      filter(date == input$Date_search)%>%
      filter(eventid == input$Event_id_search)%>%
      leaflet() %>% 
      addTiles() %>%
      addProviderTiles(providers$Stamen.Toner)%>%
      addMarkers(lat = ~latitude,lng =~longitude)
  })
  
  output$loc = renderValueBox({valueBox(input$Country_search,"Country",icon= icon("globe"),color="yellow")})
  
  filtered_data <- reactive({data%>%
      filter(country_txt == input$Country_search)%>%
      filter(date == input$Date_search)%>%
      filter(eventid == input$Event_id_search)})
  
  output$attack_type = renderValueBox({valueBox(filtered_data()%>%
                                                  distinct(attacktype1_txt)%>%
                                                  pull(attacktype1_txt),"Attack Type",icon= icon("hammer",lib = "font-awesome"),color = "red")})
  
  
  output$target_type = renderValueBox({valueBox(filtered_data()%>%
                                                  distinct(targtype1_txt)%>%
                                                  pull(targtype1_txt),
                                                "Target Type",icon= icon("building"),color = "purple")})
  
  
  output$attacker = renderValueBox({valueBox(filtered_data()%>%
                                               distinct(gname)%>%
                                               pull(gname),"Attacker",icon= icon("user"),color = "navy")})
  
  
  output$summary = renderValueBox({valueBox("Event Summary",filtered_data()%>%
                                              distinct(summary)%>%
                                              pull(summary),icon= icon("book"),color = "olive")})
  
  output$event_death = renderValueBox({valueBox(filtered_data()%>%
                                                  distinct(nkill)%>%
                                                  pull(nkill),"Death Caused",icon= icon("skull",lib="font-awesome"),color = "green")})
  
  output$event_city = renderValueBox({valueBox(filtered_data()%>%
                                                 distinct(city)%>%
                                                 pull(city),"City",icon= icon("city"),color = "olive")})
  
  output$event_motive = renderValueBox({valueBox("Motive",
                                                 filtered_data()%>%
                                                   distinct(motive)%>%
                                                   pull(motive),icon= icon("question",lib="font-awesome"),color = "olive")})
  
  output$event_damage = renderValueBox({valueBox("Damage Caused",
                                                 filtered_data()%>%
                                                   distinct(propextent_txt)%>%
                                                   pull(propextent_txt),icon= icon("money-bill",lib="font-awesome"),color = "green")})
  
  data<- left_join(data,country_code,by=c("country_txt"="Name"))
  
  data$Code
  data$Code<- tolower(data$Code)
  
  
  output$country_flag <- renderUI({
    code = filtered_data()%>%
      distinct(Code)%>%
      pull(Code)
    img(src = paste0("https://flagcdn.com/w320/",code,'.png'),alt="Flag Not Available")
    
  })
  
  ##################################################################################################
  ################################wy & psy##########################################################
  output$map1 = renderLeaflet({
    
    temp_data = data_2 %>% 
      filter(Type==input$type1,
             Date>=as.Date(input$daterange1[1]),
             Date<=as.Date(input$daterange1[2]))
    
    temp_data %>% 
      leaflet() %>% 
      addTiles() %>%
      setView(lng = 0,lat = 0,zoom = 1) %>% 
      addHeatmap(lng = ~Lng,
                 lat = ~Lat,
                 intensity = ~log(Value+1),
                 radius = 4)
    
  })
  
  output$map2 = renderPlotly({
    
    temp_data = data_2 %>% 
      filter(Type==input$type2,
             Date>=as.Date(input$daterange2[1]),
             Date<=as.Date(input$daterange2[2]))
    
    ggplot() +
      geom_polygon(data = world_map,
                   mapping = aes(x = long,
                                 y = lat,
                                 group = group),
                   fill = 'grey',
                   color = 'grey60',
                   size = 0.25) +
      geom_point(data = temp_data,
                 mapping = aes(x = Lng,
                               y = Lat,
                               color = log(Value+1),
                               text = str_c(Country,': ',Value)),
                 alpha = 0.7,
                 size = 0.5) +
      scale_color_viridis_c(option = 'A',direction = -1) +
      coord_map() +
      labs(color = str_c('Intensity of ',
                         ifelse(input$type2!='Property',
                                str_c(input$type2,'ed'),
                                'Value of Property Damage (in USD)'))) +
      theme_void()
  })
  
  output$map3 = renderPlotly({
    
    temp_data = data_2 %>% 
      filter(Date>=as.Date(input$daterange3[1]),
             Date<=as.Date(input$daterange3[2])) %>% 
      select(Country,Date,Count) %>% 
      distinct() %>% 
      group_by(Country) %>% 
      summarise(Count = sum(Count,na.rm = T)) %>% 
      ungroup()
    
    combine_data = temp_data %>% 
      right_join(world_map,by = c('Country' = 'region'))
    
    ggplot() +
      geom_polygon(data = combine_data,
                   mapping = aes(x = long,
                                 y = lat,
                                 group = group,
                                 fill = log(Count+1),
                                 text = str_c(Country,': ',Count)),
                   color = 'grey',
                   size = 0.1) +
      scale_fill_gradientn(colours = brewer.pal(9,'Reds')) +
      coord_map() +
      labs(title = ' ') +
      theme_void() +
      theme(legend.position = 'none')
    
  })
  # Cara's Map 4 
  #----------------------------------
  raw_map4_data <- raw_map4_data %>% drop_na(latitude,longitude,region_txt)
  map4_data <- subset(raw_map4_data, select = c(iyear, region_txt, latitude, longitude) )
  region <- as.numeric(as.factor(map4_data$region_txt))
  map4_data <- data.frame(map4_data, region)
  
  output$map4 = renderLeaflet({
    loc_data <- map4_data %>% filter(   map4_data$region == input$region
                                        & map4_data$iyear >= input$year_start
                                        & map4_data$iyear <= input$year_end)
    
    # remove longitude outliers
    quartiles <- quantile(loc_data$longitude, probs=c(.25, .75), na.rm = FALSE)
    IQR <- IQR(loc_data$longitude)
    Lower <- quartiles[1] - 1.5*IQR
    Upper <- quartiles[2] + 1.5*IQR 
    loc_data <- subset(loc_data, loc_data$longitude > Lower & loc_data$longitude < Upper)
    
    # remove latitude outliers
    quartiles <- quantile(loc_data$latitude, probs=c(.25, .75), na.rm = FALSE)
    IQR <- IQR(loc_data$latitude)
    Lower <- quartiles[1] - 1.5*IQR
    Upper <- quartiles[2] + 1.5*IQR 
    loc_data <- subset(loc_data, loc_data$latitude > Lower & loc_data$latitude < Upper)
    dim(loc_data)
    long <- mean(loc_data$longitude)
    lat <- mean(loc_data$latitude)
    m <- loc_data %>% leaflet() %>% addTiles() %>%
      setView(long, lat, zoom=3) %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner")%>%
      addLayersControl(baseGroups = c("Toner", "OSM"),
                       options = layersControlOptions(collapsed = FALSE))%>%
      addCircles(lng = loc_data$longitude, lat = loc_data$latitude, weight = 1, radius = 50000)%>%
      addMarkers(lng = long, lat = lat, 
                 label = ~region_txt[1], 
                 labelOptions = labelOptions(noHide = T, textsize = "20px",
                                             style = list(
                                               "color" = "red",
                                               "font-family" = "serif",
                                               "font-style" = "italic",
                                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                               "font-size" = "15px",
                                               "border-color" = "rgba(0,0,0,0.5)"
                                             )))
    
  })#end map4
  output$map4_text <- renderText("Number of Terrorism for this region in this timeframe")
  output$map4_verb <- renderText({
    loc_data <- map4_data %>% filter(   map4_data$region == input$region
                                        & map4_data$iyear >= input$year_start
                                        & map4_data$iyear <= input$year_end)
    nrow(loc_data)
  }) #end output verb
  
  data_source <- reactive({
    if (input$source == "book") {
      ww <- pull(data,country_txt)
    } else if (input$source == "own") {
      ww <- input$text
    } else if (input$source == "file") {
      ww <- input_file()
    }
    return(ww)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  create_wordcloud <- function(ww, num_words = 100, background = "white") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(ww)) {
      corpus <- Corpus(VectorSource(ww))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      ww <- sort(rowSums(tdm), decreasing = TRUE)
      ww <- data.frame(word = names(ww), freq = as.numeric(ww))
    }
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    ww <- head(ww, n = num_words)
    if (nrow(ww) == 0) {
      return(NULL)
    }
    
    wordcloud2(ww, backgroundColor = background)
  }
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })
  
  output$myTable = renderDataTable({
    data %>% select(-"summary") %>% datatable(rownames = FALSE)
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
}

shinyApp(ui = ui, server = server)
  
