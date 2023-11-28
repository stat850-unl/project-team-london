options(readr.show_col_types = FALSE)
#install.packages("tidytuesdayR")

# Read in with tidytuesdayR package
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

# tuesdata <- tidytuesdayR::tt_load('2021-07-27')
# tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
#
# olympics <- tuesdata$olympics

# Or read in the data manually

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

#Analysis 2 - AC: Shiny app: Input your age, weight, height and gender. Spits out who medaled in what sport with those characteristics.
#essentially we need to omit anyone who hasn't medaled.
medalist <- olympics %>% na.omit()

library(flexdashboard)
library(dplyr)
library(tibble)
library(shiny)
library(purrr)
library(DT)
library(shinydashboard)
library(rsconnect)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)



# start of application
#Get sidebar ready and tab names set fot the body.
sidebar <-  dashboardSidebar(
  width = 250,
  sidebarMenu(id = "tabs",
              menuItem("Dashboard",
                       tabName = "dashboard",
                       selected = TRUE,
                       startExpanded = TRUE,
                       menuItem("About us", tabName = "about"),
                       menuItem("Olympian's physical characteristics", tabName = "subitem1"),
                       menuItem(HTML("Help desk:<br/> imperial to metric conversion"),  tabName = "Imperial")

              )
  )
)

body <- dashboardBody(
  tabItems(
    #setting about us tab for fun!
    tabItem(tabName = "about",
            userBox(
              title = userDescription(
                title = "Addison Carroll",
                subtitle = "PhD Animal Science",
                type = 2,
                image = "https://lh3.googleusercontent.com/drive-viewer/AK7aPaBIIZV2XU74yuhj4R5lPX64CaCUVSttH8XY3ZrIJ0fqAhA5U7rre_nbHLA_xB_89CGqvb7LTTf_w71QerBAMYIinqaJWw=s1600?source=screenshot.guru",
              ),
              status = "primary",
              gradient = TRUE,
              background = "black",
              collapsible = FALSE,
              width = 6,
              boxToolSize = "xl",
              "Addison Carroll is a PhD with a focus in dairy cattle nutrition. SHe enjoys metalcore music,  her Aussie Bear, and exploring data for hidden relationships.",
              footer = NULL
            ),
            userBox(
              title = userDescription(
                title = "Sydney Graham",
                subtitle = "PhD Plant Breeding and Genetics",
                type = 2,
                image = "https://lh3.googleusercontent.com/drive-viewer/AK7aPaAPCKrWGfEz9fVP6iwkkrl6Lkb066jRR4UF4fRosVyGdnxSew6GQFdBpWPWyZ8e-dvaQACNiEK1b1PJmgclP_waJ54www=s1600?source=screenshot.guru",
              ),
              status = "primary",
              gradient = TRUE,
              background = "maroon",
              collapsible = FALSE,
              width = 6,
              boxToolSize = "xl",
              "Sydney Graham is a PhD in plant breeding and genetics. Outside of work she enjoys cooking, spending time with her family....",
              footer = NULL
            )),
    #setting second sub item which will be the main focus/game.
    tabItem(tabName = "subitem1",
            fluidPage(
              titlePanel(div("Influence of physical characteristics on medaling in Olympic games.. See how you stack up!",
                             style = "color: #000000")),
              fluidRow(
                #Age slider to explore the influence of age on medaling.
                #Slider is a red color
                column(12,
                       chooseSliderSkin("Shiny", color = "red"),
                       sliderInput("age",
                                   "Age range",
                                   min= min(medalist$age),
                                   max= max(medalist$age),
                                   value = c(min(medalist$age), max(medalist$age)),
                                   width = '100%'),
                       br()
                ),
                #Gender input with unique so values aren't repeated .
                #background red color and text white
                column(6,
                       wellPanel(style = "background-color: #960018; color: #FFFFFF;",
                                 selectInput("sex",
                                             "Gender:",
                                             c("All",
                                               unique(as.character(medalist$sex)))))
                ),
                #Season input with unique so values aren't repeated
                #background red color and text white
                column(6,
                       wellPanel(style = "background-color: #960018; color: #FFFFFF;",
                                 selectInput("season",
                                             "Summer or Winter season:",
                                             c("All",
                                               unique(as.character(medalist$season))))),
                       br()
                ),
                #weight picker with unique so values aren't repeated and sorted.
                #background red color and text white
                column(6,
                       wellPanel(style = "background-color: #960018; color: #FFFFFF;",
                                 pickerInput(inputId = "weight",
                                             label ="Weight (kg)",
                                             choices = c("All", unique(sort(medalist$weight)),
                                                         multiple = FALSE
                                             )))

                ),
                #height picker with unique so values aren't repeated and sorted.
                #background red color and text white
                column(6,
                       wellPanel(style = "background-color: #960018; color: #FFFFFF;",
                                 pickerInput(inputId = "height",
                                             label ="Height (cm)",
                                             choices = c("All", unique(sort(medalist$height))),
                                             multiple = FALSE
                                 )
                       )
                ),
                #giveing the tab a background color
                tags$style('.container-fluid { background-color: #FFFFFF;}'),
                tabsetPanel(tabPanel("outcome", DT::dataTableOutput("table")))
              )
            )
    ),
    #This is our tab to do imperial to metric conversions
    tabItem(tabName = "Imperial",
            h2("All data is in metric.. Here is the conversion for us dummies who don't use it!"),
            wellPanel(style = "background-color: grey;",
                      numericInput("myInput1", label = "Type in height in inches", value = 1),
                      uiOutput("ui1")),
            wellPanel(style ="background-color: #960018;",
                      numericInput("myInput2", label = "Type in weight in lbs", value = 1),
                      uiOutput("ui2"))
    )
  )
)

#Pulling together the dashboard with the sideboard and body.
ui <- dashboardPage(skin = "midnight",
                    dashboardHeader(title =  span(tagList(icon("earth-americas"), "olympics"))),
                    sidebar,
                    body
)


server <- function(input, output, session) {

  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- medalist[medalist$age >= input$age[1] & medalist$age <= input$age[2],]
    #now give the option to look at all or specific values
    if (input$sex != "All") {
      data <- data[data$sex == input$sex,]
    }
    if (input$season != "All") {
      data <- data[data$season == input$season,]
    }
    if (input$weight != "All") {
      data <- data[data$weight == input$weight,]
    }
    if (input$height != "All") {
      data <- data[data$height == input$height,]
    }
    data
  }) %>%
    #puts a background on the table so it isn't tansparent
    DT::formatStyle(colnames(data), target = 'cell', backgroundColor = 'white', opacity = 1)

  )

  #Inputs and output for height and weight calculations
  output$ui1 <- renderUI( {
    tagList(
      numericInput("obs1", "Height in cm", value = input$myInput1*2.54))
  })
  output$ui2 <- renderUI( {
    tagList(
      numericInput("obs1", "Weight in kg", value = input$myInput2*0.453592))
  })
  observe({print(input$tabs)})
}


# Run the application
shinyApp(ui, server)
