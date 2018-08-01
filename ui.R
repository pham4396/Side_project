library("ggplot2")
library("dplyr")
library(tidyr)
library(shiny)
library(rsconnect)
university_bands <- read.csv('University_bands.csv', stringsAsFactors = F)
colnames(university_bands) <- c("University", "Mascot", "Conference", "Auditions?", "Colorguard?",
                                "Number of Members", "Marching Style", "Band's Name", "website", "entry accurate?")
remove_col <- function(data, index) {
  yes <- data[index * -1]
}
university_bands_without_websites <- remove_col(university_bands, 9)
column_names <- colnames(university_bands_without_websites)

colnames(university_bands_without_websites)
my_ui <- fluidPage(
  titlePanel("Division 1 University Marching Bands"),
    sidebarLayout(
      sidebarPanel(
        h3("Filters"),
        radioButtons("conference_filters", label = "Which conference(s) would you like to look at?",
                           choices = unique(university_bands_without_websites$Conference)
        ),
        sliderInput("members_filter", label = "How many members?",
                     min = min(university_bands_without_websites$`Number of Members`),
                     max = max(university_bands_without_websites$`Number of Members`),
                     value = c(min(university_bands_without_websites$`Number of Members`), 
                               max(university_bands_without_websites$`Number of Members`))
        ),
        radioButtons("colorguard_filter", label = "Would you like to see bands with a color Guard?",
                           choices = c("Yes, Only bands with a Color Guard", "No, All Bands", "No, Only bands without a Color Guard")
        
        ),
        radioButtons("audition_filter", label = "Auditions?", 
                          choices = unique(university_bands_without_websites$`Auditions?`)
        ),
        # radioButtons("style_filter", label = "Which marching Style would you like to look at",
        #                    choices = c("Chair Step", "Ankle Knee Step", "Straight Leg", "Roll Step", "Glide Step", "Scatter", "Corps Style")
        # ),
        uiOutput("conference_filters")
      ),
    
    mainPanel(
      h3("A Shiny App that list university marching bands along with some helpful information"),
      tabsetPanel(type = "tabs", 
        tabPanel("Table", dataTableOutput('table')),
        tabPanel("Marching Styles", h2("An explaination of the different types of marching styles used by university bands"),
                p("Chair Step : Chair Step Known as the traditional form of marching is when a marcher lifts
                Their Leg till their thigh is parallel to the ground and their calf is perpindicular to the
                ground while also point their toes towards the ground. At this point the leg looks like a 'chair'."),
                p("Straight Leg: A very clean marching Style where the leg does not bend. when a band marches with this style
                  it will look like its member's legs are sciccors cutting the air."),
                p("Glide/Roll Step : The most common form of marching at both the high school and collegiate level, Glide/Roll step
                  minimizes movment of the upper body. The feet 'roll' when marching absorbing schock during movement. Most bands march
                  chair step during their pre-game show, and use Roll/Glide for their halftime show, though some bands use chair step for both"))
      )
    )
    )  
  )


my_server <- function(input, output, session ){
  
  band_filters <- reactive({
    bands <- university_bands_without_websites %>%
      filter(
        input$conference_filters == Conference &
            input$audition_filter == `Auditions?` & 
               `Number of Members` >= input$members_filter [1] &
               `Number of Members` <= input$members_filter [2]  &
          if (input$colorguard_filter == "Yes, Only bands with a Color Guard") {
            `Colorguard?` == "Yes"
          } else if (input$colorguard_filter == "No, All Bands") {
            `Colorguard?` == "Yes" | `Colorguard?` == "No"
          } else {
            `Colorguard?` == "No"
          }
        
      )
    return(bands)
  })
  #works but fix when no conferences are selected
  output$table <- renderDataTable(band_filters(), options = list(pageLength = 16))
  return('table')
}



shinyApp(my_ui, my_server)

