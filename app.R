library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(readr)



#...Importing the dataset
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
coronavirus <- read_csv(url)

#...Function to prepare the dataset
prepareDataset <- function(db){
    coronavirus <- db %>%
        dplyr::filter(location != "World", location != "International") %>%
        dplyr::rename(Country = location,
                      Deaths = new_deaths,
                      Cases = new_cases,
                      Date = date)
}

#...Function to prepare the countries list to be used in the dropdown selection list
prepareCountryList <- function(db) {
    countries <- db$Country %>% unique() %>% as.vector()    
}

#...Prepare dataset 
coronavirus <- prepareDataset(coronavirus)

#...Prepare countries list
countries <- prepareCountryList(coronavirus)

#...Defining User Interface
ui <- fluidPage(

    #...Application title
    titlePanel("Trend of Coronavirus epidemic in the word: cases and deaths by country"),

    sidebarLayout(
        #...Sidebar with a dropdown list and an action button
        sidebarPanel(
            
            selectizeInput(inputId = "selectCountry",
                           label = "Select the country of interest",
                           choices = countries,options = list(
                               placeholder = "Please select a Country below", 
                               onInitialize = I('function() { this.setValue(""); }')
                               )),
            
            actionButton(inputId = "submit", label = "Submit")
        ),

        #...Main panel with instructions, graphs of cases and deaths and dowload buttons
        mainPanel(
            h4("This application allows you to follow the daily trend of Coronavirus 
               cases and deaths by country in the word", ),
            p(tags$b(tags$em("Data source: ")),
              tags$em("Max Roser, Hannah Ritchie, Esteban Ortiz-Ospina and Joe Hasell"),
              " (2020) - 'Coronavirus Pandemic (COVID-19)'. Published online at 
              OurWorldInData.org. Retrieved from:",
              tags$a(href="https://ourworldindata.org/coronavirus",
                     "https://ourworldindata.org/coronavirus")),
            p(),
            p(tags$b(tags$em("Select the country in which you are interested")),
              "in the panel on the left: interactive graphs will display the trend 
              of Coronavirus cases and deaths by day in that country."),
            p("At the ",tags$b(tags$em("bottom of the application"))," you will be able to ",
              tags$b(tags$em("download two .csv files")),
              ": one for the whole dataset and another one with the dataset 
              related only to the country in which you are interested. The two 
              datasets will be named respectively 'CoronavirusDB",tags$em("YYYY-MM-DD"),
              ".csv' and '",tags$em("Country of interest"),"DB",tags$em("YYYY-MM-DD"),".csv'"),
            h3(textOutput(outputId = "titleGraphCases")),
            plotlyOutput("plotCases"),
            h3(textOutput(outputId = "titleGraphDeaths")),
            plotlyOutput("plotDeaths"),
            downloadButton(outputId = "downloadDB", label = "Download whole dataset"),
            downloadButton(outputId = "downloadCountryDB", label = paste0("Donwload selected country dataset"))
        )
    )
)

#...Define server logic required to draw the graphs and dowloads the datasets
server <- function(input, output) {
    
    #...Filter the dataset for the Country of Interest
    country <- reactive({
        coronavirus %>% dplyr::filter(Country == input$selectCountry)
        })
    
    #...Prepare the Title of Cases according to the Country of Interest
    observeEvent(eventExpr = input$submit, {
        output$titleGraphCases <- renderText({
            paste0("Number of Coronavirus cases by day in ", input$selectCountry)
        })
    })
    
    #...Prepare the Graphs of Cases according to the Country of Interest
    observeEvent(eventExpr = input$submit, {
        output$plotCases <- renderPlotly({
            print(
                ggplotly(
                    ggplot(data = country(), mapping = aes(x = Date, y = Cases)) +
                        geom_line() +
                        geom_smooth() +
                        labs(x = "Date", y = "Nº of cases per day")
                )
            )
        })
        
    })
    
    #...Prepare the Title of Deaths according to the Country of Interest
    observeEvent(eventExpr = input$submit, {
        output$titleGraphDeaths <- renderText({
            paste0("Number of Coronavirus deaths by day in ", input$selectCountry)
        })
    })
    
    #...Prepare the Graph of Deaths according to the Country of Interest
    observeEvent(eventExpr = input$submit, {
        output$plotDeaths <- renderPlotly({
            print(
                ggplotly(
                    ggplot(data = country(), mapping = aes(x = Date, y = Deaths)) +
                        geom_line() +
                        geom_smooth() +
                        labs(x = "Date", y = "Nº of deaths per day")
                )
            )
        })
                
    })
    
    #...Download the Whole Dataset into a .csv file
    observeEvent(eventExpr = input$submit, {
        output$downloadDB <- downloadHandler(
            filename <- function() {
                paste0("CoronavirusDB",Sys.Date(),".csv", sep="")
            },
            content <- function(file) {
                write.csv(x = coronavirus, file = file, row.names = FALSE)
            } 
        )
    })
    
    #...Download the Dataset related to the Country of Interest into a .csv file
    observeEvent(eventExpr = input$submit, {
        output$downloadCountryDB <- downloadHandler(
            filename <- function() {
                paste0(input$selectCountry,"DB",Sys.Date(),".csv", sep="")
            },
            content <- function(file) {
                write.csv(x = country(), file = file, row.names = FALSE)
            } 
        )
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
