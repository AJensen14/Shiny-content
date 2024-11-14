####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang,
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny,
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages####
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(DT)
library(reshape2)
library(stringr)
# Define UI####
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(theme = "yeti",
                           title = "Shiny app learning",
                           tabPanel("1: Name paste", # Name pasting
                                    "This panel is a name pasting thing",
                                    sidebarPanel(tags$h3("Input:"),
                                                 textInput("txt1", "Given Name:", ""),
                                                 textInput("txt2", "Surname:", "")), # sidebarPanel
                                    mainPanel(h1("The header"),
                                              h4("The output label"),# 1, 2, 3, 4 just changes the size of the heading similar to markdown
                                              verbatimTextOutput("txtout"))),
                           tabPanel("2: Additions",# Addition calc
                                    "This panel is used to do additions",
                                    sidebarPanel(tags$h3("Input:"),
                                                 textInput("txt3", "Number 1", ""),
                                                 textInput("txt4", "Number 2", "")),
                                    mainPanel(h1("Calculator"),
                                              h4("Addition"),
                                              verbatimTextOutput("answer"))),
                           tabPanel("3: Editing a plot", # makes a plot where you can change colour and size of point
                                    "This is for editing a graph",
                                    sidebarPanel(textInput("colour",
                                                           "Colour of points",
                                                           placeholder = "red"),
                                                 numericInput("pointsize", "Size of points",value = 2, min = 0, max = 50),
                                                 textInput("xlabel", "X axis title", placeholder = "x axis"),
                                                 textInput('ylabel', "Y axis title", placeholder = "y axis")),
                                    mainPanel(plotOutput("plot1")))))
# Define server function ####
server <- function(input, output) {
  output$txtout <- renderText({paste(input$txt1, input$txt2, sep = " " )})
  output$answer <- renderText({paste(as.numeric(input$txt3)+as.numeric(input$txt4))})
  output$plot1 <- renderPlot({

    color <- ifelse(input$colour == "", "red", input$colour)

    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
      geom_point(col = color, size = input$pointsize)+
      labs(x = input$xlabel, y = input$ylabel)+
      theme_classic()

  })
}
# Create Shiny object ####
shinyApp(ui = ui, server = server)

#########################################################################
# So at this stage the app is perfectly working
# now need to think of ideas for more complex shiny app functions

# Want 5 more ideas
# data generator with a download button # finished

# insert that data into a panel and it gives you descriptive statistics and a histogram# finished

# Word detailer (enter a word and it breaks it down) # 1) Is it a word check? 2) Sentence breaking down wordsL:
# (number of letters, number of vowels, number of unique letters)
# 3) The description of the word from the dictionary
#

# histogram specificed for each different species in iris dataset (options on a click down)

# Generate spelling bee map with an update button
####################################################################################
options(shiny.maxRequestSize=30*1024^2)
# Define UI####
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(theme = "yeti",
                           title = "Shiny app learning",
                           tabPanel("1: Name paste", # Name pasting
                                    "This panel is a name pasting thing",
                                    sidebarPanel(tags$h3("Input:"),
                                                 textInput("txt1", "Given Name:", ""),
                                                 textInput("txt2", "Surname:", "")), # sidebarPanel
                                    mainPanel(h1("The header"),
                                              h4("The output label"),# 1, 2, 3, 4 just changes the size of the heading similar to markdown
                                              verbatimTextOutput("txtout"))),
                           tabPanel("2: Additions",# Addition calc
                                    "This panel is used to do additions",
                                    sidebarPanel(
                                      tags$h3("Input:"),
                                      textInput("txt3", "Number 1", ""),
                                      textInput("txt4", "Number 2", "")),
                                    mainPanel(h1("Calculator"),
                                              h4("Addition"),
                                              verbatimTextOutput("answer"))),
                           tabPanel("3: Editing a plot", # makes a plot where you can change colour and size of point
                                    "This is for editing a graph",
                                    sidebarPanel(
                                      textInput("colour",
                                                "Colour of points",
                                                placeholder = "red"),
                                      numericInput("pointsize", "Size of points",value = 2, min = 0, max = 50),
                                      textInput("xlabel", "X axis title", placeholder = "x axis"),
                                      textInput('ylabel', "Y axis title", placeholder = "y axis")),
                                    mainPanel(
                                      plotOutput("plot1"))),
                           tabPanel("4: Generate sample data",
                                    "Generate some sample data",
                                    sidebarPanel(
                                      numericInput(inputId = "numberofdata",
                                                   label = "Data points",
                                                   value = 500,
                                                   min = 200,
                                                   max = 5000),
                                                 textInput(inputId = "datafilename",
                                                           label = "File Name")),
                                    mainPanel(
                                      fluidRow(
                                        column(3, tableOutput("headtable")),
                                        column(9, plotOutput("histogram"))),
                                      downloadButton("downloaddata", "Download CSV"))),
                           tabPanel("5: Upload data",
                                    "Use this tab to upload data from 4",
                                    sidebarPanel(
                                      fileInput("fileupload", "Choose CSV file", accept = ".csv"),
                                      tags$hr(),
                                      checkboxInput("header", "Header", TRUE)),
                                    mainPanel(
                                      fluidRow(
                                        column(3, DTOutput("descstats")),
                                        column(9, plotOutput("letterplot"))))),
                           tabPanel("6: Word Breakdown", "This panel will see give you information about any word",
                                    sidebarPanel(
                                      fileInput("dictionary_upload", "Dictionary", accept = ".csv"),
                                      tags$hr(),
                                      checkboxInput("header2", "Header", TRUE),
                                      textInput(inputId = "wordinput", label = "Chosen word")),
                                    mainPanel(
                                      h2("Is it in the dictionary?"),
                                      verbatimTextOutput(outputId = "isitword"),
                                      h2("Word diagnostics"),
                                      verbatimTextOutput(outputId = "wordinfo"),
                                      h2("Description of word"),
                                      verbatimTextOutput(outputId = "description")))))
# Define server function ####
server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  output$txtout <- renderText({paste(input$txt1, input$txt2, sep = " " )})# Text output for name
  output$answer <- renderText({paste(as.numeric(input$txt3)+as.numeric(input$txt4))})# addition output
  output$plot1 <- renderPlot({

    color <- ifelse(input$colour == "", "red", input$colour)

    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
      geom_point(col = color, size = input$pointsize)+
      labs(x = input$xlabel, y = input$ylabel)+
      theme_classic()

  })# first plot output (data doesnt change)
  df <- reactive({
    data.frame(Value = rnorm(n = input$numberofdata, mean = 1000, sd = 200),
                   Letter = sample(x = LETTERS, size = input$numberofdata, replace = T))
  })# Make dataframe to download (Changes)
  output$headtable <- renderTable({

    head(df())

  })# make table from dataframe
  output$downloaddata <- downloadHandler(# download button
    filename = function(){
      paste0(input$datafilename, ".csv")
    },
    content = function(file){
      write.csv(df(), file, row.names = F)
    })
  output$histogram <- renderPlot({

    ggplot(data = df(), aes(x = Value))+
      geom_density(fill = "lightblue")+
      theme_classic()+
      labs(x = "Value", y = "Count")

  })# histogram
  uploaded_data <- reactive({
    req(input$fileupload)
    read.csv(input$fileupload$datapath, header = input$header)
  })
  summary_stats <- reactive({
    req(uploaded_data())
    uploaded_data() %>%
      summarise(mean = mean(Value),
                median = median(Value),
                sum = sum(Value),
                max = max(Value),
                min = min(Value),
                sd = sd(Value)) %>%
      melt() %>%
      rename(Value = value,
             Statistic = variable) %>%
      mutate(Value = round(Value, digits = 2))
  })
  output$descstats <-  renderDT({summary_stats()}, options = list(pageLength = 5, autoWidth = TRUE))
  output$letterplot <- renderPlot({

    uploaded_data() %>%
      ggplot(aes(x = Letter, y = Value))+
      geom_bar(stat = "summary", fun = mean)+
      theme_classic()
  })
  dictionary <- reactive({
    req(input$dictionary_upload)
    read.csv(input$dictionary_upload$datapath, header = input$header2)
  })
  output$isitword <- renderText({
    req(dictionary())
    if (toupper(input$wordinput) %in% dictionary()$Word){
      paste0(input$wordinput, " is a word in the dictionary")
    } else if (!toupper(input$wordinput) %in% dictionary()$Word){
      paste0(input$wordinput, " is not in the dictionary")
    }
  })
  output$wordinfo <- renderText({
    req(dictionary())
    numberchar <- as.character(length(unlist(strsplit(toupper(input$wordinput), ""))))
    unique <- as.character(length(unique(unlist(strsplit(toupper(input$wordinput), "")))))

    count_vowels <- function(string) {
      str_count(string, "[aeiouAEIOU]")
    }

    vowells <- as.character(count_vowels(input$wordinput))

    paste0(input$wordinput, " is ", numberchar, " letters long. Contains ", unique, " unique letters. And has ", vowells, " vowels.")


  })
  output$description <- renderText({
    req(dictionary())
    word_of_interest <- dictionary() %>%
      filter(Word == toupper(input$wordinput))

    descrip <- unlist(strsplit(as.character(word_of_interest[1, 2]), split = " \\["))[1]
    paste0(descrip)

  })
}
# Create Shiny object ####
shinyApp(ui = ui, server = server)
