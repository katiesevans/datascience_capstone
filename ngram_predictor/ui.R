library(shinythemes)
library(shiny)

# Define UI for application
shinyUI(fluidPage(theme = shinytheme("yeti"),
  
  # Application title
  titlePanel("N-gram Predictor"),
  
  tabsetPanel(
      tabPanel("App",
               # Sidebar with place to input text 
               sidebarLayout(
                   sidebarPanel(
                       textInput("input", "Text:"),
                       sliderInput("numwords", "Number of word choices:", min = 1, max = 20, value = 5),
                       actionButton('go', 'Predict')
                   ),
                   
                   # Finish the sentence with 5 options
                   mainPanel(
                       tags$h4(htmlOutput("user_input")),
                       uiOutput("model_output"),
                       splitLayout(uiOutput("text_buttons"))
                       # uiOutput("text_buttons")
                   )
               )
               ),
      tabPanel("Help",
               mainPanel(
                   tags$h3("To use this application..."),
                   uiOutput("help_text")
               )
      ),
      tabPanel("About",
               mainPanel(
                   tags$h3("About the application:"),
                   uiOutput("about_text"),
                   tags$h3("About the author:"),
                   textOutput("author_text"),
                   tagList(a("Contact Katie.", href = "mailto:kathryn.evans@u.northwestern.edu"))
               )
      )
  )
  
  
))
