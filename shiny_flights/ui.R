shinyUI(
  pageWithSidebar(
    headerPanel("JetBlue Delayed Flights"),
    
    sidebarPanel(
      selectInput('airport', label = "Select airport code", choices = sort(unique(flights.df$Origin)),selected = 'SFO', multiple = FALSE),
      numericInput('lsl', 'Select lower limit - minutes delayed :', 0),
      numericInput('usl', 'Select upper limit - minutes delayed:', 350)
      ),
    mainPanel(
      tabsetPanel(
        tabPanel('Histogram', plotOutput('hist')),
        tabPanel('Boxplot', plotOutput('boxplot')),
        tabPanel('Statistics', verbatimTextOutput('stats')),
        tabPanel('X-bar', plotOutput('xbar')),
        tabPanel('Capability', plotOutput('pc')),
        tabPanel('Data', verbatimTextOutput('data'))
        )
      )
    ))
