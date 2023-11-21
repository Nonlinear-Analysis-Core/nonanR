
# To have this be able to run as a function we need to have the app directory folder inside the package folder
# Search for the folder in the package folder
# Combine the package directory with the app name specified -- the actual folder name that the app file is in
# Call runapp on that name

library(shiny)
library(shinythemes)
library(fractalRegression)
library(tidyverse)
library(plotly)
library(NONANr)

# Create some mock data to use in the app
left_dat = data.frame("stride_number" = 1:1000, "stride_time_left" = fgn_sim(1000, 0.75))
right_dat = data.frame("stride_number" = 1:1000, "stride_time_right" = fgn_sim(1000, 0.8))


myDataFrames <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
loadedData <- data()$results[,3]


library(shiny)

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
                
                # Application title
                titlePanel("NONAN App"),
                navbarPage("{NONANr}", 
                           tabPanel("About", 
                                    includeMarkdown("About_NONAN.Rmd")),
                           navbarMenu("Fractal Methods",

# DFA ---------------------------------------------------------------------
                                      
                                      tabPanel("DFA", 
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoice", "select Data", choices = list("Your Data" = c(myDataFrames), "R Datasets" = c(loadedData), selected = NULL)),
                                                   selectInput("dfax", "Select X axis", choices = NULL),
                                                   selectInput("dfay", "Select Y axis", choices = NULL),
                                                   numericInput("order", "Order:", value = 1),
                                                   numericInput("minScale", "Min Scale:", value = 4),
                                                   numericInput("maxScale", "Max Scale:", value = 10),
                                                   numericInput("scaleRatio", "Scale Ratio:", value = 2),
                                                   actionButton("goDFA", "Go!"),
                                                   p("Click the button to start the analysis.")
                                                   
                                                 ), # sidebarpanel
                                                 mainPanel(
                                                   fluidRow( 
                                                     column(12,  plotlyOutput('dfaTS')), # single row just for the time series plot
                                                   ), 
                                                   br(),
                                                   br(),
                                                   fluidRow( 
                                                     column(4,  plotOutput('dfaPlot')), 
                                                     column(4,  plotOutput('histogram')),
                                                     column(4,  plotOutput('autocorr'))
                                                   ), 
                                                   verbatimTextOutput("dfaResults"), 
                                                   br(),
                                                   br(),
                                                   # tableOutput("datHead") # This was largely for debugging
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ), # DFA tabpanel
                           ), # navbarPage
                           navbarMenu("Entropy",

# Sample Entropy ----------------------------------------------------------

                           tabPanel("Sample Entropy",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("dataChoice1", "select Data", choices = list("Your Data" = c(myDataFrames), 
                                                                                                 "R Datasets" = c(loadedData), 
                                                                                                 selected = NULL)),
                                        selectInput("SEx", "Select X axis", choices = NULL),
                                        selectInput("SEy", "Select Y axis", choices = NULL),
                                        numericInput("SEm", "m value:", value = 2),
                                        numericInput("SEr", "r value:", value = 0.2),
                                        actionButton("goSEENT", "Go!"),
                                        p("Click the button to start the analysis.")
                                        
                                      ), # sidebarpanel
                                      
                                      mainPanel(fluidRow( 
                                        column(12,  plotlyOutput('SEts')), # single row just for the time series plot
                                      ), 
                                      br(),
                                      br(),
                                      fluidRow( 
                                        column(6,  plotOutput('SEhist')),
                                        column(6,  plotOutput('SEacf'))
                                      ), 
                                      verbatimTextOutput("SEresults"), 
                                      br(),
                                      br(),
                                      # tableOutput("SEdatHead") # This was largely for debugging
                                      ) # mainpanel
                                    ) #sidebarlayout
                           ), # sample entropy tabpanel

# Approximate Entropy -----------------------------------------------------

                           tabPanel("Approximate Entropy",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("dataChoice2", "select Data", choices = list("Your Data" = c(myDataFrames), 
                                                                                                 "R Datasets" = c(loadedData), 
                                                                                                 selected = NULL)),
                                        selectInput("AEx", "Select X axis", choices = NULL),
                                        selectInput("AEy", "Select Y axis", choices = NULL),
                                        numericInput("AEdim", "dim value:", value = 8),
                                        numericInput("AEr", "r value:", value = 0.2),
                                        actionButton("goAEENT", "Go!"),
                                        p("Click the button to start the analysis.")
                                        
                                      ), # sidebarpanel
                                      
                                      mainPanel(fluidRow( 
                                        column(12,  plotlyOutput('AEts')), # single row just for the time series plot
                                      ), 
                                      br(),
                                      br(),
                                      fluidRow( 
                                        column(6,  plotOutput('AEhist')),
                                        column(6,  plotOutput('AEacf'))
                                      ), 
                                      verbatimTextOutput("AEresults"), 
                                      br(),
                                      br(),
                                      tableOutput("AEdatHead") # This was largely for debugging
                                      ) # mainpanel
                                    ) #sidebarlayout
                           ) # entropy tabpanel
                           ) # navbarMenu
                ) # navbar page
) # fluidpage


# Server ------------------------------------------------------------------

# Define server logic 
server <- function(input, output) {
  
  # get a list of the column names in the data frame
  n = reactive({
    names(get(input$dataChoice))
  })
  
  # DFA ---------------------------------------------------------------------
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoice, {
    updateSelectInput(inputId = "dfax", choices = n())
    updateSelectInput(inputId = "dfay", choices = n(), selected = n()[2])
  })
  
  # Select the desired data frame and by default the second column for analysis
  dfa_dat = reactive({
    get(input$dataChoice) |>
      select(all_of(input$dfay)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$dfaTS <- renderPlotly({
    
    plot_dat = get(input$dataChoice)
    plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$dfay]], type = 'scatter', mode = 'lines', 
            color = I('black')) %>% # Aesthetics for the plot
      layout(title = list(text = paste0("Time series of ", input$dfay)),
             xaxis = list(title = "data Index"),
             yaxis = list(title = paste0(input$dfay)))
    
    # ggplot(plot_dat, aes(x = .data[[input$xcol]], y = .data[[input$ycol]])) + 
    #   geom_line() + theme_classic() +
    #   labs(title = paste0("Time series of ", input$ycol))
  })
  
  # Set the scales for the DFA function
  scales = reactive({
    logscale(input$minScale, input$maxScale, input$scaleRatio) 
  })
  
  # DFA calculation
  dfaResult <- eventReactive(input$goDFA, {
    dfa(dfa_dat(), order = input$order, verbose = 1, scales = scales(), scale_ratio = input$scaleRatio)
  })
  
  # DFA plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goDFA, {
    output$dfaPlot <- renderPlot({
      dfa.plot(dfaResult())
    })
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goDFA, {
    output$histogram <- renderPlot({
      hist(dfa_dat(), main = paste("Histogram of ", input$ycol), xlab = input$ycol)
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goDFA, {
    output$autocorr <-  renderPlot({
      acf(dfa_dat(), main = paste("Autocorrelation of ", input$ycol))  
    })
  }) # observeEvent
  
  # Print out the DFA results
  output$dfaResults <- renderPrint({
    dfaResult()
    #input$ycol
  })
  
  # Print the data so we can see what column is actually being selected. For debugging only
  # output$datHead <- renderTable({
  #   head(get(input$dataChoice))
  # })
  
  
  # Sample Entropy -----------------------------------------------------------------
  
  # get a list of the column names in the data frame
  n1 = reactive({
    names(get(input$dataChoice1))
  })
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoice1, {
    updateSelectInput(inputId = "SEx", choices = n1())
    updateSelectInput(inputId = "SEy", choices = n1(), selected = n1()[2])
  })
  
  
  # Select the desired data frame and by default the second column for analysis
  SE_dat = reactive({
    get(input$dataChoice1) |>
      select(all_of(input$SEy)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$SEts <- renderPlotly({
    
    plot_dat = get(input$dataChoice1)
    plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$SEy]], type = 'scatter', mode = 'lines', 
            color = I('black')) %>% # Aesthetics for the plot
      layout(title = list(text = paste0("Time series of ", input$SEy)),
             xaxis = list(title = "data Index"),
             yaxis = list(title = paste0(input$SEy)))
  })
  
  # Entropy calculation
  SEresult <- eventReactive(input$goSEENT, {
    SampleEntropy(SE_dat(), m = input$SEm, R = input$SEr)
  })
  
  # Print out the DFA results
  output$SEresults <- renderPrint({
    SEresult()
  })
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goSEENT, {
    output$SEhist <- renderPlot({
      hist(SE_dat(), main = paste("Histogram of ", input$SEy), xlab = input$SEy)
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goSEENT, {
    output$SEacf <-  renderPlot({
      acf(SE_dat(), main = paste("Autocorrelation of ", input$ycol))  
    })
  }) # observeEvent

  # Print the data so we can see what column is actually being selected. For debugging only
  # output$SEdatHead <- renderTable({
  #   head(get(input$dataChoice1))
  # })
  
  # Approximate Entropy -----------------------------------------------------------------
  
  # get a list of the column names in the data frame
  n2 = reactive({
    names(get(input$dataChoice2))
  })
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoice2, {
    updateSelectInput(inputId = "AEx", choices = n2())
    updateSelectInput(inputId = "AEy", choices = n2(), selected = n2()[2])
  })
  
  
  # Select the desired data frame and by default the second column for analysis
  AE_dat = reactive({
    get(input$dataChoice2) |>
      select(all_of(input$AEy)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$AEts <- renderPlotly({
    
    plot_dat = get(input$dataChoice2)
    plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$AEy]], type = 'scatter', mode = 'lines', 
            color = I('black')) %>% # Aesthetics for the plot
      layout(title = list(text = paste0("Time series of ", input$AEy)),
             xaxis = list(title = "data Index"),
             yaxis = list(title = paste0(input$AEy)))
  })
  
  # Entropy calculation
  AEresult <- eventReactive(input$goAEENT, {
    ApproximateEntropy(AE_dat(), dim = input$AEdim, R = input$AEr)
  })
  
  # Print out the DFA results
  output$AEresults <- renderPrint({
    AEresult()
  })
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goAEENT, {
    output$AEhist <- renderPlot({
      hist(AE_dat(), main = paste("Histogram of ", input$AEy), xlab = input$AEy)
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goAEENT, {
    output$AEacf <-  renderPlot({
      acf(AE_dat(), main = paste("Autocorrelation of ", input$ycol))  
    })
  }) # observeEvent
  
  # Print the data so we can see what column is actually being selected. For debugging only
  # output$SEdatHead <- renderTable({
  #   head(get(input$dataChoice1))
  # })
  
} # server

# Run the application 
shinyApp(ui = ui, server = server)
