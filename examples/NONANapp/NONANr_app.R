
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
                                        tableOutput("datHead")
                                      ) # mainpanel
                                    ) # sidebarlayout
                           ), # DFA tabpanel
                           tabPanel("Entropy",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("dataChoice1", "select Data", choices = list("Your Data" = c(myDataFrames), 
                                                                                                "R Datasets" = c(loadedData), 
                                                                                                selected = NULL)),
                                        selectInput("entx", "Select X axis", choices = NULL),
                                        selectInput("enty", "Select Y axis", choices = NULL),
                                        numericInput("m", "m value:", value = 2),
                                        numericInput("r", "r value:", value = 0.2),
                                        actionButton("goENT", "Go!"),
                                        p("Click the button to start the analysis.")
                                        
                                      ), # sidebarpanel
                                      
                                      mainPanel(fluidRow( 
                                        column(12,  plotlyOutput('entropyTS')), # single row just for the time series plot
                                      ), 
                                      br(),
                                      br(),
                                      fluidRow( 
                                        column(6,  plotOutput('entropyHist')),
                                        column(6,  plotOutput('entropyACF'))
                                      ), 
                                      verbatimTextOutput("entropyResults"), 
                                      br(),
                                      br(),
                                      tableOutput("entdatHead")
                                      ) # mainpanel
                                    ) #sidebarlayout
                           ) # entropy tabpanel
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
  
  output$datHead <- renderTable({
    head(get(input$dataChoice))
    #head(dat())
  })
  
  
  # Entropy -----------------------------------------------------------------
  
  # get a list of the column names in the data frame
  n1 = reactive({
    names(get(input$dataChoice1))
  })
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoice1, {
    updateSelectInput(inputId = "entx", choices = n1())
    updateSelectInput(inputId = "enty", choices = n1(), selected = n1()[2])
  })
  
  
  # Select the desired data frame and by default the second column for analysis
  ent_dat = reactive({
    get(input$dataChoice1) |>
      select(all_of(input$enty)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$entropyTS <- renderPlotly({
    
    plot_dat = get(input$dataChoice1)
    plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$enty]], type = 'scatter', mode = 'lines', 
            color = I('black')) %>% # Aesthetics for the plot
      layout(title = list(text = paste0("Time series of ", input$enty)),
             xaxis = list(title = "data Index"),
             yaxis = list(title = paste0(input$enty)))
  })
  
  # Entropy calculation
  entResult <- eventReactive(input$goENT, {
    SampleEntropy(ent_dat(), m = input$m, R = input$r)
  })
  
  # Print out the DFA results
  output$entropyResults <- renderPrint({
    entResult()
  })
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goENT, {
    output$entropyHist <- renderPlot({
      hist(ent_dat(), main = paste("Histogram of ", input$enty), xlab = input$enty)
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goENT, {
    output$entropyACF <-  renderPlot({
      acf(ent_dat(), main = paste("Autocorrelation of ", input$ycol))  
    })
  }) # observeEvent
  
  output$entdatHead <- renderTable({
    head(get(input$dataChoice1))
    #head(dat())
  })
  
} # server

# Run the application 
shinyApp(ui = ui, server = server)
