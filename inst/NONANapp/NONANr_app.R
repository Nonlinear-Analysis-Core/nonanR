

# library(shiny)
# library(shinythemes)
# library(tidyverse)
# library(plotly)

# Create some mock data to use in the app
#left_dat = data.frame("stride_number" = 1:1000, "stride_time_left" = fgn_sim(1000, 0.75))
#right_dat = data.frame("stride_number" = 1:1000, "stride_time_right" = fgn_sim(1000, 0.8))


# myDataFrames <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
# loadedData <- data()$results[,3]

# Attempt to retrieve data frame names in the global environment
tryCatch({
  myDataFrames <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))
  
  if (length(myDataFrames) == 0) {
    stop("No data frame in global environment. Please load in data.")
  } else {
    # If there are data frames, continue with your code
    #loadedData <- data()$results[, 3]
    print("Using loaded data")
  }
}, error = function(e) {
  # If an error occurs (i.e., no data frames found), return custom error message
  message("No data frame in global environment. Please load in data.")
})


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
                
                # Application title
                titlePanel(title=div(img(src="AppHeader.png", height = 70), 
                                     ""), 
                           windowTitle = ""),
                # This changes the colour and padding of the titlepanel block
                tags$style(HTML("
                             h2{
                             background: black;
                             color: white;
                             margin-top: 0px;
                             margin-bottom: 0px
                             }")),
                # This adds some padding to the left side of the image in the title panel block
                tags$style(HTML(
                  "img {
                           padding-left: 10px}")),
                
                navbarPage("{nonanR}", 
                           tabPanel("About", 
                                    includeMarkdown("About_NONAN.Rmd")),
                           
                           # Fractal Methods --------------------------------------------------------
                           # Create a fractal methods tab group
                           
                           navbarMenu("Fractal Methods",
                                      
                                      ## DFA ---------------------------------------------------------------------
                                      
                                      tabPanel("DFA", 
                                               h4(strong("DFA")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoice", "Select Data:", choices = list("Your Data" = c(myDataFrames) , selected = NULL)),
                                                   selectInput("dfax", "Select X axis:", choices = NULL),
                                                   selectInput("dfay", "Select Y axis:", choices = NULL),
                                                   numericInput("order", "Order:", value = 1),
                                                   numericInput("minScale", "Min Scale:", value = 4),
                                                   numericInput("maxScale", "Max Scale:", value = 256),
                                                   numericInput("scaleRatio", "Scale Ratio:", value = 2, step = 0.1),
                                                   fluidRow(
                                                     
                                                     column(width = 6,
                                                            actionButton("goDFA", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportDFA", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   
                                                   textInput("exportDFAname", "Choose a name for your variable before exporting", "dfa_out"),
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
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("dfaResults"), 
                                                   br(),
                                                   br(),
                                                   #tableOutput("datHead") # This was largely for debugging
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ), # DFA tabpanel
                                      
                                      ## MFDFA ---------------------------------------------------------------------
                                      
                                      tabPanel("MFDFA", 
                                               h4(strong("MFDFA")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoiceMFDFA", "Select Data", choices = list("Your Data" = c(myDataFrames) , selected = NULL)),
                                                   selectInput("mfdfax", "Select X axis:", choices = NULL),
                                                   selectInput("mfdfay", "Select Y axis:", choices = NULL),
                                                   numericInput("q", "Select a q order:", value = 3, step = 1), 
                                                   numericInput("order_mfdfa", "Order:", value = 1),
                                                   numericInput("minScale_mfdfa", "Min Scale:", value = 4),
                                                   numericInput("maxScale_mfdfa", "Max Scale:", value = 256),
                                                   numericInput("scaleRatio_mfdfa", "Scale Ratio:", value = 2, step = 0.1),
                                                   fluidRow(
                                                     
                                                     column(width = 6,
                                                            actionButton("goMFDFA", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportMFDFA", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportMFDFAname", "Choose a name for your variable before exporting", "mfdfa_out"),
                                                   
                                                 ), # sidebarpanel
                                                 mainPanel(
                                                   fluidRow( 
                                                     column(12,  plotlyOutput('mfdfaTS')), # single row just for the time series plot
                                                   ), 
                                                   br(),
                                                   br(),
                                                   fluidRow( 
                                                     column(2),
                                                     column(8, plotOutput('mfdfaPlot')), 
                                                     column(2)
                                                   ), 
                                                   br(),
                                                   br(),
                                                   fluidRow( 
                                                     column(6,  plotOutput('histogram_mfdfa')),
                                                     column(6,  plotOutput('autocorr_mfdfa'))
                                                   ),
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("mfdfaResults"), 
                                                   br(),
                                                   br(),
                                                   #tableOutput("datHead") # This was largely for debugging
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ), # MFDFA tabpanel
                                      ## bayesH ---------------------------------------------------------------------
                                      
                                      tabPanel("bayesH", 
                                               h4(strong("bayesH")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoicebayesH", "Select Data", choices = list("Your Data" = c(myDataFrames) , selected = NULL)),
                                                   selectInput("bayesHx", "Select X axis:", choices = NULL),
                                                   selectInput("bayesHy", "Select Y axis:", choices = NULL),
                                                   numericInput("bayesN", "Posterior Simulations:", value = 50, step = 1, min = 1), 
                                                   fluidRow(
                                                     column(width = 6,
                                                            actionButton("gobayesH", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportbayesH", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportbayesHname", "Choose a name for your variable before exporting", "bayesH_out"),
                                                   
                                                 ), # sidebarpanel
                                                 mainPanel(
                                                   fluidRow( 
                                                     column(12,  plotlyOutput('bayesHTS')), # single row just for the time series plot
                                                   ), 
                                                   br(),
                                                   br(),
                                                   fluidRow( 
                                                     column(6,  plotOutput('histogram_bayesH')),
                                                     column(6,  plotOutput('autocorr_bayesH'))
                                                   ),
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("bayesHResults"), 
                                                   br(),
                                                   br(),
                                                   #tableOutput("datHead") # This was largely for debugging
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ),
                           ), # navbarPage
                           
                           
                           # Entropy -----------------------------------------------------------------
                           # Create an entropy tab group
                           navbarMenu("Entropy",
                                      
                                      ## Sample Entropy ----------------------------------------------------------
                                      
                                      tabPanel("Sample Entropy",
                                               h4(strong("Sample Entropy")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoice1", "select Data", choices = list("Your Data" = c(myDataFrames) , 
                                                                                                            selected = NULL)),
                                                   selectInput("SEx", "Select X axis", choices = NULL),
                                                   selectInput("SEy", "Select Y axis", choices = NULL),
                                                   numericInput("SEm", "m value:", value = 2),
                                                   numericInput("SEr", "r value:", value = 0.2, step = 0.1),
                                                   fluidRow(
                                                     
                                                     column(width = 6,
                                                            actionButton("goSEENT", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportSEENT", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportSEENTname", "Choose a name for your variable before exporting", "sampEnt_out"),
                                                   
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
                                                 br(),
                                                 br(),
                                                 verbatimTextOutput("SEresults"), 
                                                 br(),
                                                 br(),
                                                 # tableOutput("SEdatHead") # This was largely for debugging
                                                 ) # mainpanel
                                               ) #sidebarlayout
                                      ), # sample entropy tabpanel
                                      
                                      ## Approximate Entropy -----------------------------------------------------
                                      
                                      tabPanel("Approximate Entropy",
                                               h4(strong("Approximate Entropy")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoice2", "select Data", choices = list("Your Data" = c(myDataFrames), 
                                                                                                            selected = NULL)),
                                                   selectInput("AEx", "Select X axis", choices = NULL),
                                                   selectInput("AEy", "Select Y axis", choices = NULL),
                                                   numericInput("AEdim", "dim value:", value = 8),
                                                   numericInput("AEr", "r value:", value = 0.2, step = 0.1),
                                                   fluidRow(
                                                     
                                                     column(width = 6,
                                                            actionButton("goAENT", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportAENT", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportAENTname", "Choose a name for your variable before exporting", "apEnt_out"),
                                                   
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
                                                 br(),
                                                 br(),
                                                 verbatimTextOutput("AEresults"), 
                                                 br(),
                                                 br(),
                                                 #tableOutput("AEdatHead") # This was largely for debugging
                                                 ) # mainpanel
                                               ) #sidebarlayout
                                      ), # approximate entropy tabpanel
                                      
                                      ## Symbolic Entropy --------------------------------------------------------
                                      tabPanel("Symbolic Entropy",
                                               h4(strong("Symbolic Entropy")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoice3", "select Data", choices = list("Your Data" = c(myDataFrames), 
                                                                                                            selected = NULL)),
                                                   selectInput("SymEx", "Select X axis", choices = NULL),
                                                   selectInput("SymEy", "Select Y axis", choices = NULL),
                                                   numericInput("SymEthresh", "Threshold value:", value = NULL),
                                                   numericInput("SymEseql", "Sequence length:", value = 2),
                                                   fluidRow(
                                                     
                                                     column(width = 6,
                                                            actionButton("goSymENT", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportSymENT", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportSymENTname", "Choose a name for your variable before exporting", "symEnt_out"),
                                                   
                                                 ), # sidebarpanel
                                                 
                                                 mainPanel(fluidRow( 
                                                   column(12,  plotlyOutput('SymEts')), # single row just for the time series plot
                                                 ), 
                                                 br(),
                                                 br(),
                                                 fluidRow( 
                                                   column(6,  plotOutput('SymEhist')),
                                                   column(6,  plotOutput('SymEacf'))
                                                 ), 
                                                 br(),
                                                 br(),
                                                 verbatimTextOutput("SymEresults"), 
                                                 br(),
                                                 br(),
                                                 # tableOutput("SymEdatHead") # This was largely for debugging
                                                 ) # mainpanel
                                               ) #sidebarlayout
                                      ) # symbolic entropy tabpanel
                                      
                           ), # navbarMenu
                           
                           # PSR -----------------------------------------------------------------
                           navbarMenu("Phase Space Reconstruction",
                                      
                                      ## RQA ---------------------------------------------------------------------
                                      
                                      tabPanel("RQA", 
                                               h4(strong("RQA")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoice4", "Select Data:", choices = list("Your Data" = c(myDataFrames) , selected = NULL)),
                                                   selectInput("rqax", "Select X axis:", choices = NULL),
                                                   selectInput("rqay", "Select Y axis:", choices = NULL),
                                                   numericInput("embed", "Embedding Dimension:", value = 1),
                                                   numericInput("delay", "Delay:", value = 1, min = 0),
                                                   numericInput("normalize", "Normalization:", value = 0, min = 0, max = 2),
                                                   numericInput("rescale", "Rescale:", value = 1, step = 1, min = 0, max = 2),
                                                   numericInput("mindiagline", "Min Length:", value = 2, step = 1, min = 1),
                                                   numericInput("minvertline", "Min Length:", value = 2, step = 1, min = 1),
                                                   numericInput("twin", "Theiler Window:", value = 0, step = 0.1),
                                                   numericInput("radius", "Radius:", value = 0.0001, step = 0.0001, min = 0),
                                                   # numericInput("whiteline", "Rescale:", value = 2, step = 0.1),
                                                   # numericInput("recpt", "Plot:", value = 1, step = 1, min = 0, max = 1),
                                                   # fluidRow(
                                                   #   column(width = 7, 
                                                   #          checkboxInput(inputId = "psr_rqa", 
                                                   #                        label = "Automatically perform FNN and AMI?", 
                                                   #                        value = TRUE)),
                                                   #   column(width = 5, 
                                                   #          numericInput(inputId = "lag_rqa", 
                                                   #                       label = "Choose a maximum lag.", 
                                                   #                       value = NULL))),
                                                   fluidRow(
                                                     column(width = 6,
                                                            actionButton("goRQA", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportRQA", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportRQAname", "Choose a name for your variable before exporting", "rqa_out"),
                                                   
                                                 ), # sidebarpanel
                                                 mainPanel(
                                                   fluidRow( 
                                                     column(12,  plotlyOutput('rqaTS')), # single row just for the time series plot
                                                   ), 
                                                   br(),
                                                   br(),
                                                   fluidRow( 
                                                     column(4,  plotOutput('rqaPlot')), 
                                                     column(4,  plotOutput('histogram_rqa')),
                                                     column(4,  plotOutput('autocorr_rqa'))
                                                   ), 
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("rqaResults"), 
                                                   br(),
                                                   br(),
                                                   #tableOutput("datHead") # This was largely for debugging
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ), # RQA tabpanel
                                      
                                      ## LyE ---------------------------------------------------------------------
                                      tabPanel("Lyapunov Exponent", 
                                               h4(strong("Lyapunov Exponent")),
                                               h2("This is in progress and will be included in a future release."),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoiceLYE", "Select Data", choices = list("Your Data" = c(myDataFrames) , selected = NULL)),
                                                   selectInput("lyex", "Select X axis:", choices = NULL),
                                                   selectInput("lyey", "Select Y axis:", choices = NULL),
                                                   numericInput("dim", "Embedding Dimension:", value = 3, step = 1), 
                                                   numericInput("tau", "Delay:", value = 1),
                                                   fluidRow(
                                                     
                                                     column(width = 6,
                                                            actionButton("golye", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportlye", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportLYEname", "Choose a name for your variable before exporting", "lye_out"),
                                                   
                                                 ), # sidebarpanel
                                                 mainPanel(
                                                   fluidRow( 
                                                     column(12,  plotlyOutput('lyeTS')), # single row just for the time series plot
                                                   ), 
                                                   br(),
                                                   br(),
                                                   fluidRow( 
                                                     column(2),
                                                     column(8, plotOutput('lyePlot')), 
                                                     column(2)
                                                   ), 
                                                   br(),
                                                   br(),
                                                   fluidRow( 
                                                     column(6,  plotOutput('histogram_lye')),
                                                     column(6,  plotOutput('autocorr_lye'))
                                                   ),
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("lyeResults"), 
                                                   br(),
                                                   br(),
                                                   #tableOutput("datHead") # This was largely for debugging
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ), # MFDFA tabpanel
                           ), # navbarPage
                ) # navbar page
) # fluidpage


# Server ------------------------------------------------------------------

# Define server logic 
server <- function(input, output) {
  
  # Fractal Methods ---------------------------------------------------------------------
  ## DFA ---------------------------------------------------------------------
  
  # get a list of the column names in the data frame
  n = reactive({
    names(get(input$dataChoice))
  })
  
  
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
    # plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$dfay]], type = 'scatter', mode = 'lines', 
    #         color = I('black')) %>% # Aesthetics for the plot
    #   layout(title = list(text = paste0("Time series of ", input$dfay)),
    #          xaxis = list(title = "data Index"),
    #          yaxis = list(title = paste0(input$dfay)))
    
    ggplot(plot_dat, aes(x = 1:nrow(plot_dat), y = .data[[input$dfay]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$dfay), 
           x = "Index") + 
      theme_nonan()
    
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
      plot_dfa(dfaResult())
    })
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goDFA, {
    output$histogram <- renderPlot({
      #hist(dfa_dat(), main = paste("Histogram of ", input$dfay), xlab = input$dfay)
      
      w = ceiling(nrow(dfa_dat()) * 0.03) # calculate the number of bins
      n = colnames(dfa_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(dfa_dat()), aes(x = .data[[n]])) +
        # geom_histogram( color="white", fill="black", bins = w) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of", input$dfay), 
             x = input$dfay) +
        theme_nonan()
      
      
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goDFA, {
    output$autocorr <-  renderPlot({
      
      a = acf(dfa_dat(), plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(dfa_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$dfay)) + 
        theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  
  
  # Print out the DFA results
  observeEvent(input$goDFA, {
    output$dfaResults <- renderPrint({
      cat("Log Scales:", dfaResult()$log_scales, "\n", "Log RMS:", dfaResult()$log_rms, "\n", "Alpha:", dfaResult()$alpha)
      
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportDFA, {
    assign(input$exportDFAname, dfaResult(), envir = globalenv())
    
    output$dfaResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  
  # Print the data so we can see what column is actually being selected. For debugging only
  #output$datHead <- renderTable({
  # head(dfa_dat())
  #})
  
  ## MFDFA ---------------------------------------------------------------------
  
  # get a list of the column names in the data frame
  n_mfdfa = reactive({
    names(get(input$dataChoiceMFDFA))
  })
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoiceMFDFA, {
    updateSelectInput(inputId = "mfdfax", choices = n_mfdfa())
    updateSelectInput(inputId = "mfdfay", choices = n_mfdfa(), selected = n_mfdfa()[2])
  })
  
  # Select the desired data frame and by default the second column for analysis
  mfdfa_dat = reactive({
    get(input$dataChoiceMFDFA) |>
      select(all_of(input$mfdfay)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$mfdfaTS <- renderPlotly({
    
    plot_dat = get(input$dataChoiceMFDFA)
    # plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$dfay]], type = 'scatter', mode = 'lines', 
    #         color = I('black')) %>% # Aesthetics for the plot
    #   layout(title = list(text = paste0("Time series of ", input$dfay)),
    #          xaxis = list(title = "data Index"),
    #          yaxis = list(title = paste0(input$dfay)))
    
    ggplot(plot_dat, aes(x = 1:nrow(plot_dat), y = .data[[input$mfdfay]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$mfdfay), 
           x = "Index") + 
      theme_nonan()
  })
  
  # Set the q order parameter. This will always be a symmetric range based on what the parameter is.
  q_order = reactive({
    -input$q:input$q
  })
  # Set the scales for the DFA function
  scales = reactive({
    logscale(input$minScale_mfdfa, input$maxScale_mfdfa, input$scaleRatio_mfdfa) 
  })
  
  # DFA calculation
  mfdfaResult <- eventReactive(input$goMFDFA, {
    mfdfa(mfdfa_dat(), q = q_order(), order = input$order_mfdfa, scales = scales(), scale_ratio = input$scaleRatio_mfdfa)
    
  })
  
  # DFA plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goMFDFA, {
    output$mfdfaPlot <- renderPlot({
      plot_mfdfa(mfdfaResult(), do.surrogate = T, nsurrogates = 19, return.ci = T)
    })
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goMFDFA, {
    output$histogram_mfdfa <- renderPlot({
      #hist(dfa_dat(), main = paste("Histogram of ", input$dfay), xlab = input$dfay)
      
      w = ceiling(nrow(mfdfa_dat()) * 0.03) # calculate the number of bins
      n = colnames(mfdfa_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(mfdfa_dat()), aes(x = .data[[n]])) +
        # geom_histogram( color="white", fill="black", bins = w) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of ", input$mfdfay), 
             x = input$mfdfay) +
        theme_nonan()
      
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goMFDFA, {
    output$autocorr_mfdfa <-  renderPlot({
      
      a = acf(mfdfa_dat(), plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(mfdfa_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$mfdfay)) + 
        theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  
  
  # Print out the DFA results
  observeEvent(input$goMFDFA, {
    output$mfdfaResults <- renderPrint({
      cat("Log Scales:", mfdfaResult()$log_scale, "\n", 
          "Log fq:", mfdfaResult()$log_fq, "\n", 
          "Hq:", mfdfaResult()$Hq, "\n", 
          "Tau:", mfdfaResult()$Tau, "\n", 
          "H:", mfdfaResult()$h, "\n", 
          "Dh:", mfdfaResult()$Dh)
      
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportMFDFA, {
    assign(input$exportMFDFAname, mfdfaResult(), envir = globalenv())
    
    output$mfdfaResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  
  # Print the data so we can see what column is actually being selected. For debugging only
  #output$datHead <- renderTable({
  # head(dfa_dat())
  #})
  
  ## bayesH -----------------------------------------------------------------
  
  # get a list of the column names in the data frame
  n_bayes = reactive({
    names(get(input$dataChoicebayesH))
  })
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoicebayesH, {
    updateSelectInput(inputId = "bayesHx", choices = n_bayes())
    updateSelectInput(inputId = "bayesHy", choices = n_bayes(), selected = n_bayes()[2])
  })
  
  
  # Select the desired data frame and by default the second column for analysis
  bayesH_dat = reactive({
    get(input$dataChoicebayesH) |>
      select(all_of(input$bayesHy)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$bayesHTS <- renderPlotly({
    
    plot_dat = get(input$dataChoicebayesH)
    # plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$SEy]], type = 'scatter', mode = 'lines',
    #         color = I('black')) %>% # Aesthetics for the plot
    #   layout(title = list(text = paste0("Time series of ", input$SEy)),
    #          xaxis = list(title = "data Index"),
    #          yaxis = list(title = paste0(input$SEy)))
    
    ggplot(plot_dat, aes(x = 1:nrow(plot_dat), y = .data[[input$bayesHy]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$bayesHy),
           x = "Index") +
      theme_nonan()
    
  })
  
  # Entropy calculation
  bayesHresult <- eventReactive(input$gobayesH, {
    bayesH(bayesH_dat(), n = input$bayesN)
  })
  
  # Print out the sample entropy results
  observeEvent(input$gobayesH, {
    output$bayesHResults <- renderPrint({
      cat("bayesH:", median(bayesHresult()))
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportbayesH, {
    assign(input$exportbayesHname, list("hurst_pdf" = bayesHresult(), "median_hurst" =  median(bayesHresult())), envir = globalenv())
    
    output$bayesHResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$gobayesH, {
    output$histogram_bayesH <- renderPlot({
      #hist(SE_dat(), main = paste("Histogram of ", input$SEy), xlab = input$SEy)
      
      w = ceiling(nrow(bayesH_dat()) * 0.03) # calculate the number of bins
      n = colnames(bayesH_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(bayesH_dat()), aes(x = .data[[n]])) +
        # geom_histogram( color="white", fill="black", bins = w) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of ", input$bayesHy),
             x = input$bayesHy) +
        theme_nonan()
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$gobayesH, {
    output$autocorr_bayesH <-  renderPlot({
      
      a = acf(bayesH_dat(), plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(bayesH_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = 'white', linewidth = 0.7) +
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$bayesHy)) +
        theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  
  #Print the data so we can see what column is actually being selected. For debugging only
  output$SEdatHead <- renderTable({
    head(get(input$dataChoice1))
  })
  
  # Entropy -----------------------------------------------------------------
  ## Sample Entropy -----------------------------------------------------------------
  
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
    # plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$SEy]], type = 'scatter', mode = 'lines', 
    #         color = I('black')) %>% # Aesthetics for the plot
    #   layout(title = list(text = paste0("Time series of ", input$SEy)),
    #          xaxis = list(title = "data Index"),
    #          yaxis = list(title = paste0(input$SEy)))
    
    ggplot(plot_dat, aes(x = 1:nrow(plot_dat), y = .data[[input$SEy]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$SEy), 
           x = "Index") + 
      theme_nonan()
    
  })
  
  # Entropy calculation
  SEresult <- eventReactive(input$goSEENT, {
    Ent_Samp(SE_dat(), m = input$SEm, R = input$SEr)
  })
  
  # Print out the sample entropy results
  observeEvent(input$goSEENT, {
    output$SEresults <- renderPrint({
      cat("Sample Entropy:", SEresult())
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportSEENT, {
    assign(input$exportSEENTname, SEresult(), envir = globalenv())
    
    output$SEresults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goSEENT, {
    output$SEhist <- renderPlot({
      #hist(SE_dat(), main = paste("Histogram of ", input$SEy), xlab = input$SEy)
      
      w = ceiling(nrow(SE_dat()) * 0.03) # calculate the number of bins
      n = colnames(SE_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(SE_dat()), aes(x = .data[[n]])) +
        # geom_histogram( color="white", fill="black", bins = w) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of ", input$SEy), 
             x = input$SEy) +
        theme_nonan()
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goSEENT, {
    output$SEacf <-  renderPlot({
      
      a = acf(SE_dat(), plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(SE_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$SEy)) + 
        theme_nonan() # add the nonan plot theme on    
      
    })
  }) # observeEvent
  
  # Print the data so we can see what column is actually being selected. For debugging only
  # output$SEdatHead <- renderTable({
  #   head(get(input$dataChoice1))
  # })
  
  ## Approximate Entropy -----------------------------------------------------------------
  
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
    # plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$AEy]], type = 'scatter', mode = 'lines', 
    #         color = I('black')) %>% # Aesthetics for the plot
    #   layout(title = list(text = paste0("Time series of ", input$AEy)),
    #          xaxis = list(title = "data Index"),
    #          yaxis = list(title = paste0(input$AEy)))
    
    ggplot(plot_dat, aes(x = 1:nrow(plot_dat), y = .data[[input$AEy]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$AEy), 
           x = "Index") + 
      theme_nonan()
    
    
  })
  
  # Entropy calculation
  AEresult <- eventReactive(input$goAENT, {
    Ent_Ap(AE_dat(), dim = input$AEdim, R = input$AEr)
  })
  
  # Print out the approximate entropy results
  observeEvent(input$goAENT, {
    output$AEresults <- renderPrint({
      cat("Approximate Entropy:", AEresult())
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportAENT, {
    assign(input$exportAENTname, AEresult(), envir = globalenv())
    
    output$AEresults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goAENT, {
    output$AEhist <- renderPlot({
      # hist(AE_dat(), main = paste("Histogram of ", input$AEy), xlab = input$AEy)
      
      w = ceiling(nrow(AE_dat()) * 0.03) # calculate the number of bins
      n = colnames(AE_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(AE_dat()), aes(x = .data[[n]])) +
        # geom_histogram( color="white", fill="black", bins = w) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of ", input$AEy), 
             x = input$AEy) +
        theme_nonan()
      
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goAENT, {
    output$AEacf <-  renderPlot({
      
      a = acf(AE_dat(), plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(AE_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$AEy)) + 
        theme_nonan() # add the nonan plot theme on   
      
    })
  }) # observeEvent
  
  # Print the data so we can see what column is actually being selected. For debugging only
  # output$SEdatHead <- renderTable({
  #   head(get(input$dataChoice1))
  # })
  
  
  ## Symbolic Entropy --------------------------------------------------------
  
  # get a list of the column names in the data frame
  n3 = reactive({
    names(get(input$dataChoice3))
  })
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoice3, {
    updateSelectInput(inputId = "SymEx", choices = n3())
    updateSelectInput(inputId = "SymEy", choices = n3(), selected = n3()[2])
  })
  
  observeEvent(input$SymEy, ignoreInit = TRUE, { # ignore init is crucial here so that the app actually loads
    
    mean_val <- get(input$dataChoice3) %>%
      select(all_of(input$SymEy)) %>%
      as.matrix() %>%
      mean(na.rm = TRUE) %>% 
      round(digits = 4) %>%
      as.numeric()
    
    updateNumericInput(inputId = "SymEthresh", value = mean_val)
    
  })
  
  
  # Select the desired data frame and by default the second column for analysis
  SymE_dat = reactive({
    get(input$dataChoice3) |>
      select(all_of(input$SymEy)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$SymEts <- renderPlotly({
    
    plot_dat = get(input$dataChoice3)
    # plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$SymEy]], type = 'scatter', mode = 'lines', 
    #         color = I('black')) %>% # Aesthetics for the plot
    #   layout(title = list(text = paste0("Time series of ", input$SymEy)),
    #          xaxis = list(title = "data Index"),
    #          yaxis = list(title = paste0(input$SymEy)))
    
    ggplot(plot_dat, aes(x = 1:nrow(plot_dat), y = .data[[input$SymEy]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$SymEy),
           x = "Index") + 
      theme_nonan()
  })
  
  # Entropy calculation
  SymEresult <- eventReactive(input$goSymENT, {
    Ent_Sym(SymE_dat(), thresholdVal = input$SymEthresh, seqLength = input$SymEseql)
  })
  
  # Print out the approximate entropy results
  observeEvent(input$goSymENT, {
    output$SymEresults <- renderPrint({
      cat("Symbolic Entropy:", SymEresult())
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportSymENT, {
    assign(input$exportSymENTname, SymEresult(), envir = globalenv())
    
    output$SymEresults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goSymENT, {
    output$SymEhist <- renderPlot({
      # hist(SymE_dat(), main = paste("Histogram of ", input$SymEy), xlab = input$SymEy)
      
      w = ceiling(nrow(SymE_dat()) * 0.03) # calculate the number of bins
      n = colnames(SymE_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(SymE_dat()), aes(x = .data[[n]])) +
        # geom_histogram( color="white", fill="black", bins = w) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of ", input$SymEy), 
             x = input$SymEy) +
        theme_nonan()
      
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goSymENT, {
    output$SymEacf <-  renderPlot({
      
      a = acf(SymE_dat(), plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(SymE_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$SymEy)) + 
        theme_nonan() # add the nonan plot theme on   
      
    })
  }) # observeEvent
  
  # Print the data so we can see what column is actually being selected. For debugging only
  # output$SymEdatHead <- renderTable({
  #   head(get(input$dataChoice3))
  #   head(SymE_dat())
  # })
  
  # PSR ---------------------------------------------------------------------
  ## RQA ---------------------------------------------------------------------
  
  # get a list of the column names in the data frame
  n4 = reactive({
    names(get(input$dataChoice4))
  })
  
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoice4, {
    updateSelectInput(inputId = "rqax", choices = n4())
    updateSelectInput(inputId = "rqay", choices = n4(), selected = n4()[2])
  })
  
  # Select the desired data frame and by default the second column for analysis
  rqa_dat = reactive({
    get(input$dataChoice4) |>
      select(all_of(input$rqay)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$rqaTS <- renderPlotly({
    
    plot_dat = get(input$dataChoice4)
    # plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$dfay]], type = 'scatter', mode = 'lines', 
    #         color = I('black')) %>% # Aesthetics for the plot
    #   layout(title = list(text = paste0("Time series of ", input$dfay)),
    #          xaxis = list(title = "data Index"),
    #          yaxis = list(title = paste0(input$dfay)))
    
    ggplot(plot_dat, aes(x = 1:nrow(plot_dat), y = .data[[input$rqay]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$rqay), 
           x = "Index") + 
      theme_nonan()
  })
  
  # Run ami and fnn if the button is selected.
  # Add in a reactive if statement here 
  emb_dim = reactive({
    logscale(input$minScale, input$maxScale, input$scaleRatio) 
  })
  
  # DFA calculation
  rqaResult <- eventReactive(input$goRQA, {
    
    rqa(ts1 = rqa_dat(), ts2 = rqa_dat(), embed = input$embed, delay = input$delay, normalize = input$normalize,
        rescale = input$rescale, mindiagline = input$mindiagline, minvertline = input$minvertline, t_win = input$twin,
        radius = input$radius, whiteline = 0, recpt = 1)
    
    # if (input$psr_rqa && input$lag_rqa){
    #   
    #   # AMI
    #   ami_out = ami(x = rqa_dat(), y = rqa_dat(), L = input$lag_rqa, bins = 0) # Freely determine bins
    #   tau = as.data.frame(ami_out[1]) # tau data frame
    #   min_tau = tau[1,1]
    #   
    #   # FNN
    #   fnn_out = fnn(x = rqa_dat(), tau = ami_out$tau[1,1], mmax = 12, rtol = 15, atol = 2) # Use defaults
    #   emb_dim = as.numeric(fnn_out[2])
    #   
    #   # RQA
    #   rqa(ts1 = rqa_dat(), ts2 = rqa_dat(), embed = emb_dim, delay = min_tau, normalize = input$normalize, 
    #       rescale = input$rescale, mindiagline = input$mindiagline, minvertline = input$minvertline, t_win = input$twin, 
    #       radius = input$radius, whiteline = 0, recpt = input$recpt)
    # } else {
    # 
    # rqa(ts1 = rqa_dat(), ts2 = rqa_dat(), embed = input$embed, delay = input$delay, normalize = input$normalize, 
    #     rescale = input$rescale, mindiagline = input$mindiagline, minvertline = input$minvertline, t_win = input$twin, 
    #     radius = input$radius, whiteline = 0, recpt = input$recpt)
    # }
  })
  
  # DFA plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goRQA, {
    output$rqaPlot <- renderPlot({
      plot_rqa(rqaResult())
    })
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goRQA, {
    output$histogram_rqa <- renderPlot({
      #hist(dfa_dat(), main = paste("Histogram of ", input$dfay), xlab = input$dfay)
      
      w = ceiling(nrow(rqa_dat()) * 0.03) # calculate the number of bins
      n = colnames(rqa_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(rqa_dat()), aes(x = .data[[n]])) +
        # geom_histogram( color="white", fill="black", bins = w) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of ", input$rqay), 
             x = input$rqay) +
        theme_nonan()
      
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goRQA, {
    output$autocorr_rqa <-  renderPlot({
      
      a = acf(rqa_dat(), plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(rqa_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$rqay)) + 
        theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  
  
  # Print out the DFA results
  observeEvent(input$goRQA, {
    output$rqaResults <- renderPrint({
      # rqaResult()
      cat("rr:", rqaResult()$rqa$rr, "\n", 
          "det:", rqaResult()$rqa$det, "\n",
          "div:", rqaResult()$rqa$div, "\n",
          "nrline:", rqaResult()$rqa$nrline, "\n", 
          "ratio:", rqaResult()$rqa$ratio, "\n", 
          "maxline:", rqaResult()$rqa$maxline, "\n", 
          "mealine:", rqaResult()$rqa$mealine, "\n", 
          "lam:", rqaResult()$rqa$lam, "\n", 
          "tt:", rqaResult()$rqa$tt, "\n", 
          "vmax:", rqaResult()$rqa$vmax, "\n", 
          "entropy:", rqaResult()$rqa$entropy, "\n", 
          "rentropy:", rqaResult()$rqa$rentropy, "\n")
      
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportRQA, {
    assign(input$exportRQAname, rqaResult(), envir = globalenv())
    
    output$rqaResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  
  # Print the data so we can see what column is actually being selected. For debugging only
  #output$datHead <- renderTable({
  # head(dfa_dat())
  #})
  
  ## LyE ---------------------------------------------------------------------
  
  # # get a list of the column names in the data frame
  # n = reactive({
  #   names(get(input$dataChoice))
  # })
  # 
  # 
  # # Update x and y choices based on the selected dataframe
  # observeEvent(input$dataChoice, {
  #   updateSelectInput(inputId = "dfax", choices = n())
  #   updateSelectInput(inputId = "dfay", choices = n(), selected = n()[2])
  # })
  # 
  # # Select the desired data frame and by default the second column for analysis
  # dfa_dat = reactive({
  #   get(input$dataChoice) |>
  #     select(all_of(input$dfay)) |>
  #     as.matrix()
  # })
  # 
  # # plot the time series of the data
  # output$dfaTS <- renderPlotly({
  #   
  #   plot_dat = get(input$dataChoice5)
  #   # plot_ly(data = plot_dat, x = ~1:nrow(plot_dat), y = ~.data[[input$dfay]], type = 'scatter', mode = 'lines', 
  #   #         color = I('black')) %>% # Aesthetics for the plot
  #   #   layout(title = list(text = paste0("Time series of ", input$dfay)),
  #   #          xaxis = list(title = "data Index"),
  #   #          yaxis = list(title = paste0(input$dfay)))
  #   
  #   ggplot(plot_dat, aes(x = 1:nrow(plot_dat), y = .data[[input$dfay]])) +
  #     geom_line() +
  #     labs(title = paste0("Time series of ", input$dfay)) + 
  #     theme_nonan()
  # })
  # 
  # # Set the scales for the DFA function
  # scales = reactive({
  #   logscale(input$minScale, input$maxScale, input$scaleRatio) 
  # })
  # 
  # # DFA calculation
  # dfaResult <- eventReactive(input$goDFA, {
  #   dfa(dfa_dat(), order = input$order, verbose = 1, scales = scales(), scale_ratio = input$scaleRatio)
  #   
  # })
  # 
  # # DFA plot -- generate the plot only when the "Go" button has been clicked
  # observeEvent(input$goDFA, {
  #   output$dfaPlot <- renderPlot({
  #     plot_dfa(dfaResult())
  #   })
  # }) # observeEvent
  # 
  # # Histogram plot -- generate the plot only when the "Go" button has been clicked
  # observeEvent(input$goDFA, {
  #   output$histogram <- renderPlot({
  #     #hist(dfa_dat(), main = paste("Histogram of ", input$dfay), xlab = input$dfay)
  #     
  #     w = ceiling(nrow(dfa_dat()) * 0.03) # calculate the number of bins
  #     n = colnames(dfa_dat())[1] # Get the column name to use below
  #     ggplot(as.data.frame(dfa_dat()), aes(x = .data[[n]])) +
  #      # geom_histogram( color="white", fill="black", bins = w) +
  #       geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
  #       labs(title = paste("Density Plot of ", input$dfay), 
  #            x = input$dfay) +
  #       theme_nonan()
  #     
  #   })
  # }) # observeEvent
  # 
  # # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  # observeEvent(input$goDFA, {
  #   output$autocorr <-  renderPlot({
  #     
  #     a = acf(dfa_dat(), plot = F)
  #     conf.level <- 0.95 # set this at 0.95 for 95% confidence
  #     ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(dfa_dat())) # calculate the confidence intervals
  #     df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
  #     
  #     ggplot(data = df, mapping = aes(x = lag, y = acf)) +
  #       geom_hline(aes(yintercept = 0)) + # lag = 0
  #       geom_hline(aes(yintercept = ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + # confidence intervals
  #       geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = 'white', linewidth = 0.7) + 
  #       geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
  #       labs(title = paste("Autocorrelation of ", input$dfay)) + 
  #       theme_nonan() # add the nonan plot theme on
  #     
  #   })
  # }) # observeEvent
  # 
  # 
  # # Print out the DFA results
  # observeEvent(input$goDFA, {
  #   output$dfaResults <- renderPrint({
  #     cat("Log Scales:", dfaResult()$log_scales, "\n", "Log RMS:", dfaResult()$log_rms, "\n", "Alpha:", dfaResult()$alpha)
  #     
  #   })
  # })
  # 
  # # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  # observeEvent(input$exportDFA, {
  #   assign(input$exportLYEname, dfaResult(), envir = globalenv())
  #   
  #   output$dfaResults <- renderPrint({
  #     cat("Exported to global environment. Close the app to view.")
  #   }) # renderPrint
  # }) # observeEvent
  # 
  # 
  # # Print the data so we can see what column is actually being selected. For debugging only
  # #output$datHead <- renderTable({
  # # head(dfa_dat())
  # #})
  
} # server

# Run the application 
shinyApp(ui = ui, server = server)
