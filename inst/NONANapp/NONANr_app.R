

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
                                                 ) # mainpanel
                                               ) #sidebarlayout
                                      ) # symbolic entropy tabpanel
                                      
                           ), # navbarMenu
                           
                           # PSR -----------------------------------------------------------------
                           navbarMenu("Phase Space Reconstruction",
                                      
                                      ## AMI -----------------------------------------------------------------
                                      tabPanel("Average Mutual Information", 
                                               h4(strong("Average Mutual Information")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoiceAMI", "Select Data", choices = list("Your Data" = c(myDataFrames) , selected = NULL)),
                                                   selectInput("amix", "Select X axis:", choices = NULL),
                                                   selectInput("amiy", "Select Y axis:", choices = NULL),
                                                   numericInput("ami_lag", "Lag:", value = 100, step = 10, min = 0),
                                                   numericInput("ami_bins", "Number of bins:", value = 30, step = 1, min = 0, max = 1000),
                                                   fluidRow(
                                                     
                                                     column(width = 6,
                                                            actionButton("goAMI", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportAMI", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportAMIname", "Choose a name for your variable before exporting", "ami_out"),
                                                   
                                                 ), # sidebarpanel
                                                 mainPanel(
                                                   fluidRow( 
                                                     column(12,  plotlyOutput('amiTS')), # single row just for the time series plot
                                                   ), 
                                                   br(),
                                                   br(),
                                                   fluidRow(
                                                     column(4,  plotOutput('amiPlot')),
                                                     column(4,  plotOutput('histogram_ami')),
                                                     column(4,  plotOutput('autocorr_ami'))
                                                   ),
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("amiResults"), 
                                                   br(),
                                                   br()
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ), # AMI tabpanel
                                      
                                      
                                      ## FNN -----------------------------------------------------------------
                                      tabPanel("False Nearest Neighbours", 
                                               h4(strong("False Nearest Neighbours")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoiceFNN", "Select Data", choices = list("Your Data" = c(myDataFrames) , selected = NULL)),
                                                   selectInput("fnnx", "Select X axis:", choices = NULL),
                                                   selectInput("fnny", "Select Y axis:", choices = NULL),
                                                   numericInput("fnn_maxDim", "Maximum Embedding Dimension:", value = 10),
                                                   numericInput("fnn_tau", "Time Lag:", value = 1),
                                                   numericInput("fnn_rtol", "rtol:", value = 10, min = 1, step = 1),
                                                   numericInput("fnn_atol", "atol:", value = 15, min = 1, step = 1),
                                                   numericInput("fnn_tol", "Proportion of false neighbours:", value = 0.01, min = 0, max = 1, step = 0.01),
                                                   fluidRow(
                                                     
                                                     column(width = 6,
                                                            actionButton("goFNN", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportFNN", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportFNNname", "Choose a name for your variable before exporting", "fnn_out"),
                                                   
                                                 ), # sidebarpanel
                                                 mainPanel(
                                                   fluidRow( 
                                                     column(12,  plotlyOutput('fnnTS')), # single row just for the time series plot
                                                   ), 
                                                   br(),
                                                   br(),
                                                   fluidRow(
                                                     column(4,  plotOutput('fnnPlot')),
                                                     column(4,  plotOutput('histogram_fnn')),
                                                     column(4,  plotOutput('autocorr_fnn'))
                                                   ),
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("fnnResults"), 
                                                   br(),
                                                   br()
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ), # FNN tabpanel
                                      
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
                                                   numericInput("mindiagline", "Min Diagonal Length:", value = 2, step = 1, min = 1),
                                                   numericInput("minvertline", "Min Vertical Length:", value = 2, step = 1, min = 1),
                                                   numericInput("twin", "Theiler Window:", value = 0, step = 0.1),
                                                   numericInput("radius", "Radius:", value = 0.0001, step = 0.0001, min = 0),
                                                   numericInput("rqa_maxDim", "Maximum Embedding Dimension:", value = 10),
                                                   numericInput("rqa_rtol", "rtol:", value = 10, min = 1, step = 1),
                                                   numericInput("rqa_atol", "atol:", value = 15, min = 1, step = 1),
                                                   numericInput("rqa_tol", "Proportion of false neighbours:", value = 0.01, min = 0, max = 1, step = 0.01),
                                                   # numericInput("whiteline", "Rescale:", value = 2, step = 0.1),
                                                   # numericInput("recpt", "Plot:", value = 1, step = 1, min = 0, max = 1),
                                                   fluidRow(
                                                     column(width = 7,
                                                            checkboxInput(inputId = "psr_rqa",
                                                                          label = "Automatically perform FNN and AMI?",
                                                                          value = TRUE)),
                                                     column(width = 5,
                                                            numericInput(inputId = "lag_rqa",
                                                                         label = "Choose a maximum lag.",
                                                                         value = 100))
                                                     ),
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
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ), # RQA tabpanel
                                      
                                      ## LyE ---------------------------------------------------------------------
                                      tabPanel("Lyapunov Exponent", 
                                               h4(strong("Lyapunov Exponent")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoiceLYE", "Select Data", choices = list("Your Data" = c(myDataFrames) , selected = NULL)),
                                                   selectInput("lyex", "Select X axis:", choices = NULL),
                                                   selectInput("lyey", "Select Y axis:", choices = NULL),
                                                   sliderInput("lye_fs", "Sampling Frequency:", value = 100, step = 10, min = 0, max = 2000),
                                                   numericInput("lye_nsteps", "Number of Time Steps:", value = 500, step = 10),
                                                   sliderInput("lye_regpoints", "Data point for regression line:", value = c(10, 500), step = 1, min = 0, max = 1000),
                                                   numericInput("lye_maxDim", "Embedding Dimension:", value = 10),
                                                   numericInput("lye_delay", "Time Lag:", value = 1),
                                                   numericInput("lye_rtol", "rtol:", value = 10, min = 1, step = 1),
                                                   numericInput("lye_atol", "atol:", value = 15, min = 1, step = 1),
                                                   numericInput("lye_tol", "Proportion of false neighbours:", value = 0.01, min = 0, max = 1, step = 0.01),
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
                                                     column(4,  plotOutput('lyePlot')),
                                                     column(4,  plotOutput('histogram_lye')),
                                                     column(4,  plotOutput('autocorr_lye'))
                                                   ),
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("lyeResults"), 
                                                   br(),
                                                   br()
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ) # LYE tabpanel
                          ), # navbarMenu
                           
                                     
                           # Simulations -----------------------------------------------------------------
                           navbarMenu("Simulations",
                                      ## FGN SIM ---------------------------------------------------------------------
                                      tabPanel("Fractional Gaussian Noise", 
                                               h4(strong("Fractional Gaussian Noise")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   numericInput("fgn_n", "Number of Points:", value = 1000, step = 1),
                                                   numericInput("fgn_h", "Hurst Exponent:", value = 0.75, step = 0.01, min = 0.01, max = 0.99),
                                                   numericInput("fgn_mu", "Mean:", value = 0, step = 0.1),
                                                   numericInput("fgn_sigma", "Standard Deviation:", value = 1, min = 0.1, step = 0.1),
                                                   fluidRow(
                                                     
                                                     column(width = 6,
                                                            actionButton("goFGN", "Create")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportFGN", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportFGNname", "Choose a name for your variable before exporting", "fgn_out"),
                                                   
                                                 ), # sidebarpanel
                                                 mainPanel(
                                                   fluidRow( 
                                                     column(12,  plotlyOutput('fgnTS')), # single row just for the time series plot
                                                   ), 
                                                   br(),
                                                   br(),
                                                   fluidRow(
                                                     column(6,  plotOutput('histogram_fgn')),
                                                     column(6,  plotOutput('autocorr_fgn'))
                                                   ),
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("fgnResults"), 
                                                   br(),
                                                   br()
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ), # FGN tabpanel
                                      
                                      ## IAAFFT ---------------------------------------------------------------------
                                      tabPanel("IAAFFT", 
                                               h4(strong("IAAFFT")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoiceIAAFFT", "Select Data", choices = list("Your Data" = c(myDataFrames) , selected = NULL)),
                                                   selectInput("iaafftx", "Select X axis:", choices = NULL),
                                                   selectInput("iaaffty", "Select Y axis:", choices = NULL),
                                                   numericInput("iaafft_surr", "Number of Surrogates:", value = 9, step = 1, min = 1),
                                                   fluidRow(
                                                     
                                                     column(width = 6,
                                                            actionButton("goIAAFFT", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportIAAFFT", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportSIMname", "Choose a name for your variable before exporting", "iaafft_out"),
                                                   
                                                 ), # sidebarpanel
                                                 mainPanel(
                                                   fluidRow( 
                                                     column(12,  plotOutput('iaafftPlot')) # single row just for the time series plot
                                                   ),
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("iaafftResults")
                                                 ) # mainpanel
                                               ) # sidebarlayout
                                      ), # IAAFFT tabpanel
                                      
                                      ## Pseudoperiodic ----------------------------------------------------------
                                      tabPanel("Pseudoperiodic",
                                               h4(strong("Pseudoperiodic Surrogation")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput("dataChoicePseudo", "Select Data", choices = list("Your Data" = c(myDataFrames) , selected = NULL)),
                                                   selectInput("pseudox", "Select X axis:", choices = NULL),
                                                   selectInput("pseudoy", "Select Y axis:", choices = NULL),
                                                   numericInput("pseudo_tau", "Time Lag: ", value = 0, step = 1, min = 0),
                                                   numericInput("pseudo_dim", "Embedding Dimension: ", value = 1, step = 1, min = 1),
                                                   numericInput("pseudo_rho", "Noise Radius: ", value = 0.1, step = 0.1, min = 0.1),
                                                   fluidRow(
                                                     column(width = 6,
                                                            actionButton("goPseudo", "Analyze")
                                                     ),
                                                     column(width = 6,
                                                            actionButton("exportPseudo", "Export",
                                                                         style = "position: absolute; right: 19px;")
                                                     )
                                                   ), # fluidRow for action buttons
                                                   textInput("exportPPname", "Choose a name for your variable before exporting", "pseudo_out"),

                                                 ), # sidebarPanel
                                                 mainPanel(
                                                   fluidRow(
                                                     column(12,  plotOutput('pseudoPlot')) # single row just for the time series plot
                                                   ),
                                                   br(),
                                                   br(),
                                                   verbatimTextOutput("pseudoResults")
                                                 ) # mainpanel
                                               ) # sidebarLayout
                                      ) # Pseudoperiodic tabpanel
                           ) #navbar menu
                ) # navbar page
) # fluidpage


# Server ------------------------------------------------------------------

# Define server logic 
server <- function(input, output) {
  
  # Fractal Methods ---------------------------------------------------------------------
  ## DFA ---------------------------------------------------------------------
  
  # Update data to add in an index column for plotting purposes
  new_dat = reactive({
    get(input$dataChoice) |>
      mutate(Index = row_number()) |>
      select(Index, everything())
  })
  
  # get a list of the column names in the data frame
  n = reactive({
    names(new_dat())
  })
  
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoice, {
    updateSelectInput(inputId = "dfax", choices = n())
    updateSelectInput(inputId = "dfay", choices = n(), selected = n()[3])
  })
  
  # Select the desired data frame and by default the second column for analysis
  dfa_dat = reactive({
    get(input$dataChoice) |>
      select(all_of(input$dfay)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$dfaTS <- renderPlotly({
    
    plot_dat = new_dat() #get(input$dataChoice)
    
    ggplot(plot_dat, aes(x = .data[[input$dfax]], y = .data[[input$dfay]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$dfay), 
           x = input$dfax) + 
      nonanR::theme_nonan()
    
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
      
      w = ceiling(nrow(dfa_dat()) * 0.03) # calculate the number of bins
      n = colnames(dfa_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(dfa_dat()), aes(x = .data[[n]])) +
        # geom_histogram( color="white", fill="black", bins = w) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of", input$dfay), 
             x = input$dfay) +
        nonanR::theme_nonan()
      
      
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
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$dfay)) + 
        nonanR::theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  
  
  # Print out the DFA results
  observeEvent(input$goDFA, {
    output$dfaResults <- renderPrint({
      cat(input$dfay, "was analyzed", "\n",
          "Log Scales:", dfaResult()$log_scales, "\n", 
          "Log RMS:", dfaResult()$log_rms, "\n", 
          "Alpha:", dfaResult()$alpha)
      
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportDFA, {
    export_dat = append(dfaResult(), list("time_series" = input$dfay))
    
    assign(input$exportDFAname, export_dat, envir = globalenv())
    
    output$dfaResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  
  ## MFDFA ---------------------------------------------------------------------
  
  
  # Update data to add in an index column for plotting purposes
  new_dat = reactive({
    get(input$dataChoiceMFDFA) |>
      mutate(Index = row_number()) |>
      select(Index, everything())
  })
  
  # get a list of the column names in the data frame
  n_mfdfa = reactive({
    names(new_dat())
  })
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoiceMFDFA, {
    updateSelectInput(inputId = "mfdfax", choices = n_mfdfa())
    updateSelectInput(inputId = "mfdfay", choices = n_mfdfa(), selected = n_mfdfa()[3])
  })
  
  # Select the desired data frame and by default the second column for analysis
  mfdfa_dat = reactive({
    get(input$dataChoiceMFDFA) |>
      select(all_of(input$mfdfay)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$mfdfaTS <- renderPlotly({
    
    plot_dat = new_dat()#get(input$dataChoiceMFDFA)
    
    ggplot(plot_dat, aes(x = .data[[input$mfdfax]], y = .data[[input$mfdfay]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$mfdfay), 
           x = input$mfdfax) + 
      theme_nonan()
  })
  
  # Set the q order parameter. This will always be a symmetric range based on what the parameter is.
  q_order = reactive({
    -input$q:input$q
  })
  # Set the scales for the MFDFA function
  scales = reactive({
    logscale(input$minScale_mfdfa, input$maxScale_mfdfa, input$scaleRatio_mfdfa) 
  })
  
  # MFDFA calculation
  mfdfaResult <- eventReactive(input$goMFDFA, {
    mfdfa(mfdfa_dat(), q = q_order(), order = input$order_mfdfa, scales = scales(), scale_ratio = input$scaleRatio_mfdfa)
  })
  
  
  # MFDFA plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goMFDFA, {
    output$mfdfaPlot <- renderPlot({
      plot_mfdfa(mfdfaResult(), do.surrogate = T, nsurrogates = 19, return.ci = T)
    })
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goMFDFA, {
    output$histogram_mfdfa <- renderPlot({

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
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$mfdfay)) + 
        theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  
  
  # Print out the MFDFA results
  observeEvent(input$goMFDFA, {
    output$mfdfaResults <- renderPrint({
      
      df = mfdfaResult()
      
      colnames(df$log_fq) = df$q
      rownames(df$log_fq) = df$scales
      
      cat(input$dfay, "was analyzed", "\n",
          "Log fq:", "\n")
      print(df$log_fq)
      cat("Log Scales:", mfdfaResult()$log_scale, "\n", 
          "Hq:", mfdfaResult()$Hq, "\n", 
          "Tau:", mfdfaResult()$Tau, "\n", 
          "H:", mfdfaResult()$h, "\n", 
          "Dh:", mfdfaResult()$Dh)
      
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportMFDFA, {
    export_dat = append(mfdfaResult(), list(time_series = input$mfdfay))
    
    assign(input$exportMFDFAname, export_dat, envir = globalenv())
    
    output$mfdfaResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  
  ## bayesH -----------------------------------------------------------------
  
  # Update data to add in an index column for plotting purposes
  new_dat = reactive({
    get(input$dataChoiceMFDFA) |>
      mutate(Index = row_number()) |>
      select(Index, everything())
  })
  
  # get a list of the column names in the data frame
  n_bayes = reactive({
    names(new_dat())
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
    
    plot_dat = new_dat() #get(input$dataChoicebayesH)
    
    ggplot(plot_dat, aes(x = .data[[input$bayesHx]], y = .data[[input$bayesHy]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$bayesHy),
           x = input$bayesHx) +
      theme_nonan()
    
  })
  
  # bayesH calculation
  bayesHresult <- eventReactive(input$gobayesH, {
    bayesH(bayesH_dat(), n = input$bayesN)
  })
  
  # Print out the median bayesH result
  observeEvent(input$gobayesH, {
    output$bayesHResults <- renderPrint({
      cat(input$bayesHy, "was analyzed", "\n",
          "bayesH:", median(bayesHresult()))
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportbayesH, {
    assign(input$exportbayesHname, list("hurst_pdf" = bayesHresult(), 
                                        "median_hurst" =  median(bayesHresult()),
                                        "time_series" = input$bayesHy), envir = globalenv())
    
    output$bayesHResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$gobayesH, {
    output$histogram_bayesH <- renderPlot({

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
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) +
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$bayesHy)) +
        theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  

  # Entropy -----------------------------------------------------------------
  ## Sample Entropy -----------------------------------------------------------------
  
  # Update data to add in an index column for plotting purposes
  new_dat = reactive({
    get(input$dataChoice1) |>
      mutate(Index = row_number()) |>
      select(Index, everything())
  })
  
  # get a list of the column names in the data frame
  n1 = reactive({
    names(new_dat())
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
    
    plot_dat = new_dat() # get(input$dataChoice1)
    
    ggplot(plot_dat, aes(x = .data[[input$SEx]], y = .data[[input$SEy]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$SEy), 
           x = input$SEx) + 
      theme_nonan()
    
  })
  
  # Entropy calculation
  SEresult <- eventReactive(input$goSEENT, {
    Ent_Samp(SE_dat(), m = input$SEm, R = input$SEr)
  })
  
  # Print out the sample entropy results
  observeEvent(input$goSEENT, {
    output$SEresults <- renderPrint({
      cat(input$SEy, "was analyzed", "\n",
          "Sample Entropy:", SEresult())
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportSEENT, {
    assign(input$exportSEENTname, list("SE" = SEresult(),
                                       "m" = input$SEm, 
                                       "R" = input$SEr, 
                                       "time_series" = input$SEy), envir = globalenv())
    
    output$SEresults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goSEENT, {
    output$SEhist <- renderPlot({
      
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
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$SEy)) + 
        theme_nonan() # add the nonan plot theme on    
      
    })
  }) # observeEvent

  
  ## Approximate Entropy -----------------------------------------------------------------
  
  # Update data to add in an index column for plotting purposes
  new_dat = reactive({
    get(input$dataChoice2) |>
      mutate(Index = row_number()) |>
      select(Index, everything())
  })
  
  # get a list of the column names in the data frame
  n2 = reactive({
    names(new_dat())
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
    
    plot_dat = new_dat() # get(input$dataChoice2)
    
    ggplot(plot_dat, aes(x = .data[[input$AEx]], y = .data[[input$AEy]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$AEy), 
           x = input$AEx) + 
      theme_nonan()
    
  })
  
  # Approximate entropy calculation
  AEresult <- eventReactive(input$goAENT, {
    Ent_Ap(AE_dat(), dim = input$AEdim, R = input$AEr)
  })
  
  # Print out the approximate entropy results
  observeEvent(input$goAENT, {
    output$AEresults <- renderPrint({
      cat(input$SEy, "was analyzed", "\n",
          "Approximate Entropy:", AEresult())
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportAENT, {
    assign(input$exportAENTname, list("AE" = AEresult(),
                                      "dim" = input$AEdim,
                                      "R" = input$AEr, 
                                      "time_series" = input$AEy), envir = globalenv())
    
    output$AEresults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goAENT, {
    output$AEhist <- renderPlot({

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
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$AEy)) + 
        theme_nonan() # add the nonan plot theme on   
      
    })
  }) # observeEvent

  
  ## Symbolic Entropy --------------------------------------------------------
  
  # Update data to add in an index column for plotting purposes
  new_dat = reactive({
    get(input$dataChoice3) |>
      mutate(Index = row_number()) |>
      select(Index, everything())
  })
  
  # get a list of the column names in the data frame
  n3 = reactive({
    names(new_dat())
  })
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoice3, {
    updateSelectInput(inputId = "SymEx", choices = n3())
    updateSelectInput(inputId = "SymEy", choices = n3(), selected = n3()[2])
  })
  
  # set the threshold value to the mean of the selected column
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
    
    plot_dat = new_dat() # get(input$dataChoice3)
    
    ggplot(plot_dat, aes(x = .data[[input$SymEx]], y = .data[[input$SymEy]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$SymEy),
           x = input$SymEx) + 
      theme_nonan()
  })
  
  # Symbolic entropy calculation
  SymEresult <- eventReactive(input$goSymENT, {
    Ent_Sym(SymE_dat(), thresholdVal = input$SymEthresh, seqLength = input$SymEseql)
  })
  
  # Print out the approximate entropy results
  observeEvent(input$goSymENT, {
    output$SymEresults <- renderPrint({
      cat(input$SymEy, "was analyzed", "\n",
          "Symbolic Entropy:", SymEresult())
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportSymENT, {
    assign(input$exportSymENTname, list("SymE" = SymEresult(),
                                        "threshold_val" = input$SymEthresh,
                                        "seq_length" =input$SymEseql,
                                        "time_series" = input$SymEy), envir = globalenv())
    
    output$SymEresults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goSymENT, {
    output$SymEhist <- renderPlot({

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
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$SymEy)) + 
        theme_nonan() # add the nonan plot theme on   
      
    })
  }) # observeEvent
  
  
  # PSR ---------------------------------------------------------------------
  ## AMI ---------------------------------------------------------------------
  
  # Update data to add in an index column for plotting purposes
  new_dat = reactive({
    get(input$dataChoiceAMI) |>
      mutate(Index = row_number()) |>
      select(Index, everything())
  })
  
  ami_n = reactive({
    names(new_dat())
  })
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoiceAMI, {
    updateSelectInput(inputId = "amix", choices = ami_n())
    updateSelectInput(inputId = "amiy", choices = ami_n(), selected = ami_n()[2])
  })
  
  # Select the desired data frame and by default the second column for analysis
  ami_dat = reactive({
    get(input$dataChoiceAMI) |>
      select(all_of(input$amiy)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$amiTS <- renderPlotly({
    
    plot_dat = new_dat() # get(input$dataChoiceAMI)
    
    ggplot(plot_dat, aes(x = .data[[input$amix]], y = .data[[input$amiy]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$amiy), 
           x = input$amix) + 
      theme_nonan()
  })
  
  # AMI calculation
  amiResult <- eventReactive(input$goAMI, {
    ami(x = ami_dat(), y = ami_dat(), L = input$ami_lag, bins = input$ami_bins) # Freely determine bins
  })
  
  # AMI plot
  output$amiPlot <- renderPlot({
    plot_ami(amiResult())
  })
  
  # Print out the AMI results
  observeEvent(input$goAMI, {
    output$amiResults <- renderPrint({
      tau = as.data.frame(amiResult()[1]) # tau data frame
      cat(input$amiy, "was analyzed", "\n",
          "Tau:", tau[1,2], "\n",
          "Lag:", tau[1,1])
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportAMI, {
    export_dat = append(amiResult(), list("max_lag" = input$ami_lag,
                                          "bins" = input$ami_bins,
                                          "time_series" = input$amiy))
    
    assign(input$exportAMIname, export_dat, envir = globalenv())
    
    output$amiResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goAMI, {
    output$histogram_ami <- renderPlot({

      w = ceiling(nrow(ami_dat()) * 0.03) # calculate the number of bins
      n = colnames(ami_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(ami_dat()), aes(x = .data[[n]])) +
        # geom_histogram( color="white", fill="black", bins = w) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of ", input$amiy), 
             x = input$amiy) +
        theme_nonan()
      
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goAMI, {
    output$autocorr_ami <-  renderPlot({
      
      a = acf(ami_dat(), plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(ami_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$amiy)) + 
        theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  
  ## FNN ---------------------------------------------------------------------
  
  # Update data to add in an index column for plotting purposes
  new_dat = reactive({
    get(input$dataChoiceFNN) |>
      mutate(Index = row_number()) |>
      select(Index, everything())
  })
  
  fnn_n = reactive({
    names(new_dat())
  })
    
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoiceFNN, {
    updateSelectInput(inputId = "fnnx", choices = fnn_n())
    updateSelectInput(inputId = "fnny", choices = fnn_n(), selected = fnn_n()[2])
  })
  
  # Select the desired data frame and by default the second column for analysis
  fnn_dat = reactive({
    get(input$dataChoiceFNN) |>
      select(all_of(input$fnny)) |>
      as.matrix()
  })
  
  # plot the time series of the data
  output$fnnTS <- renderPlotly({
    
    plot_dat = new_dat() # get(input$dataChoiceFNN)
    
    ggplot(plot_dat, aes(x = .data[[input$fnnx]], y = .data[[input$fnny]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$fnny), 
           x = input$fnnx) + 
      theme_nonan()
  })
  
  # FNN calculation
  fnnResult <- eventReactive(input$goFNN, {
    false_nearest_neighbors(fnn_dat(), maxDim = input$fnn_maxDim, delay = input$fnn_tau, 
                            rtol = input$fnn_rtol, atol = input$fnn_atol, fnn_tol = input$fnn_tol)
    })
  
  # FNN plot
  output$fnnPlot <- renderPlot({
    plot_fnn(fnnResult())
  })
  
  # Print out the FNN results
  observeEvent(input$goFNN, {
    output$fnnResults <- renderPrint({
      cat(input$fnny, "was analyzed", "\n",
          "Embedding Dimension:", as.numeric(fnnResult()$dim))
    })
  })
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportFNN, {
    
    export_dat = append(fnnResult(), list("max_dim" = input$fnn_maxDim,
                                          "tau" = input$fnn_tau,
                                          "rtol" = input$fnn_rtol,
                                          "atol" = input$fnn_atol,
                                          "fnn_tol" = input$fnn_tol,
                                          "time_series" = input$fnny))
    
    
    assign(input$exportFNNname, export_dat, envir = globalenv())
    
    output$fnnResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goFNN, {
    output$histogram_fnn <- renderPlot({

      w = ceiling(nrow(fnn_dat()) * 0.03) # calculate the number of bins
      n = colnames(fnn_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(fnn_dat()), aes(x = .data[[n]])) +
        # geom_histogram( color="white", fill="black", bins = w) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of ", input$fnny), 
             x = input$fnny) +
        theme_nonan()
      
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goFNN, {
    output$autocorr_fnn <-  renderPlot({
      
      a = acf(fnn_dat(), plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(fnn_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$fnny)) + 
        theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  
  
  
  ## RQA ---------------------------------------------------------------------
  
  # Update data to add in an index column for plotting purposes
  new_dat = reactive({
    get(input$dataChoice4) |>
      mutate(Index = row_number()) |>
      select(Index, everything())
  })
  
  # get a list of the column names in the data frame
  n4 = reactive({
    names(new_dat())
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
    
    plot_dat = new_dat() # get(input$dataChoice4)
    
    ggplot(plot_dat, aes(x = .data[[input$rqax]], y = .data[[input$rqay]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$rqay), 
           x = input$rqax) + 
      theme_nonan()
  })
  
  # Run ami and fnn if the button is selected.
  # Add in a reactive if statement here 
  emb_dim = reactive({
    logscale(input$minScale, input$maxScale, input$scaleRatio) 
  })
  
  # DFA calculation
  rqaResult <- eventReactive(input$goRQA, {
    
    if (input$psr_rqa && input$lag_rqa){

      # AMI
      ami_out = ami(x = rqa_dat(), y = rqa_dat(), L = input$lag_rqa, bins = 0) # Freely determine bins
      tau = ami_out$tau[1,1]

      # FNN
      fnn_out = false_nearest_neighbors(rqa_dat(), maxDim = input$fnn_maxDim, delay = input$fnn_tau, 
                              rtol = input$fnn_rtol, atol = input$fnn_atol, fnn_tol = input$fnn_tol)
      emb_dim = as.numeric(fnn_out$dim)

      # RQA
      rqa(ts1 = rqa_dat(), ts2 = rqa_dat(), embed = emb_dim, delay = tau, normalize = input$normalize,
          rescale = input$rescale, mindiagline = input$mindiagline, minvertline = input$minvertline, t_win = input$twin,
          radius = input$radius, whiteline = 0, recpt = 1)
      
    } else {
    rqa(ts1 = rqa_dat(), ts2 = rqa_dat(), embed = input$embed, delay = input$delay, normalize = input$normalize,
        rescale = input$rescale, mindiagline = input$mindiagline, minvertline = input$minvertline, t_win = input$twin,
        radius = input$radius, whiteline = 0, recpt = 1)
    }
  })
  
  # RQA plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goRQA, {
    output$rqaPlot <- renderPlot({
      plot_rqa(rqaResult())
    })
  }) # observeEvent
  
  # Histogram plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goRQA, {
    output$histogram_rqa <- renderPlot({

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
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$rqay)) + 
        theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  
  
  # Print out the RQA results
  observeEvent(input$goRQA, {
    output$rqaResults <- renderPrint({
      # rqaResult()
      cat(input$rqay, "was analyzed", "\n",
          "rr:", rqaResult()$rqa$rr, "\n",
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
    export_dat = append(rqaResult(), list("time_series" = input$rqay))
    
    assign(input$exportRQAname, export_dat, envir = globalenv())
    
    output$rqaResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  
  ## LyE ---------------------------------------------------------------------
  
  # Update data to add in an index column for plotting purposes
  new_dat = reactive({
    get(input$dataChoiceLYE) |>
      mutate(Index = row_number()) |>
      select(Index, everything())
  })
  
  # get a list of the column names in the data frame
  lye_n = reactive({
    names(new_dat())
  })
  
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoiceLYE, {
    updateSelectInput(inputId = "lyex", choices = lye_n())
    updateSelectInput(inputId = "lyey", choices = lye_n(), selected = lye_n()[2])
  })
  

  # Select the desired data frame and by default the second column for analysis
  lye_dat = reactive({
    get(input$dataChoiceLYE) |>
      select(all_of(input$lyey)) |>
      as.matrix()
  })

  # plot the time series of the data
  output$lyeTS <- renderPlotly({

    plot_dat = new_dat() # get(input$dataChoiceLYE)

    ggplot(plot_dat, aes(x = .data[[input$lyex]], y = .data[[input$lyey]])) +
      geom_line() +
      labs(title = paste0("Time series of ", input$lyey), 
           x = input$lyex) +
      theme_nonan()
  })

  # LyE calculation
    lye_result = eventReactive(input$golye, {
      
      mean_freq = meanfreq(signal = lye_dat(), samp_rate = input$lye_fs)
      
      time_delay = ami(x = lye_dat(), y = lye_dat(), L = input$lye_fs, bins = 0)
      tau = time_delay$tau[1,1]
      
      #fnn_out = false_nearest_neighbors(lye_dat(), maxDim = input$lye_maxDim, delay = input$lye_tau, 
      #                        rtol = input$lye_rtol, atol = input$lye_atol, fnn_tol = input$lye_tol)
      dim = input$lye_maxDim#fnn_out$dim
      
      psr_length = length(lye_dat()) - tau*(dim-1)
      start = 1
      stop = psr_length
      X = matrix(nrow = psr_length, ncol = dim)
      for (i in 1:dim) {
        X[,i] = lye_dat()[start:stop]
        start = start + tau
        stop = stop + tau
      }
      
      lye_rosenstein(X = lye_dat(), samp_rate = input$lye_fs, mean_freq = mean_freq, 
                     nsteps = input$lye_nsteps, regpoints = input$lye_regpoints)
      
    })

  # lye plot 
  observeEvent(input$golye, {
    output$lyePlot <- renderPlot({
      plot_lye(lye_result())
    })
  }) # observeEvent
  
  # density plot
  observeEvent(input$golye, {
    output$histogram_lye <- renderPlot({
      w = ceiling(nrow(lye_dat()) * 0.03) # calculate the number of bins
      n = colnames(lye_dat())[1] # Get the column name to use below
      ggplot(as.data.frame(lye_dat()), aes(x = .data[[n]])) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = paste("Density Plot of ", input$lyey), 
             x = input$lyey) +
        theme_nonan()
    })
  }) # observeEvent
  
  # acf plot??
  observeEvent(input$golye, {
    output$autocorr_lye <-  renderPlot({
      
      a = acf(lye_dat(), plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(lye_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = paste("Autocorrelation of ", input$lyey)) + 
        theme_nonan() # add the nonan plot theme on   
      
    })
  }) # observeEvent
  
    observeEvent(input$golye, {
    output$lyeResults <- renderPrint({
      cat(input$lyey, "was analyzed", "\n",
          "LyE Value:", lye_result()$lye[2])
    })
  })
  

  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportlye, {
    export_dat = append(lye_result(), list("time_series" = input$lyey))
    
    assign(input$exportLYEname, export_dat, envir = globalenv())
    
    output$lyeResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent

  
  # Simulations --------------------------------------------------------------
  ## FGN SIM -----------------------------------------------------------------
  
  # FGN simulation
  fgn_result = eventReactive(input$goFGN, {
    fgn_sim(n = input$fgn_n, H = input$fgn_h, mean = input$fgn_mu, std = input$fgn_sigma)
  })
  
  # Turn into a data frame for plotting
  plot_dat <- eventReactive(input$goFGN, {
    data.frame("index" = 1:nrow(fgn_result()), "amp" = fgn_result())
  })
  
  # FGN plot
  observeEvent(input$goFGN, {
    output$fgnTS <- renderPlotly({
      
      # plot_dat = data.frame("index" = 1:nrow(fgn_result()), "amp" = fgn_result())
      
      ggplot(plot_dat(), aes(x = index, y = amp)) +
        geom_line() +
        labs(title = "Simulated time series") +
             # caption = paste0("Length: ", input$fgn_n, "H: ", input$fgn_h, "Mean:", input$mu, "SD: ", input$sigma)) +
        theme_nonan()
      
    })
  }) # observeEvent
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportFGN, {
    assign(input$exportFGNname, fgn_result(), envir = globalenv())
    
    output$fgnResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  # plot histogram
  observeEvent(input$goFGN, {
    output$histogram_fgn <- renderPlot({
      w = ceiling(nrow(plot_dat()) * 0.03) # calculate the number of bins
      n = colnames(plot_dat())[1] # Get the column name to use below
      ggplot(plot_dat(), aes(x = amp)) +
        geom_density(color = "black", fill = "grey40", alpha = 0.7, linewidth = 1.1) +
        labs(title = "Density Plot of FGN",
             x = "Amplitude") +
        theme_nonan()
    })
  }) # observeEvent
  
  # Autocorrelation plot -- generate the plot only when the "Go" button has been clicked
  observeEvent(input$goFGN, {
    output$autocorr_fgn <-  renderPlot({
      
      a = acf(plot_dat()$amp, plot = F)
      conf.level <- 0.95 # set this at 0.95 for 95% confidence
      ciline <- qnorm((1 - conf.level)/2)/sqrt(nrow(plot_dat())) # calculate the confidence intervals
      df = cbind.data.frame("acf" = a$acf, "lag" = a$lag) # combine the lags and acf into a dataframe for plotting
      
      ggplot(data = df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) + # lag = 0
        geom_hline(aes(yintercept = ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + # confidence intervals
        geom_hline(aes(yintercept = -ciline), linetype = "dashed", color = '#C8102E', linewidth = 0.7) + 
        geom_segment(mapping = aes(xend = lag, yend = 0), color = "black", linewidth = 3) + # lags as individual segments
        labs(title = "Autocorrelation of FGN") + 
        theme_nonan() # add the nonan plot theme on
      
    })
  }) # observeEvent
  
  # Print out the FGN results
  observeEvent(input$goFGN, {
    output$fgnResults <- renderPrint({

      cat("Hurst Exponent:", input$fgn_h, "\n",
          "Mean:", mean(fgn_result()), "\n",
          "Standard Deviation:", sd(fgn_result()))
    })
  })
  
  
  ## IAAFFT ---------------------------------------------------------------
  
  # IAAFFT data selection
  iaafft_n = reactive({
    names(get(input$dataChoiceIAAFFT))
  })
  
  
  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoiceIAAFFT, {
    updateSelectInput(inputId = "iaafftx", choices = iaafft_n())
    updateSelectInput(inputId = "iaaffty", choices = iaafft_n(), selected = iaafft_n()[2])
  })
  
  
  # Select the desired data frame and by default the second column for analysis
  iaafft_dat = reactive({
    get(input$dataChoiceIAAFFT) |>
      select(all_of(input$iaaffty)) |>
      as.matrix()
  })
  

  # IAAFFT simulation
  iaafft_result = eventReactive(input$goIAAFFT, {
    iaafft(signal = iaafft_dat(), N = input$iaafft_surr)
  })
  
  # plot the time series of the data
  # Electing to not plot this now as the original is included in the plot_iaafft function
  # output$iaafftPlot <- renderPlotly({
  #   
  #   plot_dat = get(input$dataChoiceIAAFFT)
  #   
  #   ggplot(plot_dat, aes(x = 1:nrow(plot_dat), y = .data[[input$iaaffty]])) +
  #     geom_line() +
  #     labs(title = paste0("Time series of ", input$iaaffty), 
  #          x = "Index") + 
  #     theme_nonan()
  #   
  # })
  
  # IAAFFT plot
  observeEvent(input$goIAAFFT, {
    output$iaafftPlot <- renderPlot({
      plot_iaafft(iaafft_dat(), iaafft_result())
    })
      output$iaafftResults <- renderPrint({
        cat("To view surrogate data, select the Export button.")
    })
  }) # observeEvent
  
  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportIAAFFT, {
    assign(input$exportSIMname, iaafft_result(), envir = globalenv())
    
    output$iaafftResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
  ## Pseudoperiodic ---------------------------------------------------------------
  
  # Pseudoperiodic data selection
  pseudo_n = reactive({
    names(get(input$dataChoicePseudo))
  })


  # Update x and y choices based on the selected dataframe
  observeEvent(input$dataChoicePseudo, {
    updateSelectInput(inputId = "pseudox", choices = pseudo_n())
    updateSelectInput(inputId = "pseudoy", choices = pseudo_n(), selected = pseudo_n()[2])
  })


  # Select the desired data frame and by default the second column for analysis
  pseudo_dat = reactive({
    get(input$dataChoicePseudo) |>
      select(all_of(input$pseudoy)) |>
      as.matrix()
  })


  # IAAFFT simulation
  pseudo_result = eventReactive(input$goPseudo, {
    Surr_PseudoPeriodic(pseudo_dat(), input$pseudo_tau, input$pseudo_dim, input$pseudo_rho)
  })

  # plot the time series of the data
  # Electing to not plot this now as the original is included in the plot_pseudo function
  # output$pseudoPlot <- renderPlotly({
  #
  #   plot_dat = get(input$dataChoicePseudo)
  #
  #   ggplot(plot_dat, aes(x = 1:nrow(plot_dat), y = .data[[input$pseudoy]])) +
  #     geom_line() +
  #     labs(title = paste0("Time series of ", input$pseudoy),
  #          x = "Index") +
  #     theme_nonan()
  #
  # })

  # Pseudoperiodic plot
  observeEvent(input$goPseudo, {
    output$pseudoPlot <- renderPlot({
      plot_Surr_PseudoPeriodic(pseudo_dat(), pseudo_result()[1])
    })
    output$pseudoResults <- renderPrint({
      cat("To view surrogate data, select the Export button.")
    })
  }) # observeEvent

  # Export results -- only when the "Export" button has been clicked. This appears in the environment once the app is closed.
  observeEvent(input$exportPseudo, {
    assign(input$exportPPname, pseudo_result(), envir = globalenv())

    output$pseudoResults <- renderPrint({
      cat("Exported to global environment. Close the app to view.")
    }) # renderPrint
  }) # observeEvent
  
} # server

# Run the application 
shinyApp(ui = ui, server = server)
