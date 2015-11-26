# This is a sample application for Keboola App development
# It can be used as a template to create your own Keboola application
# For help creating shiny applications, please see the documentation at http://shiny.rstudio.com

# We need to load the shared library on the UI side 
# because it contains keboolaPage and keboolaModal methods
library("keboola.shiny.lib")
library("networkD3")
library("plotly")

shinyUI(
    # keboolaPage - displays a keboola application and takes care of authentication/login 
    #               and provides consistent header with optional toolbar.
    # @param - page - page element.  This is your main display.  
    #               Can be pageWithSidebar, bootsrtapPage, fluidPage, or anything that shiny supports
    # @param - appTitle - optional title for the app.  Is displayed in the top-right of the page header.
    keboolaPage(
        fluidPage(
            
            #make an empty column on the left as a margin (just for fun!) 
            column(1,div()), 
            
            # There are 12 columns to a row, so with this column at 10, it will be nicely centered.
            column(10,              
                
                # Our demo will be inside a few tabs...
                tabsetPanel(
                   
                    #the first tab will be a brief intro
                    tabPanel("Introduction",
                        h1("Welcome to the Keboola sample application!"),
                        div(
                            p("This is an example of generated content within the server.R file."),
                            p("Shiny uses bootstrap, so you can use it's classes/components if you like."),
                            div("For help with bootstrap, look here:", a("bootstrap", href="http://getbootstrap.com")),
                            div("This is a success alert, for example", class="alert alert-success"),
                            
                            # custom js and css example
                            div("Feel free to use any custom javascript and css also"),
                            p("Can we add custom css and javasript?", class="clickable", id="js_example")
                        )    
                   ),
                   
                   # here we have the data in a simple table on the right, and some filters on the left
                   tabPanel("For Example",
                            
                        # Here we'll have a table selection element and that is the table we'll use throughout
                        # We'll update the choices for this element on the server side, after the data loads
                        fluidRow(
                            column(7, h4("This is a basic example of how outputs are dependent on inputs.")),
                            column(5, selectInput("table","Choose a table from the bucket to analyse",choices=c()))
                        ),
                        
                        # In this tab we'll set up a sidebar/main style layout  
                        fluidRow(
                            column(3, # sidebar
                                h5("Dynamic Filters"),
                                helpText("Note, this app isn't doing any type checking, 
                                         so if you choose a column with a data type that doesn't match the input, things may break."),
                                wellPanel(
                                    # all these 
                                    selectInput("rangeCols","Numeric Ranges",choices=c(), multiple=TRUE),
                                    
                                    # this will hold a selection of range selectors depending on the chosen rangeCols.
                                    uiOutput("rangeElementsUI")
                                ),
                                helpText("Go ahead and choose a different table at the top!  Everything should update accordingly")
                            ),
                            column(8,
                                tabsetPanel(
                                    tabPanel("Histograms",
                                        plotOutput('histPlot'),
                                        fluidRow(
                                            column(6,
                                                selectInput("histCol", "Column To Plot", choices=c())   
                                            ),
                                            column(6,
                                                sliderInput("bins",
                                                        "Number of bins:",
                                                        min = 1,
                                                        max = 100,
                                                        value = 30
                                                )
                                            )
                                        ),
                                        fluidRow(
                                            column(6,div(
                                                "Mean:", textOutput("varmean")    
                                            )),
                                            column(6,div(
                                                "Standard Deviation:", textOutput("varsd")    
                                            ))
                                        )
                                    ),
                                    tabPanel("Data Table",
                                             dataTableOutput("sampleTable")         
                                    )
                                ) 
                            )
                        )    
                    ),
                   tabPanel("D3 Examples",
                        sankeyNetworkOutput("sankey")),
                   tabPanel("Plot.ly Examples",
                        plotlyOutput("trendPlot"),
                        sliderInput("moviebins", "Number of bins:", min = 1, max = 50, value = 10),
                        plotlyOutput("boxPlot"),
                        plotlyOutput("volcano")
                    )
                ),
               
                # here we add our custom css and javascript to the HTML head
                tags$head(tags$script(src="example.js"),
                         tags$link(href="example.css", rel="stylesheet"))
            )
        ),
        
        # our application title
        appTitle = "Sample"
    )
)