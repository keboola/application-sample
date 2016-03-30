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
            fluidRow(
                column(3, # sidebar
                       # Here we'll have a table selection element and that is the table we'll use throughout
                       # We'll update the choices for this element on the server side, after the data loads
                       selectInput("table","Choose a table to analyse",choices=c()),
                       
                       h5("Dynamic Filters"),
                       helpText("Note, this app isn't doing any type checking.  Choosing improper columns for the selectors may result in unexpected behaviour.."),
                       actionButton("apply","Apply Filters"),
                       wellPanel(
                           # dynamically generated element for numerical ranges
                           dynamicRangeInput("rangeCols")
                       ),
                       wellPanel(
                           # dynamically generated element for date ranges
                           dynamicDateRangeInput("dateCols")
                       ),
                       wellPanel(
                           # dynamically generated element for factor (ie. categorical) value selection
                           dynamicFactorInput("factorCols")
                       )
                ),
                column(8,
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
                                    ),
                                    helpText("Choosing a different table at the top should update accordingly.")
                           ),
                           
                           # here we have the data in a simple table on the right, and some filters on the left
                           tabPanel("Histogram",
                                # here we'll output a histogram plot with a select for which column, and a slider for the # of bins to use. 
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
                           tabPanel("Box Plot",
                                fluidRow(
                                    column(4, selectInput("boxX", "X Column", choices=c())),
                                    column(4, selectInput("boxY", "Y Column", choices=c())),
                                    column(4, selectInput("boxColor", "Colour", choices=c()))
                                ),
                                fluidRow(
                                    plotlyOutput("boxPlot")
                                )
                           ),
                           tabPanel("Scatter Plot",
                                fluidRow(
                                    column(3, selectInput("scatterX", "X Column", choices=c())),
                                    column(3, selectInput("scatterY", "Y Column", choices=c())),
                                    column(3, selectInput("scatterColor", "Colour", choices=c())),
                                    column(3, selectInput("scatterFacet", "Facet", choices=c()))
                                ),
                                fluidRow(
                                    column(3, checkboxInput("scatterXDate", "X is a date")),
                                    column(3, checkboxInput("scatterYDate", "Y is a date")),
                                    column(3, checkboxInput("scatterSmooth", "Apply statistical smoother")),
                                    column(3, div())
                                ),
                                fluidRow(
                                    plotlyOutput("scatterPlot")    
                                )
                           ),
                           tabPanel("Data Table",
                                dataTableOutput("sampleTable")         
                           )
                       ),
                       # here we add our custom css and javascript to the HTML head
                       tags$head(tags$script(src="example.js"),
                                 tags$link(href="example.css", rel="stylesheet"))           
                )
            )
        ),
        
        # our application title
        appTitle = "Sample"
    )
)