# Framework application for keboola applications. This 
# code should be filled with whatever application logic you like.

# Load libraries
library(shiny)
library(networkD3)
library(plotly)
library(data.table)

# connection to keboola, use what you like.  
# https://github.com/keboola/shiny-lib
library(keboola.shiny.lib)

shinyServer(function(input, output, session) {
    
    # Create instance of keboola/shiny-lib
    klib <- KeboolaShiny$new()
    
    # keboola will hold 
    keboola <- reactive({
        
        # start it up
        ret <- klib$startup(
                             list(appTitle = "Application Title",
                                  tables = NULL,
                                  dataToSave = NULL,
                                  configCallback = NULL,
                                  description = FALSE,
                                  customElements = NULL))
        
        #return login info list so the rest of the app can use it if desired.
        return(ret$loginInfo)
    })
    
    # This observe method will run if anything inside the keboola() reactive changes.  
    # That will only happen when the library is fully loaded and the authentication complete.
    # For this reason, this is a good place to trigger pulling the data and updating input elements.
    #
    # We'll use the sapi-r-client (https://github.com/keboola/sapi-r-client) to grab a list of tables in our bucket
    # Note that we'll need to check the value of keboola()$ready to make sure we're logged in and the page is loaded
    # If we're not ready, we'll do nothing and return NULL
    observe({
        if (keboola()$ready) {
            
            # grab a list of all table objects (meta data about the table, not the actual data)
            tables <- klib$client$listTables(bucket = klib$bucket)
            
            # grab the name of each table object
            tableNames <- lapply(tables, function(t) { t$name })
            
            #update our select input element
            updateSelectInput(session,"table",choices=c(tableNames))
            
        } else {
            print("server.R not ready")
            NULL
        }
    })
    
    # Every time the chosen table changes, we'll import it into our session from SAPI
    # By default the loadTables function will display a progress bar while downloading data
    # Once we have the data, we can update our rangeCols selector with the column names
    sourceData <- reactive({
        if (input$table != "") {
            
            # this library method returns
            klib$loadTable("sd",input$table)
            
            table <- klib$sourceData()()$sd
            
            print(head(table))
            # update the choices
            updateSelectInput(session,"rangeCols", choices=names(table))
            updateSelectInput(session,"histCol", choices=names(table))
            
            # return the table
            table
        } else {
            NULL
        }
    })
    
    # any time filter inputs chnage, this will run and return reflected changes
    filteredData <- reactive({
        sd <- sourceData()
        if (!is.null(sd) && length(input$rangeCols) > 0) {
            
            # loop over our chosen range inputs
            for (i in 1:length(input$rangeCols)) {
                rangeElem <- input$rangeCols[i]
                
                # we need to set the columns as numeric.
                # careful, if the column is not really numeric, this line will shout at us.
                sd[[rangeElem]] <- as.numeric(sd[[rangeElem]])
                
                # filter our dataset
                sd <- sd[
                            which(
                                sd[,rangeElem] > input[[rangeElem]][1] &
                                sd[,rangeElem] < input[[rangeElem]][2]
                            ), 
                            # leaving the second argument empty like this means all columns will be selected
                        ]
            }
        } else {
            # no range filters to use
        }
        # return the dataset
        sd
    })
    
    # dynamically create range selectors for the chosen columns
    # the data returned by filteredData is filtered by these inputs
    # filteredData() is then used by our plot and table output elements
    output$rangeElementsUI <- renderUI({ 
        if (length(input$rangeCols) > 0) {
            
            # get our chosen table (selected by the top select input)
            sd <- sourceData()
            
            # this function creates a list of sliderInputs with name equal to the selected column
            # Note, we aren't capturing this in a variable 
            # and there are no further statements so this is our return value
            lapply(seq_along(input$rangeCols), function(x) {
                # here is the numeric assumption
                colData <- as.numeric(sd[,input$rangeCols[x]])
                
                minval <- min(colData)
                maxval <- max(colData)
                sliderInput(input$rangeCols[x], input$rangeCols[x],
                            min = minval,
                            max = maxval,
                            value = c(minval,maxval))
            })
        } else {
            helpText("Go ahead and choose a numeric column.")
        } 
    })
    
    # Render a table, this methods shows an example of a reactive element
    output$sampleTable <- renderDataTable({
        
        # the table will only list our filtered data
        filteredData()
    })
    
    # [CHANGE] Plot histogram, this methods shows an example of a reactive element
    output$histPlot <- renderPlot({
        
        # Get our filtered data
        dataSet <- filteredData()
        
        # get our selected column
        x    <- as.numeric(dataSet[,input$histCol])
        
        bins <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, prob=TRUE, breaks = bins, col = 'lightblue', border = 'white', main=paste("Histogram of", input$histCol))
        
        # lets add a density line as it can help visualize peaks and valleys
        lines(density(x, na.rm=TRUE), col="darkblue", lwd=2) # add a density estimate with defaults
    })
    
    output$sankey <- renderSankeyNetwork({
        URL <- paste0("https://cdn.rawgit.com/christophergandrud/networkD3/",
                      "master/JSONdata/energy.json")
        Energy <- jsonlite::fromJSON(URL)
        
        # Plot
        sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
                      Target = "target", Value = "value", NodeID = "name",
                      fontSize = 12, nodeWidth = 30)
    })
    
    output$varmean <- renderText({
        mean(as.numeric(filteredData()[,input$histCol]))
    })
    output$varsd <- renderText({
        sd(as.numeric(filteredData()[,input$histCol]))
    })
    
    # This is the plot.ly histogram
    output$trendPlot <- renderPlotly({
        data(movies, package="ggplot2")
        minx <- min(movies$rating)
        maxx <- max(movies$rating)
        
        # size of the bins depend on the input 'bins'
        size <- (maxx - minx) / input$moviebins
        
        # a simple histogram of movie ratings
        p <- plot_ly(movies, x = rating, autobinx = F, type = "histogram",
                     xbins = list(start = minx, end = maxx, size = size))
        # style the xaxis
        layout(p, xaxis = list(title = "Ratings", range = c(minx, maxx), autorange = F,
                               autotick = F, tick0 = minx, dtick = size))
    })
    
    # This is a plotly box plot
    output$boxPlot <- renderPlotly({
        plot_ly(midwest, x = percollege, color = state, type = "box")
    })
    
    # Fancy 3D surface plot
    output$volcano <- renderPlotly({
        plot_ly(z = volcano, type = "surface")
    })
    
    output$pplot <- renderPlotly({
        plot_ly(zup, x=input$xcol, y=input$ycol)
    })
})
