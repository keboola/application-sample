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
    
    # This is the startup method, it will run whenever the page is loaded
    keboola <- reactive({
        
        # here we create and register a list of our inputs
        # doing this means that when configs are loaded these inputs will be automatically updated
        appInputs <- list(
            # data filters
            list(id="table", type="select", hasDependents=TRUE),
            list(id="rangeCols", type="dynamicRanges"),
            list(id="dateCols", type="dynamicDateRanges"),
            list(id="factorCols", type="dynamicFactors"),
            #list(id="apply", type="actionButton"),
            
            # box plot inputs
            list(id="boxX", type="select"),
            list(id="boxY", type="select"),
            list(id="boxColor", type="select"),
            # scatter plot inputs
            list(id="scatterX", type="select"),
            list(id="scatterY", type="select"),
            list(id="scatterXDate", type="select"),
            list(id="scatterYDate", type="select"),
            list(id="scatterColor", type="select"),
            list(id="scatterFacet", type="select"),
            list(id="scatterSmooth", type="checkbox"),
            # histogram inputs
            list(id="histCols", type="select"),
            list(id="bins", type="slider")
            
        )
        
        # start it up
        # for a full list of options available to the startup method please see the documentation
        ret <- klib$startup(
                            list(
                                appTitle = "Application Title", 
                                tables="all", 
                                inputList = appInputs, 
                                dataToSave=filteredData,
                                forkButtonRef="https://github.com/keboola/application-sample/fork"
                                )
                            )
        
        
        
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
            # klib$loadTable("sd",input$table)
            table <- klib$sourceData()()[[input$table]]
            
            # we'll make a HACK guess that columns with fewer than 100 unique values can be treated as factors
            maybeFactors <- sapply(table,function(x) { length(unique(x)) < 100 })
            
            # update the choices to be of columns of the selected table
            # updateSelectInput(session,"rangeCols", choices=names(table))
            # updateSelectInput(session,"dateCols", choices=names(table))
            # updateSelectInput(session,"factorCols", choices=names(table[maybeFactors]))
            
            updateSelectInput(session,"histCol", choices=names(table))
            updateSelectInput(session,"boxColor", choices=c("None",names(table[maybeFactors])))
            updateSelectInput(session,"boxX", choices=names(table))
            updateSelectInput(session,"boxY", choices=names(table))
            updateSelectInput(session,"scatterColor", choices=c("None", names(table[maybeFactors])))
            updateSelectInput(session,"scatterFacet", choices=c(None = ".", names(table[maybeFactors])))
            updateSelectInput(session,"scatterY", choices=names(table))
            updateSelectInput(session,"scatterX", choices=names(table))
            # return the table
            table
        } else {
            NULL
        }
    })
    
    
    
    config <- callModule(appConfig,"kb",klib$kfig)
    f1 <- callModule(dynamicRange,"rangeCols",sourceData, config)
    f2 <- callModule(dynamicDateRange, "dateCols", f1, config)
    filteredData <- callModule(dynamicFactor,"factorCols", f2, config)

    ######################################################################
    #  The remainder of this app are output elements (graphs/tables etc...)
    
    # Render a table, this methods shows an example of a reactive element
    output$sampleTable <- renderDataTable({
        # the table will only list our filtered data
        fd <- filteredData()
        print(paste("dataTable data (fd) has ", nrow(fd), "rows"))
        fd
    })
    
    # [CHANGE] Plot histogram, this methods shows an example of a reactive element
    output$histPlot <- renderPlot({
        
        # Get our filtered data
        dataSet <- filteredData()
        
        # we can only show the graph if a column is selected.
        # it will still work without this, but ignorable errors will be thrown to the logs (and briefly on the screen).
        if (nchar(input$histCol) > 0 && length(dataSet[,input$histCol]) > 0) {
            
            x    <- suppressWarnings(as.numeric(dataSet[,input$histCol]))
            
            bins <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out = input$bins + 1)
            
            # draw the histogram with the specified number of bins
            hist(x, prob=TRUE, breaks = bins, col = 'lightblue', 
                 border = 'white', main=paste("Histogram of", input$histCol),
                 xlab=input$histCol)
            
            # lets add a density line as it can help visualize peaks and valleys
            lines(density(x, na.rm=TRUE), col="darkblue", lwd=2) # add a density estimate with defaults
            
        }
    })
    
    output$varmean <- renderText({
        if (nchar(input$histCol) > 0) {
            # only calculate if a column has been selected
            mean(as.numeric(filteredData()[,input$histCol]))
        }
    })
    output$varsd <- renderText({
        if (nchar(input$histCol) > 0) {
            # only calculate if a column has been selected
            sd(as.numeric(filteredData()[,input$histCol]))    
        }
    })
    
    # This is a plotly box plot
    output$boxPlot <- renderPlotly({
        fd <- filteredData()
        
        xaxis <- list(title=input$boxX)
        yaxis <- list(title=input$boxY)
        
        if (input$boxColor != "None" && input$boxY != "None") {
            fd[,input$boxY] <- as.numeric(fd[,input$boxY])
            
            p <- plot_ly(fd, x = fd[,input$boxX], y = fd[,input$boxY], color = fd[,input$boxColor], type = "box")                    
            p <- p %>% layout(xaxis=xaxis, yaxis=yaxis, boxmode="group")
        } else if (input$boxY != "None" && input$boxColor == "None") {
            fd[,input$boxY] <- as.numeric(fd[,input$boxY])
            p <- plot_ly(fd, x = fd[,input$boxX], y = fd[,input$boxY], type = "box")
            p <- p %>% layout(xaxis=xaxis, yaxis=yaxis, title=paste(input$boxX,"vs",input$boxY))
        } else if (input$boxY == "None" && input$color != "None") {
            p <- plot_ly(fd, x = fd[,input$boxX], color = fd[,input$boxColor], type = "box")
            p <- p %>% layout(xaxis=xaxis, yaxis=list(title=input$boxColor), title=paste(input$boxX,"by",input$boxColor))
        }
        p
    })
    
    output$scatterPlot <- renderPlotly({
        sd <- filteredData()
        if (input$scatterX == "" || input$scatterX == "None" || input$scatterY == "" || input$scatterY == "None") {
            return(NULL)
        }
        if (input$scatterXDate) {
            sd[,input$scatterX] <- as.Date(sd[,input$scatterX])
        } else {
            sd[,input$scatterX] <- as.numeric(sd[,input$scatterX])
        }
        if (input$scatterYDate) {
            sd[ ,input$scatterY] <- as.Date(sd[ ,input$scatterY])
        } else {
            sd[ ,input$scatterY] <- as.numeric(sd[ ,input$scatterY])
        }
        print(paste("trying plot with ", input$scatterX," vs ", input$scatterY))
        
        p <- ggplot(sd, aes_string(x = input$scatterX, y = input$scatterY))
        p <- p + geom_point()
        
        if (input$scatterColor != 'None' & input$scatterColor != "") {
            p <- p + aes_string(color=input$scatterColor)
        }
        facets <- paste('. ~', input$scatterFacet)
        if (facets != '. ~ .' & input$scatterFacet != "") {
            p <- p + facet_grid(facets)
        }
        
        if (input$scatterXDate) {
            p <- p + scale_x_date(labels = scales::date_format("%d. %m. %Y"))    
        }
        if (input$scatterYDate) {
            p <- p + scale_y_date(labels = scales::date_format("%d. %m. %Y"))    
        }
        if (input$scatterSmooth) {
            p <- p + stat_smooth()    
        }
        
        ggplotly(p)
    })
})
