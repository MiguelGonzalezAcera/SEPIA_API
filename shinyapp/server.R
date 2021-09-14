suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(datasets))
suppressPackageStartupMessages(library(RMariaDB))
suppressPackageStartupMessages(library(ggplot2))

# Function for query project instances and comparisons
projectPreproc <- function(project) {
  # Establish the connection to the projects database
  projectsDb <- dbConnect(RMariaDB::MariaDB(), user='root', password="Plater1a", dbname='Projects', host='localhost')
  
  # Create the query with the project name
  queryProjText <- sprintf("select * from %s;", project)

  # Run the query against the database and fetch the resulting dataframe
  rsProjInsert <- dbSendQuery(projectsDb, queryProjText)
  dbProjRows <- dbFetch(rsProjInsert)
  
  # Clear the query
  dbClearResult(rsProjInsert)
  
  # Disconnect the database
  dbDisconnect(projectsDb)
  
  # return projects dataframe
  return(dbProjRows)
}

# Function for query design of project
designPreproc <- function(project) {
  # Establish the connection to the projects database
  designDb <- dbConnect(RMariaDB::MariaDB(), user='root', password="Plater1a", dbname='Designs', host='localhost')
  
  # Create the query with the project name
  queryDesText <- sprintf("select * from %s;", project)
  
  # Run the query against the database and fetch the resulting dataframe
  rsDesInsert <- dbSendQuery(designDb, queryDesText)
  dbDesRows <- dbFetch(rsDesInsert)
  
  # Clear the query
  dbClearResult(rsDesInsert)
  
  # Disconnect the database
  dbDisconnect(designDb)
  
  # return projects dataframe
  return(dbDesRows)
}

# Function for the fold change data
preprocFCdata <- function(dbProjRows, genename) {
  # Connect to RNAseq database
  rnaseqDbFC <- dbConnect(RMariaDB::MariaDB(), user='root', password="Plater1a", dbname='RNAseq', host='localhost')
  
  # Get a dataframe with the columns of the fold change of all the samples
  clust_df <- NULL
  
  # Loop to get the sets of data we need for the display
  for (i in c(1:nrow(dbProjRows))) {
    # create the query dor the rnaseq db 
    queryRNAText <- sprintf("select * from %s where Genes='%s';", dbProjRows[['Comparison']][i], genename)
    
    # Run the wuery for the data
    rsRNAFCInsert <- dbSendQuery(rnaseqDbFC, queryRNAText)
    dbRNARows<-dbFetch(rsRNAFCInsert)
    
    # Clear query
    dbClearResult(rsRNAFCInsert)
    
    # Filter the columns of the differential expression data
    dbRNARowsFilt <- dbRNARows[c('id','EnsGenes','baseMean','log2FoldChange','lfcSE','stat','pvalue','padj','Genes')]
    
    # Add comparison name
    dbRNARowsFilt['Comparison'] <- c(paste(dbProjRows[['Sample']][i], dbProjRows[['Control']][i], sep=' v '))
    
    # Add data to clust_df
    if (is.null(clust_df) == T){
      # If the final df is empty, fill it with one column
      clust_df <- dbRNARowsFilt
    } else {
      # If not, add the column to the df
      clust_df <- rbind(clust_df, dbRNARowsFilt)
    }
  }
  
  # Add a condition to clust_df for the color
  clust_df$colorPlot <- ifelse(clust_df$log2FoldChange > 0, 'Upreg', 'Downreg')
  
  # fix names as a factor
  clust_df$Comparison <- factor(clust_df$Comparison, levels = clust_df$Comparison)
  
  # Disconnect from database
  dbDisconnect(rnaseqDbFC)

  # Return processed dataframe
  return(clust_df)
}

# Function for the counts data
preprocCountsData <- function(dbProjRows, dbDesRows, genename) {
  # create the query dor the rnaseq db
  rnaseqDbCounts <- dbConnect(RMariaDB::MariaDB(), user='root', password="Plater1a", dbname='RNAseq', host='localhost')
  
  # Create the vector for containment
  meanList = list()
  errorList = list()
  
  # Loop to get the sets of data we need for the display
  for (i in c(1:nrow(dbProjRows))) {
    # create the query dor the rnaseq db 
    queryRNAText <- sprintf("select * from %s where Genes='%s';", dbProjRows[['Comparison']][i], genename)
    
    # Run the wuery for the data
    rsRNACountsInsert <- dbSendQuery(rnaseqDbCounts, queryRNAText)
    dbRNARows<-dbFetch(rsRNACountsInsert)
    
    # Clear query
    dbClearResult(rsRNACountsInsert)
    
    # Get data for control
    controlRows <- dbRNARows[,dbDesRows[dbDesRows$Treatment == dbProjRows[['Control']][i],][['Sample']]]
    
    # Compute meam and std
    meanList[[dbProjRows[['Control']][i]]] = mean(as.list(as.data.frame(t(controlRows)))[[1]])
    errorList[[dbProjRows[['Control']][i]]] = sd(as.list(as.data.frame(t(controlRows)))[[1]])
    
    # Get data for sample
    controlRows <- dbRNARows[,dbDesRows[dbDesRows$Treatment == dbProjRows[['Sample']][i],][['Sample']]]
    
    # Compute meam and std
    meanList[[dbProjRows[['Sample']][i]]] = mean(as.list(as.data.frame(t(controlRows)))[[1]])
    errorList[[dbProjRows[['Sample']][i]]] = sd(as.list(as.data.frame(t(controlRows)))[[1]])
  }
  
  # Fix mean and std lists as dataframe
  countsDf <- data.frame(
    sample = factor(names(meanList), levels=names(meanList)),
    means = unlist(meanList, use.names = FALSE),
    error = unlist(errorList, use.names = FALSE),
    errorSup = unlist(meanList, use.names = FALSE) + unlist(errorList, use.names = FALSE),
    errorInf = unlist(meanList, use.names = FALSE) - unlist(errorList, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  
  # Reassign negative errors to 0
  countsDf$errorInf[countsDf$errorInf<0] <- 0
  
  # Disconnect from database
  dbDisconnect(rnaseqDbCounts)

  # Return processed dataframe
  return(countsDf)
}

# create the function to obtain both tables given a generate and a project
preprocessing <- function(project, genename) {
  # Get data from the project
  dbProjRows <- projectPreproc(project)
  
  # Get data from the project design
  dbDesRows <- designPreproc(project)
  
  # Get the fold change data
  clust_df <- preprocFCdata(dbProjRows, genename)
  
  # Get the fold change data
  countsDf <- preprocCountsData(dbProjRows, dbDesRows, genename)

  # Attach both dataframes on a named list for returning
  preprocResult = list(foldChangeData = clust_df,
                       countsData = countsDf)
  
  # Return result
  return(preprocResult)
}

# Define server logic
shinyServer(function(input, output) {
  thematic::thematic_shiny()
  
  preprocResultInput <- reactive({
    preprocessing(input$project, input$genename)
  })
  
  # Render the title
  output$resultTitle <- renderText({
    sprintf('Fold change of %s in %s model', input$genename, input$project)
  })
  
  # Create and render barplot for fold change
  output$FCplot <- renderPlot({
    FCdata <- preprocResultInput()[['foldChangeData']]
    
    # Select the max and min y value of the plot
    if (max(FCdata$log2FoldChange) > 5) {
      ymax <- max(FCdata$log2FoldChange) + 1
    }else{
      ymax <- 5
    }
    
    if (min(FCdata$log2FoldChange) < -5) {
      ymin <- min(FCdata$log2FoldChange) - 1
    }else{
      ymin <- -5 
    }
    
    # run the plot
    ggplot(FCdata) +
    geom_bar( aes(x=Comparison, y=log2FoldChange, fill = colorPlot), stat="identity") +
    scale_fill_manual(values = c("Upreg" = "red",
                                 "Downreg" = "blue")) +
    ylim(ymin, ymax) +
    coord_flip()
  },
  height = 400, width = 600)

  # Render fold change table
  output$FCtable <- renderTable({
    preprocResultInput()[['foldChangeData']][c('Comparison','log2FoldChange','pvalue','padj')]
  })
  
  # Render title of counts plot
  output$resultTitleCounts <- renderText({
    sprintf('Counts of %s in %s model', input$genename, input$project)
  })
  
  # Create and render barplot for counts
  output$countsPlot <- renderPlot({
    if (input$timecourse) {
      ggplot(preprocResultInput()[['countsData']]) +
        geom_line(aes(x=sample, y=means, group=1), color="red") +
        geom_point(aes(x=sample, y=means)) +
        geom_errorbar(aes(x=sample, ymin=errorInf, ymax=errorSup), width=0.4, colour="orange")
    }else{
      ggplot(preprocResultInput()[['countsData']]) +
        geom_bar(aes(x=sample, y=means), stat="identity", fill="skyblue") +
        geom_errorbar(aes(x=sample, ymin=errorInf, ymax=errorSup), width=0.4, colour="orange") +
        ylim(0,NA) + 
        coord_flip()
    }
  },
  height = 400, width = 600)
})