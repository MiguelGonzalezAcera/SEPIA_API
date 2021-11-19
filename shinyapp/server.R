box::use(
  shiny[...],
  ggplot2[...],
  RMariaDB[...],
  . / entities[fullExp,singleExp,geneLabels],
)

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

# function to query an experiment
queryExperiment <- function(tabname, genename) {
  # Establish the connection to the database
  dbconnection <- dbConnect(RMariaDB::MariaDB(), user='root', password="Plater1a", dbname='RNAseq', host='localhost')
  
  # create the query dor the rnaseq db 
  queryRNAText <- sprintf("select * from %s;", tabname)
  
  # Run the wuery for the data
  rsRNAFCInsert <- dbSendQuery(dbconnection, queryRNAText)
  dbRNARows<-dbFetch(rsRNAFCInsert)

  # filter by genename(s)
  dbRNARows <- subset(dbRNARows, dbRNARows$Genes %in% genename)
  
  # Clear query
  dbClearResult(rsRNAFCInsert)
  
  # Disconnect from database
  dbDisconnect(dbconnection)
  
  # Return the table
  return(dbRNARows)
}

# Function for asking a single table. This is shit code design, but that's what we are doing today
preprocDCdataSingle <- function(project, genename) {
  # Ask the database for the table
  dbRNARowsFilt <- queryExperiment(project, genename)
  
  # Add name of the project
  dbRNARowsFilt['Comparison'] <- c(project)
  
  # Return processed dataframe
  return(dbRNARowsFilt)
}

# Function for the fold change data
preprocFCdata <- function(dbProjRows, genename) {
  # Get a dataframe with the columns of the fold change of all the samples
  clust_df <- NULL
  
  # Loop to get the sets of data we need for the display
  for (i in c(1:nrow(dbProjRows))) {
    # Run the query database function
    dbRNARows <- queryExperiment(dbProjRows[['Comparison']][i], genename)
    
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

  # Return processed dataframe
  return(clust_df)
}

# Function for the counts data on a single experiment
preprocCountsData <- function(dbProjRows, dbDesRows, genename) {
  # Create the vector for containment
  meanList = list()
  errorList = list()
  
  # Select the particular controls and samples IDs
  
  # Loop to get the sets of data we need for the display
  for (i in c(1:nrow(dbProjRows))) {
    # Run the query database function
    dbRNARows <- queryExperiment(dbProjRows[['Comparison']][i], genename)
    
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
  
  # Return processed dataframe
  return(countsDf)
}

# Function for the counts data
preprocCountsDataSingle <- function(project, dbDesRows, genename) {
  # Run the query database function
  dbRNARows <- queryExperiment(project, genename)
  
  # Transform into pure numeric dataframe
  wdb <- dbRNARows[-which(names(dbRNARows) %in% c('id','EnsGenes','baseMean','log2FoldChange','lfcSE','stat','pvalue','padj','Genes'))]
  rownames(wdb) <- dbRNARows[['Genes']]
  wdb <- as.data.frame(t(wdb))
  wdb$Sample <- rownames(wdb)
  
  # Merge left the dataframe with the design table
  mdb <- merge(x=wdb, y=dbDesRows, by='Sample', all.x=TRUE)
  mdb <- mdb[c('Treatment', genename)]
  
  # Initialize final df
  fdf <- data.frame()
  # Loop through genes to give them the proper format
  for (gene in genename) {
    # Add genename column to df
    wmdb <- mdb[c('Treatment', gene)]
    wmdb['genename'] <- gene
    colnames(wmdb) <- c('Treatment','Counts','Genename')
    # Init if empty
    if (dim(fdf)[2] == 0) {
      fdf <- wmdb
    } else {
      fdf <- rbind(fdf, wmdb)
    }
  }
  
  # Return processed dataframe
  return(fdf)
}

# create the function to obtain both tables given a generate and a project
preprocessing <- function(project, genename) {
  # Choose between single experiment and complete sequentiations
  if (project %in% names(fullExp)){
    # Get data from the project
    dbProjRows <- projectPreproc(fullExp[[project]])
    
    # Get data from the project design
    dbDesRows <- designPreproc(fullExp[[project]])
    
    # Get the fold change data
    clust_df <- preprocFCdata(dbProjRows, genename)
    
    # Get the fold change data
    countsDf <- preprocCountsData(dbProjRows, dbDesRows, genename)
  } else if (project %in% names(singleExp)) {
    # Get data from the project
    dbProjRows <- projectPreproc(singleExp[[project]][['project']])

    # Get data from the project design
    dbDesRows <- designPreproc(singleExp[[project]][['project']])
    
    # Get the table of asking
    clust_df <- preprocDCdataSingle(singleExp[[project]][['tabid']], genename)

    # Get the fold change data
    countsDf <- preprocCountsDataSingle(singleExp[[project]][['tabid']], dbDesRows, genename)
  }
  
  # Attach both dataframes on a named list for returning
  preprocResult = list(foldChangeData = clust_df,
                       countsData = countsDf)
  
  # Return result
  return(preprocResult)
}

# Define server logic
shinyServer(function(input, output, session) {
  thematic::thematic_shiny()
  
  # Update gene selector with existing labels
  updateSelectizeInput(
    session,
    "genename",
    choices = geneLabels$mouse_genes,
    selected = c("S100a8","Vil1"),
    server = TRUE
  )
  
  # Preprocess the data
  preprocResultInput <- reactive({
    req(input$genename)
    preprocessing(input$project, input$genename)
  })
  
  # Render title of counts plot
  output$resultTitleCounts <- renderText({
    req(input$genename)
    sprintf('Counts of the selected genes in %s model', input$project)
  })
  
  # Make dimensions for the plot
  plot_dimensions <- reactive({
    list(
      heigth = max(300, ifelse(length(input$genename) %% 3 == 0, 300*(trunc(length(input$genename)/3)), 300*(1+trunc(length(input$genename)/3)))),
      width = ifelse(length(input$genename) <= 1, 300, ifelse(length(input$genename) <= 2, 600, 900))
    )
  })
  
  # Create and render barplot for counts
  output$countsPlot <- renderPlot({
    req(input$genename)
    if (input$timecourse) {
      ggplot(preprocResultInput()[['countsData']]) +
        geom_line(aes(x=sample, y=means, group=1), color="red") +
        geom_point(aes(x=sample, y=means)) +
        geom_errorbar(aes(x=sample, ymin=errorInf, ymax=errorSup), width=0.4, colour="orange")
    }else{
      ggplot(preprocResultInput()[['countsData']], aes(x=Treatment, y=Counts))+
        geom_boxplot()+
        facet_wrap(~Genename, scales="free_y", ncol=3)
      # ggplot(preprocResultInput()[['countsData']]) +
      #   geom_bar(aes(x=sample, y=means), stat="identity", fill="skyblue") +
      #   geom_errorbar(aes(x=sample, ymin=errorInf, ymax=errorSup), width=0.4, colour="orange") +
      #   ylim(0,NA) + 
      #   coord_flip()
    }
  })
  
  # Wrap in ui for dynamism
  output$countsPlot_ui <- renderUI({
    plotOutput("countsPlot", height = plot_dimensions()$heigth, width = plot_dimensions()$width)
  })
  
  # Render the title
  output$resultTitle <- renderText({
    req(input$genename)
    sprintf('Fold change of the selected genes in %s model', input$project)
  })

  # Render fold change table
  output$FCtable <- renderTable({
    req(input$genename)
    preprocResultInput()[['foldChangeData']][c('Comparison','Genes','log2FoldChange','pvalue','padj')]
  })
})