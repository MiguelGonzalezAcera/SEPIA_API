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

# function to query an experiment
queryExperiment <- function(tabname, genename) {
  # Establish the connection to the database
  dbconnection <- dbConnect(RMariaDB::MariaDB(), user='root', password="Plater1a", dbname='RNAseq', host='localhost')
  
  # create the query dor the rnaseq db 
  queryRNAText <- sprintf("select * from %s where Genes='%s';", tabname, genename)
  
  # Run the wuery for the data
  rsRNAFCInsert <- dbSendQuery(dbconnection, queryRNAText)
  dbRNARows<-dbFetch(rsRNAFCInsert)
  
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
    dbRNARowsFilt <- queryExperiment(dbProjRows[['Comparison']][i], genename)
    
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
preprocCountsDataSingle <- function(dbProjRows, dbDesRows, genename) {
  # Create the vector for containment
  meanList = list()
  errorList = list()
  
  # Run the query database function
  dbRNARows <- queryExperiment(dbProjRows[['Comparison']][1], genename)
  
  # Get data for control
  controlRows <- dbRNARows[,dbDesRows[dbDesRows$Treatment == dbProjRows[['Control']][1],][['Sample']]]
  
  # Compute meam and std
  meanList[[dbProjRows[['Control']][1]]] = mean(as.list(as.data.frame(t(controlRows)))[[1]])
  errorList[[dbProjRows[['Control']][1]]] = sd(as.list(as.data.frame(t(controlRows)))[[1]])
  
  # Get data for sample
  controlRows <- dbRNARows[,dbDesRows[dbDesRows$Treatment == dbProjRows[['Sample']][1],][['Sample']]]
  
  # Compute meam and std
  meanList[[dbProjRows[['Sample']][1]]] = mean(as.list(as.data.frame(t(controlRows)))[[1]])
  errorList[[dbProjRows[['Sample']][1]]] = sd(as.list(as.data.frame(t(controlRows)))[[1]])
  
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

# create the function to obtain both tables given a generate and a project
preprocessing <- function(project, genename) {
  # Generate lists of options for the displays
  fullExp <- list(
    "DSSTC" = "DSS_TimeCourse",
    "WHTC" = "WoundHealing"
  )
  
  singleExp <- list(
    "AcDSS" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_DSS_Cerl"),
    "cDSS" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_cDSS_Cerl"),
    "OxC" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_OxC_Cerl"),
    "TC" = list("project" = "MouseModelsInflammation", "tabid" = "MouseModelsInflammation_TC_RKO"),
    "TdAc" = list("project" = "TNFdARE_model", "tabid" = "TNFdARE_model_Col_dARE_Col_WT"),
    "TdAi" = list("project" = "TNFdARE_model", "tabid" = "TNFdARE_model_SI_dARE_SI_WT"),
    "AcTNBS" = list("project" = "TNBS_model", "tabid" = "TNBS_model_TNBS_Ac_Healthy"),
    "cTNBS" = list("project" = "TNBS_model", "tabid" = "TNBS_model_TNBS_Chr_Healthy"),
    "C8KOc" = list("project" = "Casp8Colon", "tabid" = "Casp8Colon_Col_Casp8dIEC_Col_Casp8flox"),
    "EvInf" = list("project" = "Eimeria_vermiformis_model", "tabid" = "Eimeria_vermiformis_model_EV_WT"),
    "HhInf" = list("project" = "HhColitis", "tabid" = "HhColitis_HhCol_SSt")
  )
  
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
    countsDf <- preprocCountsDataSingle(dbProjRows[dbProjRows$Comparison == singleExp[[project]][['tabid']],], dbDesRows, genename)
  }

  # Attach both dataframes on a named list for returning
  preprocResult = list(foldChangeData = clust_df,
                       countsData = countsDf)
  
  # Return result
  return(preprocResult)
}

# Define server logic
shinyServer(function(input, output) {
  thematic::thematic_shiny()
  
  # Add default text for the page. It will disappear once we ask for a gene
  output$defaultText <- renderText({
    if (input$genename == "") {
      "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. Ph'orr'e k'yarnak shtunggli stell'bsna n'ghaagl uln fhtagn, mnahn'og hupadgh ron gnaiih kadishtu ch' n'ghaoth Dagonor gof'nn ph'ehye. Nnngotha nghrii nog cn'gha s'uhn ilyaa Shub-Niggurath ah Hastur y'hahyar, nglui athg n'gha mnahn' hupadgh Nyarlathotep y-vulgtlagln hrii zhronyth Azathothyar, ftaghu nnnChaugnar Faugn f'Azathoth kn'a tharanak nw ah gotha. Ep f'mg k'yarnak ebunma lw'nafh fhtagn nnnehye 'bthnk grah'n naflzhro chtenff lw'nafh phlegeth lloig ronog, ftaghu nw goka shtunggli hafh'drn hai shugg nwoth Nyarlathotep y-r'luh ooboshu chtenff. Ph'Nyarlathotep gof'nn vulgtm ph'gotha Dagon ph'Cthulhu ah Azathoth, goka hafh'drn h'bug ooboshu h'Shub-Niggurath ya ph'ooboshu bug, s'uhn shogg stell'bsna fm'latgh athg li'hee."
    }
  })
  
  # Preprocess the data
  preprocResultInput <- reactive({
    req(input$genename)
    preprocessing(input$project, input$genename)
  })
  
  # Render title of counts plot
  output$resultTitleCounts <- renderText({
    req(input$genename)
    sprintf('Counts of %s in %s model', input$genename, input$project)
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
      ggplot(preprocResultInput()[['countsData']]) +
        geom_bar(aes(x=sample, y=means), stat="identity", fill="skyblue") +
        geom_errorbar(aes(x=sample, ymin=errorInf, ymax=errorSup), width=0.4, colour="orange") +
        ylim(0,NA) + 
        coord_flip()
    }
  },
  height = 400, width = 600)
  
  # Render the title
  output$resultTitle <- renderText({
    req(input$genename)
    sprintf('Fold change of %s in %s model', input$genename, input$project)
  })

  # Render fold change table
  output$FCtable <- renderTable({
    req(input$genename)
    preprocResultInput()[['foldChangeData']][c('Genes','log2FoldChange','pvalue','padj')]
  })
})