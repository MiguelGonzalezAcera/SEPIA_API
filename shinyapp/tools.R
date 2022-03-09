box::use(
  shiny[...],
  RMariaDB[...],
  dplyr[...],
  stats[...],
  ggplot2[...],
  ggpubr[...],
  . / entities[fullExp,singleExp,displayNames],
)

#'@export
geneLabels <- function(){
  # Establish the connection to the projects database
  projectsRefDb <- dbConnect(RMariaDB::MariaDB(), user='sepia', password="sepia_TRR241", dbname='Refs', host='localhost')
  
  # Create the query with the project name
  queryRefText <- "select Genes from mouse_genes;"
  
  # Run the query against the database and fetch the resulting dataframe
  rsRefInsert <- dbSendQuery(projectsRefDb, queryRefText)
  dbRefCol <- dbFetch(rsRefInsert)
  
  # Clear the query
  dbClearResult(rsRefInsert)
  
  # Disconnect the database
  dbDisconnect(projectsRefDb)
  
  # return List of genes with a name
  geneLabelsList <- list(
    'mouse_genes' = unique(as.vector(dbRefCol[['Genes']]))
  )
  return(geneLabelsList)
}

#' @export
designPreproc <- function(project) {
  # Establish the connection to the projects database
  designDb <- dbConnect(RMariaDB::MariaDB(), user='sepia', password="sepia_TRR241", dbname='Designs', host='localhost')
  
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

#' @export
queryExperiment <- function(tabname, genename = c()) {
  # Establish the connection to the database
  dbconnection <- dbConnect(RMariaDB::MariaDB(), user='sepia', password="sepia_TRR241", dbname='RNAseq', host='localhost')
  
  # create the query dor the rnaseq db 
  queryRNAText <- sprintf("select * from %s;", tabname)
  
  # Run the wuery for the data
  rsRNAFCInsert <- dbSendQuery(dbconnection, queryRNAText)
  dbRNARows<-dbFetch(rsRNAFCInsert)

  # filter by genename(s) if genelist has genes
  if (length(genename) > 0) {
    dbRNARows <- subset(dbRNARows, dbRNARows$Genes %in% genename)
  }
  
  # Clear query
  dbClearResult(rsRNAFCInsert)
  
  # Disconnect from database
  dbDisconnect(dbconnection)
  
  # Return the table
  return(dbRNARows)
}

#' @export
preprocDCdataSingle <- function(project, genename) {
  # Ask the database for the table
  dbRNARowsFilt <- queryExperiment(project, genename)
  
  # Check if the dataframe is empty and return it directly if so
  if (dim(dbRNARowsFilt)[1] == 0) {
    # If it is empty, return it without changes. The error will be managed in the upper function.
    return(dbRNARowsFilt)
  } else {
    # Add name of the project
    dbRNARowsFilt['Comparison'] <- c(project)
    
    # Return processed dataframe
    return(dbRNARowsFilt)
  }
}

#' @export
preprocCountsDataSingle <- function(project, dbDesRows, genename) {
  # Run the query database function
  dbRNARows <- queryExperiment(project, genename)

  # Check if the dataframe is empty and return it directly if so
  if (dim(dbRNARows)[1] == 0) {
    # If it is empty, return it without changes. The error will be managed in the upper function.
    return(dbRNARows)
  } else {
    # Transform into pure numeric dataframe
    wdb <- dbRNARows[-which(names(dbRNARows) %in% c('id','EnsGenes','baseMean','log2FoldChange','lfcSE','stat','pvalue','padj','Genes','FLAG'))]
    rownames(wdb) <- dbRNARows[['Genes']]
    wdb <- as.data.frame(t(wdb))
    wdb$Sample <- rownames(wdb)

    # Merge left the dataframe with the design table
    mdb <- merge(x=wdb, y=dbDesRows, by='Sample', all.x=TRUE)
    
    # Check if any of the genes is not in the column names and drop em
    errored <- setdiff(genename, colnames(mdb))
    genename <- genename[!genename %in% errored]
    
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
    
    # Add project column
    fdf['Comparison'] = project
    
    # Return processed dataframe
    return(fdf)
  }
}

#' @export
preprocessing <- function(project, genename) {
  # Init result dataframes
  clust_df <- NULL
  countsDf <- NULL
  errored <- list()
  
  # Loop through the projects
  for (proj in project) {
    # Get data from the project design
    dbDesRows <- designPreproc(singleExp[[proj]][['project']])
    
    # Get the table of asking
    clust_df_tmp <- preprocDCdataSingle(singleExp[[proj]][['tabid']], genename)
    # Get the fold change data
    countsDf_tmp <- preprocCountsDataSingle(singleExp[[proj]][['tabid']], dbDesRows, genename)
    
    # Check empty dataframes and missing genes, store them in the error package
    erroredItems <- genename[which(!genename %in% clust_df_tmp[['Genes']])]
    # Display the readable name, and not the code
    projectName <- names(displayNames)[displayNames == proj]
    errored[[projectName]] <- erroredItems

    # Merge the fold change data
    if (is.null(clust_df) == T || dim(clust_df)[1] == 0){
      # If the final df is empty, fill it with one column
      clust_df <- clust_df_tmp
    } else {
      # If not, add the column to the df. Select columns if this is the case
      clust_df <- clust_df[c('id','Comparison','EnsGenes','log2FoldChange','pvalue','padj','Genes')]
      clust_df_tmp <- clust_df_tmp[c('id','Comparison','EnsGenes','log2FoldChange','pvalue','padj','Genes')]
      clust_df <- rbind(clust_df, clust_df_tmp)
    }
    
    if (is.null(countsDf) == T){
      # If the final df is empty, fill it with one column
      countsDf <- countsDf_tmp
    } else {
      # If not, add the column to the df
      countsDf <- rbind(countsDf, countsDf_tmp)
    }
  }
  # create boolean to hide the results if there is none
  plotDispl <- dim(clust_df)[1] != 0
  
  # Transform errored into table for display
  erroredDf <- data.frame()

  if (length(unlist(errored, use.names = FALSE)) == 0) {
    errDispl <- FALSE
  } else {
    erroredDf <- data.frame(
      'Exp' = names(errored),
      'Genes' = unlist(errored, use.names = FALSE)
    )
    errDispl <- TRUE
  }
  
  # Attach both dataframes on a named list for returning
  preprocResult = list(foldChangeData = clust_df,
                       countsData = countsDf,
                       errorData = erroredDf,
                       plotDispl = plotDispl,
                       errDispl = errDispl
                       )
  # Return result
  return(preprocResult)
}

#' @export
preprocComparisons <- function(projectA, projectB, genename) {
  # Ask the database for the table for experiment A
  dbRNARowsA <- queryExperiment(singleExp[[projectA]][['tabid']])
  dbRNARowsFiltA <- dbRNARowsA[c('id','EnsGenes','log2FoldChange','pvalue','padj','Genes')]
  
  # Filter by p value threshold
  dbRNARowsFiltA <- dbRNARowsFiltA[dbRNARowsFiltA$padj < 0.05,]
  
  # Add name of the project A
  dbRNARowsFiltA['Comparison'] <- c(projectA)
  
  # Repeat for project B
  dbRNARowsB <- queryExperiment(singleExp[[projectB]][['tabid']])
  dbRNARowsFiltB <- dbRNARowsB[c('id','EnsGenes','log2FoldChange','pvalue','padj','Genes')]
  dbRNARowsFiltB <- dbRNARowsFiltB[dbRNARowsFiltB$padj < 0.05,]
  dbRNARowsFiltB['Comparison'] <- c(projectB)
  
  # Merge both dataframes according to gene
  dbRNARowsMerg <- merge(dbRNARowsFiltA, dbRNARowsFiltB, by=c('EnsGenes','Genes'), all = FALSE)
  
  # Create the base plot
  plot2 <- ggplot(dbRNARowsMerg, aes(x=log2FoldChange.x, y=log2FoldChange.y)) + 
    geom_hline(yintercept=0, color = "orange") + 
    geom_vline(xintercept=0, color = "orange") +
    xlab(sprintf("Log2 Fold Change of %s", names(displayNames)[match(projectA,displayNames)])) + 
    ylab(sprintf("Log2 Fold Change of %s", names(displayNames)[match(projectB,displayNames)]))
  
  # Display all of the genes or only the listed ones
  if (length(genename) == 0) {
    plot2 <- plot2 + geom_point(colour='black') + geom_smooth(method=lm, formula = y ~ x) + 
      stat_regline_equation(label.y = max(dbRNARowsMerg$log2FoldChange.y)*0.95, aes(label = ..eq.label..)) +
      stat_regline_equation(label.y = max(dbRNARowsMerg$log2FoldChange.y)*0.9, aes(label = ..rr.label..))
  } else {
    # Select only the rows with the genes
    dbRNARowsGlist <- subset(dbRNARowsMerg, dbRNARowsMerg$Genes %in% genename)
    plot2 <- plot2 + 
      geom_point(colour='grey') + geom_smooth(method=lm, formula = y ~ x) +
      stat_regline_equation(label.y = max(dbRNARowsMerg$log2FoldChange.y)*0.95, aes(label = ..eq.label..)) +
      stat_regline_equation(label.y = max(dbRNARowsMerg$log2FoldChange.y)*0.9, aes(label = ..rr.label..)) + 
      geom_point(data=dbRNARowsGlist, aes(x=log2FoldChange.x,y=log2FoldChange.y), color='red') +
      geom_text(data=dbRNARowsGlist, aes(label=Genes),hjust=0, vjust=0)
  }
  
  # Attach all results to a named list for returning
  preprocResult = list(plotData = plot2)
  
  # Return result
  return(preprocResult)
}
