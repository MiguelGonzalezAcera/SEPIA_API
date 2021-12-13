box::use(
  shiny[...],
  RMariaDB[...],
  dplyr[...],
  stats[...],
  ggplot2[...],
  ggpubr[...],
  . / entities[fullExp,singleExp,geneLabels,displayNames],
)

#' @export
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

#' @export
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

#' @export
queryExperiment <- function(tabname, genename = c()) {
  # Establish the connection to the database
  dbconnection <- dbConnect(RMariaDB::MariaDB(), user='root', password="Plater1a", dbname='RNAseq', host='localhost')
  
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
  
  # Add name of the project
  dbRNARowsFilt['Comparison'] <- c(project)
  
  # Return processed dataframe
  return(dbRNARowsFilt)
}

#' @export
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
  
  # Return processed dataframe
  return(clust_df)
}

#' @export
preprocCountsData <- function(dbProjRows, dbDesRows, genename) {
  # Get a dataframe with the columns of the fold change of all the samples
  countsDf <- NULL
  
  # Loop to get the sets of data we need for the display
  for (i in c(1:nrow(dbProjRows))) {
    # Run the single id for each project
    countsDfTmp <- preprocCountsDataSingle(dbProjRows[['Comparison']][i], dbDesRows, genename)
    
    # Merge the piece of data
    if (is.null(countsDf) == T){
      # If the final df is empty, fill it with one column
      countsDf <- countsDfTmp
    } else {
      # If not, add the column to the df
      countsDf <- rbind(countsDf, countsDfTmp)
    }
  }
  
  # Group by treatment and genename
  countsDfGr <- countsDf %>% group_by(Treatment, Genename) %>% summarise(CountsMean = mean(Counts), CountsErrSup = mean(Counts) + sd(Counts), CountsErrInf = mean(Counts) - sd(Counts))
  # Transform to dataframe
  countsDfGr <- data.frame(countsDfGr)
  # Merge with design in order to keep the order
  countsDfGrMer <- merge(x=dbDesRows, y=countsDfGr, by='Treatment', all.y=TRUE)
  # Order by the intended ID, keep the interesting columns, drop duplicated rows
  countsDfFinal <- unique(countsDfGrMer[order(countsDfGrMer$ID), c('Treatment','Genename','CountsMean','CountsErrSup','CountsErrInf')])
  # Turn treatment into a character vector and then back into a factor to keep order of treatments in the plot
  countsDfFinal$Treatment <- as.character(countsDfFinal$Treatment)
  countsDfFinal$Treatment <- factor(countsDfFinal$Treatment, levels=unique(countsDfFinal$Treatment))
  
  # Return processed dataframe
  return(countsDfFinal)
}

#' @export
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
  
  # Add project column
  fdf['Comparison'] = project
  
  # Return processed dataframe
  return(fdf)
}

#' @export
preprocessing <- function(project, genename) {
  # Init result dataframes
  clust_df <- NULL
  countsDf <- NULL
  
  # Loop through the projects
  for (proj in project) {
    # Choose between single experiment and complete sequentiations
    if (proj %in% names(fullExp)){
      # Get data from the project
      dbProjRows <- projectPreproc(fullExp[[proj]])
      
      # Get data from the project design
      dbDesRows <- designPreproc(fullExp[[proj]])
      
      # Get the fold change data
      clust_df_tmp <- preprocFCdata(dbProjRows, genename)
      # Get the fold change data
      countsDf_tmp <- preprocCountsData(dbProjRows, dbDesRows, genename)
    } else if (proj %in% names(singleExp)) {
      # Get data from the project design
      dbDesRows <- designPreproc(singleExp[[proj]][['project']])
      
      # Get the table of asking
      clust_df_tmp <- preprocDCdataSingle(singleExp[[proj]][['tabid']], genename)
      # Get the fold change data
      countsDf_tmp <- preprocCountsDataSingle(singleExp[[proj]][['tabid']], dbDesRows, genename)
    }
    
    # Merge the fold change data
    if (is.null(clust_df) == T){
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
  # Attach both dataframes on a named list for returning
  preprocResult = list(foldChangeData = clust_df,
                       countsData = countsDf
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
      geom_point(data=dbRNARowsGlist, aes(x=log2FoldChange.x,y=log2FoldChange.y), color='red')
  }
  
  # Attach all results to a named list for returning
  preprocResult = list(plotData = plot2)
  
  # Return result
  return(preprocResult)
}