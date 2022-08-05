box::use(
  shiny[...],
  RMariaDB[...],
  dplyr[...],
  stats[...],
  ggplot2[...],
  ggpubr[...],
  ggtext[...],
  stringr[...],
  ggVennDiagram[...],
  readxl[...],
  ComplexHeatmap[...],
  dendextend[...],
  cluster[...],
  gplots[...],
  circlize[...],
  grid[...],
  clusterProfiler[...],
  enrichplot[...],
  . / entities[fullExp,singleExp,displayNames],
)

#'@export
geneLabels <- function(){
  # Establish the connection to the projects database
  projectsRefDb <- dbConnect(RMariaDB::MariaDB(), user='sepia', password="sepia_TRR241", dbname='Refs', host='localhost')
  
  # Create the query with the project name
  queryRefText <- "select EnsGenes,Genes from mouse_genes;"
  
  # Run the query against the database and fetch the resulting dataframe
  rsRefInsert <- dbSendQuery(projectsRefDb, queryRefText)
  dbRefCol <- dbFetch(rsRefInsert)
  
  # Clear the query
  dbClearResult(rsRefInsert)
  
  # Disconnect the database
  dbDisconnect(projectsRefDb)
  
  # Make the mouse list
  mouse_genelist <- as.list(dbRefCol[['EnsGenes']])
  names(mouse_genelist) <- as.vector(dbRefCol[['Genes']])
  
  # return List of genes with a name
  geneLabelsList <- list(
    'mouse_genes' = mouse_genelist
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
projectPreproc <- function(project) {
  # Establish the connection to the projects database
  projectDb <- dbConnect(RMariaDB::MariaDB(), user='sepia', password="sepia_TRR241", dbname='Projects', host='localhost')
  
  # Create the query with the project name
  queryProjText <- sprintf("select * from %s;", project)

  # Run the query against the database and fetch the resulting dataframe
  rsProjInsert <- dbSendQuery(projectDb, queryProjText)
  dbProjRows <- dbFetch(rsProjInsert)
  
  # Clear the query
  dbClearResult(rsProjInsert)
  
  # Disconnect the database
  dbDisconnect(projectDb)
  
  # return projects dataframe
  return(dbProjRows)
}

#' @export
queryExperiment <- function(tabname, genename = c()) {
  # Establish the connection to the database
  dbconnection <- dbConnect(RMariaDB::MariaDB(), user='sepia', password="sepia_TRR241", dbname='SEPIA', host='localhost')
  
  # create the query dor the rnaseq db 
  queryRNAText <- sprintf("select * from %s;", tabname)
  
  # Run the wuery for the data
  rsRNAFCInsert <- dbSendQuery(dbconnection, queryRNAText)
  dbRNARows<-dbFetch(rsRNAFCInsert)

  # filter by genename(s) if genelist has genes
  if (length(genename) > 0) {
    dbRNARows <- subset(dbRNARows, dbRNARows$EnsGenes %in% genename)
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
preprocCountsDataSingle <- function(project, dbDesRows, dbProjRows, genename) {
  # Run the query database function
  dbRNARows <- queryExperiment(project, genename)
  
  # Replace the ensembl id array by the gene names
  genename <- dbRNARows[['Genes']]
  
  # Select the row with our project
  dbProjRows <- dbProjRows[dbProjRows$Comparison == project,]
  # Create reference for the order of the control and sample rows
  orderCtrl <- c(dbProjRows[['Control']], dbProjRows[['Sample']])

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
    mdb <- mdb[order(mdb$Treatment),]
    
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

      # Sort the dataframe according to the control sample dynamic
      if (wmdb$Treatment[1] != orderCtrl[1]) {
        wmdb <- wmdb[rev(order(wmdb$Treatment)),]
        wmdb$Treatment <- as.factor(wmdb$Treatment)
        wmdb$Treatment <- with(wmdb, relevel(Treatment, orderCtrl[1]))
      }
      rownames(wmdb) <- NULL

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
  countsBox <- list()
  errored <- list()
  
  # Loop through the projects
  for (proj in project) {
    # Get data from the project design
    dbDesRows <- designPreproc(singleExp[[proj]][['project']])
    
    # Get data from the project design
    dbProjRows <- projectPreproc(singleExp[[proj]][['project']])
    
    # Get the fold change data
    clust_df_tmp <- preprocDCdataSingle(singleExp[[proj]][['tabid']], genename)
    
    # Check empty dataframes and missing genes, store them in the error package
    erroredItems <- genename[which(!genename %in% clust_df_tmp[['EnsGenes']])]
    # Display the readable name, and not the code
    projectName <- names(displayNames)[displayNames == proj]
    
    # Add the gene if it has failed
    if (length(erroredItems) >= 1) {
      errored[[projectName]] <- names(geneLabels()$mouse_genes)[geneLabels()$mouse_genes %in% erroredItems]
    }
    
    # Add display name also to the dataframes if they are not empty
    if (dim(clust_df_tmp)[1] != 0) {
      clust_df_tmp['ModelName'] <- projectName
    }

    # Merge the fold change data
    if (dim(clust_df_tmp)[1] != 0) {
      if (is.null(clust_df) == T){
        # If the final df is empty, fill it with one column
        clust_df <- clust_df_tmp
      } else {
        # If not, add the column to the df. Select columns if this is the case
        clust_df <- clust_df[c('id','ModelName','EnsGenes','log2FoldChange','pvalue','padj','Genes')]
        clust_df_tmp <- clust_df_tmp[c('id','ModelName','EnsGenes','log2FoldChange','pvalue','padj','Genes')]
        clust_df <- rbind(clust_df, clust_df_tmp)
      }
    }

    # Loop throught the genes
    for (gene in genename) {
      # Get the flag of the gene
      flagResult <- queryExperiment(singleExp[[proj]][['tabid']], gene)[['FLAG']]
      pvalue <- queryExperiment(singleExp[[proj]][['tabid']], gene)[['padj']]

      # Get the counts data
      countsDf <- preprocCountsDataSingle(singleExp[[proj]][['tabid']], dbDesRows, dbProjRows, gene)

      # Check if it is empty
      if (dim(countsDf)[1] != 0) {
        # Select only the interesting columns
        countsDf2 <- countsDf[c('Treatment','Counts')]

        # Transform the name of the gene from ensemblidinto geneid
        geneID <- names(geneLabels()$mouse_genes)[geneLabels()$mouse_genes == gene]
        
        # Select background color
        if (flagResult != 'OK') {
          bgColor <- '#F0EED4'
        } else if (pvalue < 0.05) {
          bgColor <- '#D5F0D4'
        } else {
          bgColor <- 'white'
        }
        
        # Create the boxplot with the extracted data
        geneBplot <- ggplot(countsDf2, aes(x=Treatment, y=Counts)) + 
          geom_boxplot() +
          ggtitle(str_wrap(paste(geneID, projectName, sep = ' - '), 35)) +
          theme(
            panel.background = element_rect(fill = bgColor,
                                            colour = bgColor,
                                            size = 0.5, linetype = "solid")
          )
        
        # Add plot to list
        countsBox[[paste(geneID, projectName, sep = '_')]] <- geneBplot
      }
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
                       countsData = countsBox,
                       errorData = erroredDf,
                       plotDispl = plotDispl,
                       errDispl = errDispl
                       )
  
  # Return result
  return(preprocResult)
}

#' @export
formatExcelDL <- function(dbRNA, projectA, projectB) {
  # Get a subtable for each quadrant
  dbRNAPP <- dbRNA[dbRNA$log2FoldChange.x >= 0 & dbRNA$log2FoldChange.y >= 0,]
  dbRNAPN <- dbRNA[dbRNA$log2FoldChange.x >= 0 & dbRNA$log2FoldChange.y <= 0,]
  dbRNANP <- dbRNA[dbRNA$log2FoldChange.x <= 0 & dbRNA$log2FoldChange.y >= 0,]
  dbRNANN <- dbRNA[dbRNA$log2FoldChange.x <= 0 & dbRNA$log2FoldChange.y <= 0,]
  
  # Rename the columns with each experiment
  newNames <- str_replace(colnames(dbRNA), '.x', sprintf('_%s',projectA))
  newNames <- str_replace(newNames, '.y', sprintf('_%s',projectB))
  
  colnames(dbRNAPP) <- newNames
  colnames(dbRNAPN) <- newNames
  colnames(dbRNANP) <- newNames
  colnames(dbRNANN) <- newNames
  
  # Arrange the things in a list and return
  quadrantsRNA <- list()
  quadrantsRNA[[sprintf('%s v %s. Q2', projectA, projectB)]] = dbRNAPP
  quadrantsRNA[[sprintf('%s v %s. Q4', projectA, projectB)]] = dbRNAPN
  quadrantsRNA[[sprintf('%s v %s. Q1', projectA, projectB)]] = dbRNANP
  quadrantsRNA[[sprintf('%s v %s. Q3', projectA, projectB)]] = dbRNANN
  
  # Return the ting
  return(quadrantsRNA)
}

#' @export
vennComparison <- function(dbA, dbB, pNames) {
  # Remove log
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  
  #Select by up/down and make two venns
  # upreg
  dbAUfilt <- dbA[dbA$log2FoldChange >= 0,]
  dbBUfilt <- dbB[dbB$log2FoldChange >= 0,]
  
  #Sets of information
  vUpreg <- list()
  vUpreg[[str_wrap(pNames[1], 20)]] <- dbAUfilt[['Genes']]
  vUpreg[[str_wrap(pNames[2], 20)]] <- dbBUfilt[['Genes']]

  # Upreg plot
  upVenn <- ggVennDiagram(
    vUpreg,
    set_size = 4,
    edge_size = 0.1,
    edge_color = 'black'
  ) +
    ggtitle('Upregulated genes') +
    scale_fill_gradient(limits = c(0,NA),low="white",high = "red") + 
    scale_y_continuous(expand = expansion(mult = .2))

  # downreg
  dbADfilt <- dbA[dbA$log2FoldChange <= 0,]
  dbBDfilt <- dbB[dbB$log2FoldChange <= 0,]
  
  #Sets of information
  vDownreg <- list()
  vDownreg[[str_wrap(pNames[1], 20)]] <- dbADfilt[['Genes']]
  vDownreg[[str_wrap(pNames[2], 20)]] <- dbBDfilt[['Genes']]
  
  # downreg plot
  downVenn <- ggVennDiagram(
    vDownreg,
    set_size = 4,
    edge_size = 0.1,
    edge_color = 'black'
  ) +
    ggtitle('Downregulated genes') +
    scale_fill_gradient(limits = c(0,NA),low="white",high = "red") + 
    scale_y_continuous(expand = expansion(mult = .2))
  
  #Make the list
  resultVenn <- ggarrange(upVenn, downVenn, ncol = 2)
  
  return(resultVenn)
}

#' @export
preprocComparisons <- function(projectA, projectB, genename) {
  # Ask the database for the table for experiment A
  dbRNARowsA <- queryExperiment(singleExp[[projectA]][['tabid']])
  dbRNARowsFiltA <- dbRNARowsA[c('EnsGenes','log2FoldChange','pvalue','padj','Genes')]
  
  # Filter by p value threshold
  dbRNARowsFiltA <- dbRNARowsFiltA[dbRNARowsFiltA$padj < 0.05,]
  
  # Add name of the project A
  dbRNARowsFiltA['Comparison'] <- c(projectA)
  
  # Repeat for project B
  dbRNARowsB <- queryExperiment(singleExp[[projectB]][['tabid']])
  dbRNARowsFiltB <- dbRNARowsB[c('EnsGenes','log2FoldChange','pvalue','padj','Genes')]
  dbRNARowsFiltB <- dbRNARowsFiltB[dbRNARowsFiltB$padj < 0.05,]
  dbRNARowsFiltB['Comparison'] <- c(projectB)
  
  # Generate name
  projectNameA <- names(displayNames)[displayNames == projectA]
  projectNameB <- names(displayNames)[displayNames == projectB]
  projectName <- paste(names(displayNames)[displayNames == projectA], names(displayNames)[displayNames == projectB], sep = ' v ')
  
  # Generate the venn diagrams
  vennDiags <- vennComparison(dbRNARowsFiltA, dbRNARowsFiltB, c(projectNameA, projectNameB))

  # Merge both dataframes according to gene
  dbRNARowsMerg <- merge(dbRNARowsFiltA, dbRNARowsFiltB, by=c('EnsGenes','Genes'), all = FALSE)

  # Create the base plot
  plot2 <- ggplot(dbRNARowsMerg, aes(x=log2FoldChange.x, y=log2FoldChange.y)) +
    geom_hline(yintercept=0, color = "orange") +
    geom_vline(xintercept=0, color = "orange") +
    xlab(sprintf("Log2 Fold Change of %s", names(displayNames)[match(projectA,displayNames)])) +
    ylab(sprintf("Log2 Fold Change of %s", names(displayNames)[match(projectB,displayNames)]))

  # Display all of the genes or only the listed ones
  if (length(genename) == 0 | length(subset(dbRNARowsMerg, dbRNARowsMerg$EnsGenes %in% genename)) == 0) {
    # format the table for download
    dbRNARowsDL <- formatExcelDL(dbRNARowsMerg, projectA, projectB)

    # Complete the plot
    plot2 <- plot2 + geom_point(colour='black') + geom_smooth(method=lm, formula = y ~ x) +
      stat_regline_equation(label.y = max(dbRNARowsMerg$log2FoldChange.y)*0.95, aes(label = ..eq.label..)) +
      stat_regline_equation(label.y = max(dbRNARowsMerg$log2FoldChange.y)*0.9, aes(label = ..rr.label..)) + 
      theme_bw()
  } else {
    # Select only the rows with the genes
    dbRNARowsGlist <- subset(dbRNARowsMerg, dbRNARowsMerg$EnsGenes %in% genename)

    # format the table for download
    dbRNARowsDL <- formatExcelDL(dbRNARowsGlist, projectA, projectB)

    # Complete the plot
    plot2 <- plot2 +
      geom_point(colour='grey') + geom_smooth(method=lm, formula = y ~ x) +
      stat_regline_equation(label.y = max(dbRNARowsMerg$log2FoldChange.y)*0.95, aes(label = ..eq.label..)) +
      stat_regline_equation(label.y = max(dbRNARowsMerg$log2FoldChange.y)*0.9, aes(label = ..rr.label..)) +
      geom_point(data=dbRNARowsGlist, aes(x=log2FoldChange.x,y=log2FoldChange.y), color='red') +
      geom_text(data=dbRNARowsGlist, aes(label=Genes),hjust=0, vjust=0) + 
      theme_bw()
  }

  # Attach all results to a named list for returning
  preprocResult = list(plotData = plot2,
                       fullData = dbRNARowsMerg,
                       vennData = vennDiags,
                       commonData = dbRNARowsDL)

  # Return result
  return(preprocResult)
}

#' @export
getMarkerlist <- function(markerID) {
  # Establish the connection to the projects database
  markersDb <- dbConnect(RMariaDB::MariaDB(), user='sepia', password="sepia_TRR241", dbname='Refs', host='localhost')
  
  # Create the query with the project name
  queryMarkText <- sprintf("select * from %s;", markerID)
  
  # Run the query against the database and fetch the resulting dataframe
  rsMarkInsert <- dbSendQuery(markersDb, queryMarkText)
  dbMarkRows <- dbFetch(rsMarkInsert)
  
  # Clear the query
  dbClearResult(rsMarkInsert)
  
  # Disconnect the database
  dbDisconnect(markersDb)
  
  # Select the gene names
  genelist <- dbMarkRows[['EnsGenes']]

  # return Table of the wxperiment with the selected genes
  return(genelist)
}

#' @export
readGenelist <- function(filepath) {
  if (endsWith(filepath, '.txt')) {
    # read txt file
    genelist <- scan(filepath, character())
  } else if (endsWith(filepath, '.xlsx')) {
    # Read excel file
    genelist <- as.data.frame(read_excel(filepath, col_names=FALSE))[,1]
  }
  
  # Translate the gene names into ensembl ids
  genelist <- unlist(geneLabels()$mouse_genes[genelist], use.names = F)
  
  # return character vector with the selected genes
  return(genelist)
}

#' @export
heatmap <- function (project, genelist) {
  # check if the length of the project list is 1 or more
  if (length(project) > 1) {
    clust_df <- NULL
    pval_df <- NULL
    for (tabname in project){
      # Establish the connection to the database
      genelistDF <- queryExperiment(singleExp[[tabname]][['tabid']], genelist)
      
      # Filter flagged genes
      genelistDF <- subset(genelistDF, genelistDF$FLAG == "OK")
      
      # Put ensembl as row names
      rownames(genelistDF) <- genelistDF$EnsGenes
      
      # Sort the names by rowname (ENSEMBLID)
      genelistDF <- genelistDF[order(row.names(genelistDF)),]
      
      # Get the fold change and the pvalue column
      FC_df <- genelistDF["log2FoldChange"]
      #pv_df <- full_df["padj_fix"]
      pv_df <- genelistDF["padj"]
      
      # Name the column as the file
      colnames(FC_df) <- c(tabname)
      colnames(pv_df) <- c(tabname)
      
      if (is.null(clust_df) == T){
        # If the final df is empty, fill it with one column
        clust_df <- FC_df
        pval_df <- pv_df
      } else {
        # If not, add the column to the df
        clust_df <- merge(clust_df, FC_df, by=0,all=T)
        rownames(clust_df) <- clust_df$Row.names
        clust_df$Row.names <- NULL
        
        pval_df <- merge(pval_df, pv_df, by=0,all=T)
        rownames(pval_df) <- pval_df$Row.names
        pval_df$Row.names <- NULL
      }
    }
    # Replace NA values
    clust_df[is.na(clust_df)] <- 0
    pval_df[is.na(pval_df)] <- 1
    
    # Keep only genes with valid pvalues
    clust_df <- clust_df[rownames(clust_df) %in% rownames(pval_df), ,drop=FALSE]
    
    # cluster the dataframe and get the order of files
    hr <- hclust(as.dist(1-cor(t(data.matrix(clust_df)), method="pearson")), method="average")
    gene_order <- hr$labels[hr$order]
    
    # Reorder dataframes according to the new order
    clust_df['Row.names'] <- rownames(clust_df)
    clust_df <- clust_df[match(gene_order,clust_df$Row.names),]
    clust_df$Row.names <- NULL
    
    pval_df['Row.names'] <- rownames(pval_df)
    pval_df <- pval_df[match(gene_order,pval_df$Row.names),]
    pval_df$Row.names <- NULL
    
    # Create object of result
    circle_df <- NULL
    modelNames <- c()
    # Iter through the columns to create the proper object
    for (column in colnames(clust_df)) {
      # Element1: Gene names
      genes_list <- rownames(clust_df[column])
      
      #Element2: Fold changes
      FC_list <- clust_df[[column]]
      
      #Element3: log10 pvals
      pv_list <- pval_df[[column]]
      
      #Element4: names of genes.
      genenames_list <- names(geneLabels()$mouse_genes[geneLabels()$mouse_genes %in% genes_list][order(match(geneLabels()$mouse_genes[geneLabels()$mouse_genes %in% genes_list],gene_order))])
      
      # Create data frame
      wdf <- data.frame(
        EnsGenes = genes_list,
        Genes = genenames_list,
        FoldChange = FC_list,
        pval = pv_list
      )
      
      # Element5: Table name
      wdf['model'] <- names(displayNames)[displayNames == column]
      
      # Add display name to model names
      modelNames <- c(modelNames, names(displayNames)[displayNames == column])
      
      if (is.null(circle_df) == T){
        circle_df <- wdf
      } else {
        circle_df <- rbind(circle_df, wdf)
      }
    }
    
    # Get model column and gene column as factor to avoid releveling
    circle_df$model <- factor(circle_df$model, levels = modelNames)
    circle_df$Genes <- factor(circle_df$Genes, levels = genenames_list)
    
    # Define limits of the scale
    limitEdges <- c(min(min(circle_df$FoldChange),-2),max(max(circle_df$FoldChange),2))
    
    # Define the scaling of the colors (Lord help me)
    colorscale <- c('blue','white','red')
    
    # Get the values for the color scale maximums
    minblue = (-2-limitEdges[1])/(limitEdges[2]-limitEdges[1])
    zerowhite = (0-limitEdges[1])/(limitEdges[2]-limitEdges[1])
    maxred = (2-limitEdges[1])/(limitEdges[2]-limitEdges[1])
    
    valuescale <- c(minblue,zerowhite,maxred)
    
    # Set additional values if necessary
    if (minblue > 0) {
      colorscale <- c('blue',colorscale)
      valuescale <- c(0, valuescale)
    }
    
    if (maxred < 1) {
      colorscale <- c(colorscale, 'red')
      valuescale <- c(valuescale, 1)
    }
    
    # Determine maximum value of radius
    radius <- max(5, 30 - min(15,if (length(levels(circle_df$model)) < 5) {0} else {1.2*length(levels(circle_df$model))}) - min(10,if (length(levels(as.factor(circle_df$EnsGenes))) < 7) {0} else {0.5*length(levels(as.factor(circle_df$EnsGenes)))}))

    # Make the plot. Got it from https://stackoverflow.com/questions/67746044/r-heatmap-with-circles
    resultHeatmap <- ggplot(circle_df, aes(model, Genes, fill = FoldChange, size = pval)) +
      geom_point(shape = 21, stroke = 0) +
      geom_hline(yintercept = seq(.5, length(levels(circle_df$Genes)) + 0.5, 1), size = .1, colour = "#F88E06") +
      geom_vline(xintercept = seq(.5, length(levels(circle_df$model)) + 0.5, 1), size = .1, colour = "#F88E06") +
      scale_x_discrete(position = "bottom") +
      scale_radius(limits = c(0, 0.2), breaks = c(0, 0.05, 0.1, 0.15), range = c(radius, 0)) +
      scale_fill_gradientn(colours = colorscale, values = valuescale, breaks = c(-4, -2, 0, 2, 4), labels = c('-4', "-2", "0", "2", '4'), limit = limitEdges) +
      theme_minimal()
    
    # check if genelist is too long
    if (length(genelist) > 30) {
      resultHeatmap <- resultHeatmap + theme(legend.position = "bottom", 
                                             panel.grid.major = element_blank(),
                                             axis.text.y=element_blank(),
                                             text = element_text(size = 25),
                                             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                             legend.text = element_text(size = 10),
                                             legend.title = element_text(size = 10))
    } else {
      resultHeatmap <- resultHeatmap + theme(legend.position = "bottom", 
                                             panel.grid.major = element_blank(),
                                             text = element_text(size = 25),
                                             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                             legend.text = element_text(size = 10),
                                             legend.title = element_text(size = 10))
    }
    
    # Add the rest of the parameters
    resultHeatmap <- resultHeatmap + guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25), 
                                                                label.position = "bottom",
                                                                title.position = "right", 
                                                                order = 1),
                                            fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
                                      labs(size = "Area = P value", fill = "Fold Change:", x = NULL, y = NULL)
  } else {
    # Query the data with the obtained gene list
    genelistDF <- queryExperiment(singleExp[[project]][['tabid']], genelist)
    
    # Get data from the project design
    dbDesRows <- designPreproc(singleExp[[project]][['project']])
    
    # Get data from the project design
    dbProjRows <- projectPreproc(singleExp[[project]][['project']])
    
    # Select the row with our project
    dbProjRows <- dbProjRows[dbProjRows$Comparison == singleExp[[project]][['tabid']],]
    
    # Subset design and get length of control
    dbDesSlice <- dbDesRows[dbDesRows$Treatment == dbProjRows[['Control']] | dbDesRows$Treatment == dbProjRows[['Sample']],]
    # Transform the treatment column into a factor with the control first
    dbDesSlice$Treatment <- factor(dbDesSlice$Treatment, levels = c(dbProjRows[['Control']], dbProjRows[['Sample']]))
    # Sort by the factor levels to get the control always on the left of the heatmap
    dbDesSlice <- dbDesSlice[order(dbDesSlice$Treatment),]
  
    # Change unknown gene names to something more fitting for the heatmap calculations
    rows_hm <- as.character(genelistDF$Genes)
    
    rows_hm[is.na(rows_hm)|duplicated(rows_hm)|rows_hm == 'NA'] <- as.character(genelistDF$EnsGenes)[is.na(rows_hm)|duplicated(rows_hm)|rows_hm == 'NA']
    rownames(genelistDF) <- rows_hm
  
    # Select the columns only with the counts for the heatmap
    genelistDF <- genelistDF[-which(names(genelistDF) %in% c('id','EnsGenes','baseMean','log2FoldChange','lfcSE','stat','pvalue','padj','Genes','FLAG'))]
    
    # Cluster the rows of the data frame. Insert into a trycatch in case some thing odd happens
    hr = tryCatch({
      as.dendrogram(hclust(as.dist(1-cor(t(data.matrix(genelistDF)),
                                         method="pearson")), method="complete"))
    }, error = function(e) {
      FALSE
    })
    
    # Establish colors
    color <- colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
  
    # Make the heatmap (hide the names if it is too large)
    if (length(genelist) > 30) {
      resultHeatmap <-Heatmap(t(scale(t(log(data.matrix(genelistDF) + 1)))), cluster_rows = hr,
                              cluster_columns = FALSE,
                              show_row_names = FALSE,
                              col=color, column_dend_height = unit(5, "cm"),
                              row_dend_width = unit(2, "cm"),
                              column_split = factor(dbDesSlice$Treatment),
                              cluster_column_slices = FALSE,
                              column_gap = unit(0.5, "cm"),
                              show_column_names = FALSE,
                              heatmap_legend_param = list(
                                title = "relative expression",
                                title_gp = gpar(fontsize = (90/length(rows_hm)+5)),
                                legend_height = unit(10, "cm"),
                                grid_width = unit(1, "cm"),
                                at = c(-4, -2, 0, 2, 4),
                                labels_gp = gpar(fontsize = (90/length(rows_hm)+5)),
                                title_position = "leftcenter-rot"
                              )
                              
      )
    } else {
      resultHeatmap <-Heatmap(t(scale(t(log(data.matrix(genelistDF) + 1)))), cluster_rows = hr,
                              cluster_columns = FALSE,
                              row_names_gp = gpar(fontsize = (90/length(rows_hm)+5)),
                              col=color, column_dend_height = unit(5, "cm"),
                              row_dend_width = unit(2, "cm"),
                              column_split = factor(dbDesSlice$Treatment),
                              cluster_column_slices = FALSE,
                              column_gap = unit(0.5, "cm"),
                              show_column_names = FALSE,
                              heatmap_legend_param = list(
                                title = "relative expression",
                                title_gp = gpar(fontsize = (90/length(rows_hm)+5)),
                                legend_height = unit(10, "cm"),
                                grid_width = unit(1, "cm"),
                                at = c(-4, -2, 0, 2, 4),
                                labels_gp = gpar(fontsize = (90/length(rows_hm)+5)),
                                title_position = "leftcenter-rot"
                              )
      )
    }
  }
  # Return finished plot
  return(resultHeatmap)
}

#' @export
volcanoPlot <- function(project, genelist) {
  # Query the data with the obtained gene list
  genelistDF <- queryExperiment(singleExp[[project]][['tabid']], genelist)
  
  # Create the displayable column for the p value
  genelistDF[['padj_fix']] <- -log10(genelistDF$pvalue+(1*10^-31))
  
  # Filter the dataframe for the genes sign. up and down
  genelistDFUp <- genelistDF[genelistDF$log2FoldChange >= 1 & genelistDF$pvalue < 0.05,]
  genelistDFDw <- genelistDF[genelistDF$log2FoldChange <= -1 & genelistDF$pvalue < 0.05,]

  # Generate the plot
  # We put the data in geom_point, because if not, it draws a rectangle per row, one on top of each other, rendering them opaque in the end (https://stackoverflow.com/questions/43511416/how-do-you-control-the-translucence-of-geom-rect-rectangles)
  plot <- ggplot(genelistDF, aes(x=log2FoldChange, y=padj_fix)) +
    geom_point(colour='black') + 
    annotate(
      "rect",
      xmin = 1,
      xmax = Inf,
      ymin = -log10(0.05),
      ymax = Inf,
      linetype = 'blank',
      fill = 'red',
      alpha = 0.2
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = -1,
      ymin = -log10(0.05),
      ymax = Inf,
      linetype = 'blank',
      fill = 'blue',
      alpha = 0.2
    ) +
    ylim(0, max(25, max(genelistDF$padj_fix))) +
    xlim(
      min(c(-2, min(genelistDF$log2FoldChange))),
      max(c(2, max(genelistDF$log2FoldChange)))
    ) +
    geom_hline(yintercept=0, color = "orange") +
    geom_vline(xintercept=0, color = "orange") +
    xlab('Log2 Fold Change') +
    ylab('-log10(padj)') +
    theme_bw()
  
  # Add the colored dots (if any)
  if (dim(genelistDFUp)[1] != 0) {
    plot <- plot + geom_point(data = genelistDFUp, aes(x=log2FoldChange, y=padj_fix), color = 'red', size = 3)
  }
  if (dim(genelistDFDw)[1] != 0) {
    plot <- plot + geom_point(data = genelistDFDw, aes(x=log2FoldChange, y=padj_fix), color = 'blue', size = 3)
  }
  
  # Return the plot
  return(plot)
}

#' @export
GSEAgraph <- function(project, genelist, handle) {
  # Query the data with the obtained gene list
  genelistDF <- queryExperiment(singleExp[[project]][['tabid']], genelist)
  
  genelistTab <- queryExperiment(singleExp[[project]][['tabid']])
  
  # Create the ordered genelist for reference
  gene_List <- genelistTab$log2FoldChange
  names(gene_List) <- genelistTab$EnsGenes
  
  # Make the gene groups
  groups <- data.frame(handle, genelistDF$EnsGenes)
  
  # run the GSEA analysis
  z = GSEA(sort(gene_List,decreasing=T), TERM2GENE = groups, pvalueCutoff = 1, minGSSize = 5)
  
  # Save the objects
  GSEAtable <- as.data.frame(z)
  GSEAplot <- gseaplot2(z, geneSetID = 1, color="red", pvalue_table = FALSE)
  
  # Amalgame the results in a list
  result = list(
    table_gsea = GSEAtable,
    plot_GSEA = GSEAplot
  )
  
  # Return the thing
  return(result)
}

