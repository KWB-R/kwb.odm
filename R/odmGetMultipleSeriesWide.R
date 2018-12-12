# getMultipleTimeSeries --------------------------------------------------------
getMultipleTimeSeries <- function # getMultipleTimeSeries
### getMultipleTimeSeries
(
  siteCodes,
  variableCodes,
  from.UTC = NULL, 
  to.UTC = NULL,
  roundToDigits = NULL,
  db = currentDb(),
  odmDatasets = availableOdmDatasets(db = db),
  wide = TRUE
)
{
  datasetIDs <- getDatasetIDs(odmDatasets, siteCodes, variableCodes)
  
  if (isNullOrEmpty(datasetIDs)) {
    message("No datasets found.")
    return()
  }
  
  dataValues.raw <- getDataValuesByDatasetID(
    db, datasetIDs, from.UTC, to.UTC, odmDatasets=odmDatasets, dbg = FALSE)
  
  dataValues <- resolveForeignKeys(
    dataValues.raw, db, idsToResolve = c("SiteID", "VariableID"))
  
  dataValues <- .roundByVariableCode(dataValues, variableCodes, roundToDigits)
  
  dataValues <- .removeIdColumns(dataValues)
  
  dataValues <- .renameSitesAndVariables(dataValues, siteCodes, variableCodes)
  
  if (wide) {
    dataValues.wide <- .reshapeToWideView(dataValues)
    
    dataValues.wide <- .renameDataValueColumns(dataValues.wide)
    
    result <- .sortRowsAndColumns(dataValues.wide)    
  }
  else {    
    result <- dataValues[order(dataValues$DateTimeUTC), ]    
  }
  
  result
}

# .toShortNameReplacements -----------------------------------------------------
.toShortNameReplacements <- function(variableCodes)
{  
  variableShortNames <- names(variableCodes)
  
  if (is.null(variableShortNames)) {
    replacements <- NULL
  }
  else {
    noShortName <- variableShortNames == ""
    variableShortNames[noShortName] <- variableCodes[noShortName]

    replacements <- as.list(variableShortNames)    
    names(replacements) <- variableCodes    
  }
  
  replacements
}

# getDatasetIDs ----------------------------------------------------------------
getDatasetIDs <- function # getDatasetIDs
### getDatasetIDs
(
  odmDatasets, siteCodes, variableCodes
)
{
  keys <- odmDatasets$keyFields
  select <- keys$SiteCode %in% siteCodes & keys$VariableCode %in% variableCodes
  
  keys <- keys[select, ]
  
  if (isNullOrEmpty(keys)) {
    datasetIDs <- NULL
  }
  else {
    numberOfSeries <- aggregate(
      dsid ~ SiteCode + VariableCode, data = keys, FUN = length)
    
    ambiguous <- numberOfSeries[numberOfSeries$dsid > 1, ]
    
    if (!isNullOrEmpty(ambiguous)) {
      print(ambiguous)
      stop("SiteCode and VariableCode do not identify the dataset unambiguously ",
           "(see table above)")
    }
    datasetIDs <- keys$dsid
  }
  
  datasetIDs
}

# getDataValuesByDatasetID -----------------------------------------------------
getDataValuesByDatasetID <- function # getDataValuesByDatasetID
### getDataValuesByDatasetID
(
  db, 
  datasetIDs, 
  from.UTC, 
  to.UTC, 
  timeColumns = c("DateTimeUTC"), # , "LocalDateTime", "UTCOffset")
  odmDatasets,
  dbg = TRUE
)
{
  datasetInfo <- availableOdmDatasets(db = db, resolveIDs = FALSE)
  
  dataValues <- NULL
  
  for (datasetID in datasetIDs) {
    dsInfo <- odmDatasets$keyFields[odmDatasets$keyFields$dsid == datasetID, ]

    cat("\nLoading", as.character(dsInfo$VariableCode), 
        "at", as.character(dsInfo$SiteCode), "...\n")
    
    odmDataset <- getOdmDataset(
      dsid = datasetID, 
      from.UTC = from.UTC, 
      to.UTC = to.UTC,
      dsInfo = datasetInfo,   
      fields = commaCollapsed(c(timeColumns, "DataValue,SiteID,VariableID")),
      time.ordered = TRUE,
      as.is = TRUE,
      dbg = dbg
    )
    
    dataValues <- rbind(dataValues, odmDataset)
  }
  
  dataValues
}

# .roundByVariableCode ---------------------------------------------------------
.roundByVariableCode <- function(dataValues, variableCodes, roundToDigits = NULL)
{
  if (!is.null(roundToDigits)) {
    
    digitNames <- names(roundToDigits)
    
    # if roundToDigits is not a named vector, create a named vector by recycling 
    # "roundToDigits" to the length of variableCodes and giving the 
    # variableCodes as names
    if (is.null(digitNames)) {
      roundToDigits <- recycle(roundToDigits, length(variableCodes))
      names(roundToDigits) <- variableCodes
    }
    else {
      variableCodes <- digitNames
    }
      
    for (variableCode in variableCodes) {
      
      digits <- roundToDigits[variableCode]
      
      cat("Rounding", variableCode, "to", digits, "digits ... ")
      
      selected <- dataValues$VariableCode == variableCode
      
      dataValues$DataValue[selected] <- round(
        x = dataValues$DataValue[selected], 
        digits = digits
      )
      
      cat("ok.\n")
    }
  }
  
  dataValues
}

# .removeIdColumns -------------------------------------------------------------
.removeIdColumns <- function(dataValues) 
{
  cat("Removing ID columns... ")
  dataValues <- removeColumns(dataValues, c("SiteID", "VariableID"))
  cat("ok.\n")  
  
  dataValues
}

# .reshapeToWideView -----------------------------------------------------------
.reshapeToWideView <- function
(
  dataValues
)
{
  timeColumns <- .availableTimeColumns(names(dataValues))
  
  cat("Reshaping to 'wide' view (step 1) ... ")
  dataMatrix.1 <- reshape(
    dataValues, 
    direction = "wide",
    idvar = c(timeColumns, "VariableCode"),
    timevar = "SiteCode",
  )
  cat("ok.\n")
  
  cat("Reshaping to 'wide' view (step 2) ... ")
  dataMatrix.2 <- reshape(
    dataMatrix.1,
    direction = "wide",
    idvar = timeColumns,
    timevar = "VariableCode"
  )
  cat("ok.\n")
  
  dataMatrix.2
}

# .renameSitesAndVariables -----------------------------------------------------
.renameSitesAndVariables <- function(dataValues, siteCodes, variableCodes)
{
  cat("Renaming Site codes and Variable codes ... ")
  
  dataValues$SiteCode <- multiSubstitute(
    strings = dataValues$SiteCode, 
    replacements = .toShortNameReplacements(siteCodes)
  )
  
  dataValues$VariableCode <- multiSubstitute(
    strings = dataValues$VariableCode, 
    replacements = .toShortNameReplacements(variableCodes)
  )

  cat("ok.\n")
  
  dataValues
}

# .renameDataValueColumns ------------------------------------------------------
.renameDataValueColumns <- function
(
  dataValues.wide, 
  variableShortNames = NULL, 
  siteShortNames = NULL
)
{
  cat("Renaming DataValue columns ... ")  
  names(dataValues.wide) <- gsub("DataValue\\.", "", names(dataValues.wide))  
  cat("ok.\n")
  
  dataValues.wide
}

# .sortRowsAndColumns ----------------------------------------------------------
.sortRowsAndColumns <- function(dataValues.wide)
{
  columnNames <- names(dataValues.wide)
  timeColumns <- .availableTimeColumns(columnNames)
  
  sortedColumns <- c(
    timeColumns, 
    sort(columnNames[-(columnNames %in% timeColumns)])
  )
  
  sortedRows <- order(dataValues.wide$DateTimeUTC)
  
  dataValues.wide[sortedRows, sortedColumns]
}

# .availableTimeColumns --------------------------------------------------------
.availableTimeColumns <- function(columnNames)
{
  intersect(columnNames, c("DateTimeUTC", "LocalDateTime", "UTCOffset"))
}
