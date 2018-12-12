# insertNewDataAndPlotBeforeAndAfter -------------------------------------------
insertNewDataAndPlotBeforeAndAfter <- function # insertNewDataAndPlotBeforeAndAfter
### insertNewDataAndPlotBeforeAndAfter
(
  keyFields, 
  dataValues, 
  variableCode = "",
  unitAbbreviation = "",
  db = currentDb(),
  version = 0, 
  dbg = FALSE
)
{
  old.par <- par(mfrow=c(2,1))
  on.exit(par(old.par))
  
  siteName <- odmSites(SiteID=keyFields$SiteID, odbc = db)$SiteName
  
  variableDescription <- sprintf("%s, %s (%s)", 
                                 siteName, variableCode, unitAbbreviation)
  
  rereadFromDataValuesTableAndPlot(
    keyFields, db = db, main = paste("Last status of:", variableDescription))
  
  timeRange <- range(as.POSIXct(dataValues$LocalDateTime))
  cat(sprintf("Time range of data to insert: %s - %s\n",
              timeRange[1], timeRange[2]))
  
  insertNewData(dataValues, keyFields, version = version, db = db, 
                errors = FALSE, dbg = dbg)
  
  rereadFromDataValuesTableAndPlot(
    keyFields, db = db, main = paste("Current status of:", variableDescription))
}

# rereadFromDataValuesTableAndPlot ---------------------------------------------
rereadFromDataValuesTableAndPlot <- function # rereadFromDataValuesTableAndPlot
### rereadFromDataValuesTableAndPlot
(
  keyFields, db, ...
) 
{
  x <- rereadFromDataValuesTable(keyFields, db)
  
  if (nrow(x) > 0) {
    plotOdmDataValues(x, ...)    
  }
  else {
    cat(sprintf("\n\n*** No current data with %s\n\n", 
                commaCollapsed(keyValuesToSqlAssignment(keyFields))))
  }
}

# rereadFromDataValuesTable ----------------------------------------------------
rereadFromDataValuesTable <- function # rereadFromDataValuesTable
### rereadFromDataValuesTable
(
  keyFields, db
)
{
  sql <- sqlForSelect("datavalues", "*", keyValuesToSqlFilter(keyFields))
  hsSqlQuery(db, sql, as.is = TRUE)
}

# plotOdmDataValues ------------------------------------------------------------
plotOdmDataValues <- function # plotOdmDataValues
### plotOdmDataValues
(
  dataValues, noDataValue = -9999, ...
)
{
  x <- hsToPosix(dataValues$DateTimeUTC)
  y <- dataValues$DataValue
  
  # replace "noDataValue" with NA
  missingValue <- which(y == noDataValue)
  
  y[missingValue] <- NA
  
  plot(x, y, xlab="DateTimeUTC", ylab="DataValue", cex=0.4, ...)
  
  # indicate missing values by vertical grey line
  abline(v = x[missingValue], col = "grey")
}

# insertNewData ----------------------------------------------------------------
insertNewData <- function # insertNewData
### insertNewData
(
  newData, 
  keyFields, 
  version = 1,
  ### version (default = 1) < 3: data import via temporary table
  ### version >= 3: data import directly into DataValues table
  ### (version == 3: by using keyFields; 
  ### version > 3: by setting SourceID to 999)
  db = currentDb(),
  ### database. Default: currentDb()
  whereClauseType = 2,
  ### whereClauseType == 0: whereClause = "TRUE";
  ### whereClauseType == 1: whereClause = "DateTimeUTC NOT IN (<existingTimestamps>)";
  ### whereClauseType == 2: whereClause = "DateTimeUTC > <lastDateInDataValuesTable>";
  ### Default: 2
  ...
  ### further arguments passed to \code{\link{insertTemporaryTableIntoDataValuesTable}}
)
{
  if (version < 3) {
    
    writeToTemporaryTable(newData, db = db, version = version, dbg = FALSE)
    
    insertTemporaryTableIntoDataValuesTable(
      keyFields, whereClauseType = whereClauseType, db = db, ...)
    
    #hsDropTable(db, tbl = "_tmp_")
  } 
  else {
    cat("insert directly into datavalues table...\n")
    
    if (version > 3) {
      myData <- data.frame(newData, SourceID = 999)
    }
    else {
      myData <- data.frame(newData, keyFields)
    }
    
    insertDataFrameIntoDataValuesTable(myData, db)
    
    if (version > 3) {
      updateKeyFields(keyFields, whereClause = "SourceID=999", db = db)
      deleteDataValuesWithKeyFields(list(SourceID = 999), db = db)
    }    
  }
}

# writeToTemporaryTable --------------------------------------------------------
writeToTemporaryTable <- function # writeToTemporaryTable
### writeToTemporaryTable
(
  newData, 
  db = currentDb(), 
  tablename = "_tmp_", 
  ### table name. Default: "_tmp_"
  version = 1,
  ### version == 1 (default): temporary table is filled by one SQL INSERT;
  ### version == 2: temporary table is created by using hsPutTable
  reread = FALSE, 
  ### if TRUE, data is reread from table and returned. Default: FALSE
  dbg,
  ### if TRUE, debug messages are shown
  ...
  ### further arguments passed to \code{\link{sqlForInsertDataFrame}}, e.g.
  ### \emph{ignore} or hsPutTable (if version == 2)
)
{
  # old version used hsPutTable -> one INSERT per row -> very slow!  
  # this version clears _tmp_ and uses one big INSERT -> much faster!  
  
  stopifnot(version %in% 1:2)
  
  if (version == 1) {

    if (isMySQL(db)) {
      sql <- paste("SELECT * FROM", tablename, "LIMIT 1")
    }
    else {
      sql <- paste("SELECT TOP 1 * FROM", tablename)      
    }
    
    firstTableLine <- hsSqlQuery(db, sql)
    
    if (nrow(firstTableLine) > 0) {
      hsClearTable(db, tablename)
    }
    
    sql <- sqlForInsertDataFrame(tablename, newData, ...)
    
    mySystemTime(hsSqlQuery, 
                 args = list(db, sql, dbg = FALSE))    
  }
  else {
    cat("*** Creating table", tablename, "...\n")
    mySystemTime(hsPutTable, 
                 args = list(db, newData, tablename, overwrite = TRUE, 
                             dbg = dbg, ...))
  }
  
  if (reread) {
    hsGetTable(db, tablename, as.is = TRUE)
  }  
}

# insertTemporaryTableIntoDataValuesTable --------------------------------------
insertTemporaryTableIntoDataValuesTable <- function # insertTemporaryTableIntoDataValuesTable
### insertTemporaryTableIntoDataValuesTable
(
  keyFields, 
  whereClauseType, 
  db = currentDb(), 
  ...
  ### passed to \code{\link{hsSqlQuery}}
)
{
  mainFields <- "LocalDateTime,UTCOffset,DateTimeUTC,DataValue"
  
  targetFieldList <- commaCollapsed(c(mainFields, names(keyFields)))
  sourceFieldList <- commaCollapsed(c(mainFields, listValuesToSql(keyFields)))
  
  whereClause <- whereClauseNonExistingTimestamps(keyFields, whereClauseType, 
                                                  db = db)
  
  sqlSource <- sqlForSelect("_tmp_", sourceFieldList, whereClause)
  
  ignore <- isMySQL(db)
  sql <- sqlForInsert("datavalues", targetFieldList, sqlSource, ignore = ignore)
  
  mySystemTime(hsSqlQuery, args = list(db, sql, ...))
}

# whereClauseNonExistingTimestamps ---------------------------------------------
whereClauseNonExistingTimestamps <- function # whereClauseNonExistingTimestamps
### whereClauseNonExistingTimestamps
(
  keyFields, whereClauseType, db = currentDb()
)
{
  if (whereClauseType == 0) {
    whereClause <- "TRUE"
  }
  else if (whereClauseType == 1) {
    
    sqlSource <- sqlForSelectByKey("datavalues", "DateTimeUTC", keyFields)
    
    whereClause <- sprintf("DateTimeUTC NOT IN (%s)", sqlSource)
  } 
  else if (whereClauseType == 2) {
    
    maxDate <- lastDateInDataValuesTable(keyFields, db = db)
    
    if (is.na(maxDate)) {      
      whereClause <- "TRUE"      
    } 
    else {      
      whereClause <- sprintf("DateTimeUTC > '%s'", maxDate)      
    }
  }
  
  return (whereClause)
}

# lastDateInDataValuesTable ----------------------------------------------------
lastDateInDataValuesTable <- function # lastDateInDataValuesTable
### lastDateInDataValuesTable
(
  keyFields, db = currentDb()
) 
{
  sql <- sqlForSelectByKey("datavalues", "Max(DateTimeUTC) AS maxDate", keyFields)
  
  hsSqlQuery(db, sql, dbg = FALSE, as.is = TRUE)$maxDate
}

# insertDataFrameIntoDataValuesTable -------------------------------------------
insertDataFrameIntoDataValuesTable <- function # insertDataFrameIntoDataValuesTable
### insertDataFrameIntoDataValuesTable
(
  myData, db, ignore = TRUE, dbg = FALSE
)
{
  sql <- sqlForInsertDataFrame(tablename = "datavalues", myData, ignore=ignore)  
  
  mySystemTime(hsSqlQuery, args = list(db, sql, dbg = dbg))
}

# updateKeyFields --------------------------------------------------------------
updateKeyFields <- function # updateKeyFields
### updateKeyFields
(
  keyFields, whereClause, db = currentDb()
)
{
  sql <- sqlForUpdate("datavalues", keyFields, whereClause, ignore = TRUE)
  
  mySystemTime(hsSqlQuery, args = list(db, sql))    
}

# deleteDataValuesWithKeyFields ------------------------------------------------
deleteDataValuesWithKeyFields <- function # deleteDataValuesWithKeyFields
### deleteDataValuesWithKeyFields
(
  keyFields, db = currentDb(), warn = TRUE
)
{
  myData <- do.call("odmDatavalues", c(keyFields, odbc = db, dbg = FALSE))
  
  valueIDs <- myData$ValueID
  
  if (length(valueIDs) > 0) {
    odmDeleteDataValues(db, valueIDs, warn = warn, dbg = FALSE)
  } 
  else {
    cat("Nothing to delete.\n")
  }
}
