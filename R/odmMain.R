# sqlSourceForResolvedOdmDataValues --------------------------------------------
sqlSourceForResolvedOdmDataValues <- function # sqlSourceForResolvedOdmDataValues
### sqlSourceForResolvedOdmDataValues
(
)
{
  startQuery <- structure( 
    sqlForSelect(
      tablename = structure("VariableGroups", alias = "t1"),
      whereClause = "VariableGroupName <> 'OGRE'"
    ),
    alias = "VariableGroups"
  )
  
  sql <- sqlJoinExpression(
    left = "Units", 
    right = sqlJoinExpression(
      left = startQuery, 
      right = sqlJoinExpression(
        left = sqlJoinExpression(
          left = "Variables", 
          right = sqlJoinExpression(
            left = "Sites", 
            right = sqlJoinExpression(
              left = "Samples", 
              right = "DataValues",
              condition = "Samples.SampleID = DataValues.SampleID"
            ),
            condition = "Sites.SiteID = DataValues.SiteID"
          ),
          condition = "Variables.VariableID = DataValues.VariableID"
        ),
        right = "VariableGroupVariable",
        condition = "Variables.VariableID = VariableGroupVariable.VariableID"
      ),
      condition = "VariableGroups.VariableGroupID = VariableGroupVariable.VariableGroupID"
    ),
    condition = "Units.UnitsID = Variables.VariableUnitsID"
  )
  
  sql
}

# sqlSourceForResolvedOdmDataValues.2 ------------------------------------------
sqlSourceForResolvedOdmDataValues.2 <- function # sqlSourceForResolvedOdmDataValues.2
### sqlSourceForResolvedOdmDataValues.2
(
  condition = "TRUE"
)
{
  startQuery <- sqlForSelect(
    tablename = structure("DataValues", alias = "t1"),
    whereClause = condition
  )
  
  sql <- structure(startQuery, alias = "DataValues")
  
  tablesAndIds <- c(
    Sites = "SiteID",
    Variables = "VariableID",
    OffsetTypes = "OffsetTypeID",
    Qualifiers = "QualifierID",
    Methods = "MethodID",
    Sources = "SourceID",
    Samples = "SampleID",
    QualityControlLevels = "QualityControlLevelID"
  )
  
  for (i in seq_len(length(tablesAndIds))) {
    
    tableName <- names(tablesAndIds)[i]
    fieldName <- tablesAndIds[i]
    
    sql <- sqlLeftJoinExpression(
      left = sql, 
      right = tableName,
      condition = .sqlEquals(
        .sqlFieldIdentifier(fieldName, "DataValues"), 
        .sqlFieldIdentifier(fieldName, tableName) 
      )
    )
  }
  
  .addSqlType(sql, "joined table")
}

# odmFieldList -----------------------------------------------------------------
odmFieldList <- function # odmFieldList
### odmFieldList
(
  variant = 1
)
{
  fieldSet1 <- "VariableGroups.VariableGroupName, 
    DataValues.ValueID, 
    DataValues.LocalDateTime, 
    DataValues.DataValue, 
    DataValues.CensorCode,
    Sites.SiteCode, 
    Variables.VariableCode, 
    Samples.LabSampleCode, 
    Units.UnitsAbbreviation"
  
  fieldSet2 <- "Sources.SourceID,
    Sources.Organization,
    Sources.SourceDescription"
  
  if (variant == 1) {
    fieldSet1
  }
  else if (variant == 2) {
    paste(fieldSet1, fieldSet2, sep = ",\n")
  }
}

# updateOdmSeriesCatalog -------------------------------------------------------
updateOdmSeriesCatalog <- function # update ODM's series catalog
### update ODM's series catalog
(
  db = currentDb(),
  ### name of ODBC data source. Default: "DSWT
  confirm = TRUE,
  ### if TRUE, user needs to confirm by pressing "Y"
  dbg = FALSE
)
{
  if (confirm && !.userConfirmed()) {
    return()
  }
  
  hsClearTable(db, "seriescatalog", errors = FALSE, dbg = dbg)
  
  series <- odmDataSeries(db)
  
  fieldListExpression <- commaCollapsed(names(series))
  
  if (isMySQL(db)) {
    sql <- sprintf("INSERT INTO seriescatalog(%s) VALUES %s",
                   fieldListExpression, 
                   commaCollapsed(dataFrameToSqlTuples(series)))    
  }
  else {
    
    # MS Access does not support INSERT of multiple records
    
    tableName <- "_new_SeriesCatalog_"
    
    hsPutTable(db, series, tbl = tableName, overwrite=TRUE, dbg = dbg)
    
    sql <- sprintf("INSERT INTO seriescatalog(%s) SELECT %s FROM %s",
                   fieldListExpression, fieldListExpression, tableName)
  }
  
  hsSqlQuery(db, sql, dbg = dbg)
  
  # Update variable units and time units
  for (unitsType in c("Variable", "Time")) {
    
    sql <- paste(
      "UPDATE",
      sqlJoinExpression(
        left = "seriescatalog", 
        right = "units", 
        condition = sprintf("units.unitsID = seriescatalog.%sunitsID", unitsType),
        type = "left"
      ),
      sprintf("SET seriescatalog.%sunitsName = units.unitsName", unitsType)
    )
    
    hsSqlQuery(db, sql = sql, dbg = dbg)    
  }
}

# .userConfirmed ---------------------------------------------------------------
.userConfirmed <- function()
{
  prompt <- "Do you really want to rewrite the SeriesCatalog (Y, n)? "
  answer <- readline(prompt = prompt)
  answer == "Y"  
}

# odmDataSeries ----------------------------------------------------------------
odmDataSeries <- function # odmDataSeries
### odmDataSeries
(
  db = currentDb(),
  ### ODBC database name
  dbg = FALSE
)
{
  # names of fields that are required for the series catalog. 
  catalogFields <- hsFields(db, "seriescatalog")
  
  # Variable units and time units will be resolved in a separate step
  ignoreFields <- c("SeriesID", "VariableUnitsName", "TimeUnitsName")
  ignore <- tolower(catalogFields) %in% tolower(ignoreFields)
  catalogFields <- catalogFields[!ignore]  
  
  tablesAndIDs <- c(
    sites = "SiteID", 
    variables = "VariableID",
    methods = "MethodID",
    sources = "SourceID", 
    qualitycontrollevels = "QualityControlLevelID"
  )
  
  hsSqlQuery(
    mdb = db, 
    sql = sqlLeftJoin(
      sqlSource = .sqlForOdmDataSeries(), 
      tablesAndIDs = tablesAndIDs, 
      fields = commaCollapsed(
        .makeIdFieldNamesUnique(catalogFields, tablesAndIDs)
      )
    ),
    dbg = dbg, 
    stringsAsFactors = FALSE
  )
}

# .sqlForOdmDataSeries ---------------------------------------------------------
.sqlForOdmDataSeries <- function()
{
  # ODM Version 1.1 Design Specifications: "A data series consists of all the
  # data values associated with a unique site, variable, method, source, and
  # quality control level combination in the DataValues table"
  
  fields <- "SiteID,VariableID,MethodID,SourceID,QualityControlLevelID"
  
  paste(
    "SELECT",
    "Count(*) AS ValueCount,",
    "Min(LocalDateTime) AS BeginDateTime,",
    "Max(LocalDateTime) AS EndDateTime,",
    "Min(DateTimeUTC) AS BeginDateTimeUTC,",
    "Max(DateTimeUTC) AS EndDateTimeUTC,",                             
    fields,
    "FROM datavalues",
    "GROUP BY", 
    fields
  )  
}

# getKeyValuePairsForDataset ---------------------------------------------------
getKeyValuePairsForDataset <- function # getKeyValuePairsForDataset
### getKeyValuePairsForDataset
(
  available, 
  ### list as returned by \code{\link{availableOdmDatasets}}, called with
  ### \emph{resolveIDs = FALSE}
  dsid
)
{
  keyFields <- available$keyFields
  constFields <- available$constFields
  
  dsrow <- keyFields$dsid == dsid
  
  keyFields <- keyFields[dsrow, nonInfoColumns(keyFields)]
  constFields <- constFields[, nonInfoColumns(constFields)]
  
  hsDelEmptyCols(cbind(keyFields, constFields))
  ### list of key-value pairs with the names of the list elements representing
  ### the names of the fields in ODM-table "datavalues" and the list values
  ### representing the values of the corresponding fields in the data record.
}

# nonInfoColumns ---------------------------------------------------------------
nonInfoColumns <- function # nonInfoColumns
### nonInfoColumns
(
  keyFields
)
{
  setdiff(names(keyFields), 
          c("dsid", "nrows", "minLocalDateTime", "maxLocalDateTime",
            "ValueID", "DataValue", "UTCOffset", "DateTimeUTC"))
}

# getOdmDataset ----------------------------------------------------------------
getOdmDataset <- function # getOdmDataset
### getOdmDataset
(
  dsid = 0,
  ### dataset identifier according to column \emph{dsid} in list element
  ### \emph{keyFields} of \emph{dsInfo}
  from.UTC = NULL, 
  to.UTC = NULL,  
  db = currentDb(dbg = dbg), 
  ### name of ODBC data source. Default: currentDb()
  fields = "DateTimeUTC,LocalDateTime,UTCOffset,DataValue",
  ### fields to be selected from ODM table datavalues. Default:
  ### "LocalDateTime,UTCOffset,DataValue"
  dsInfo = availableOdmDatasets(db),
  ### information on available ODM datasets as returned by 
  ### \code{\link{availableOdmDatasets}}
  as.is = FALSE,
  ### passed to \code{hsSqlQuery}. If TRUE, Date and Time columns are not
  ### converted to POSIXct but stay of mode character. Default: FALSE.
  time.ordered = TRUE,
  dbg = TRUE
)
{
  stopifnot(is.list(dsInfo))  

  stopifnot("keyFields" %in% names(dsInfo))
  
  keyFields <- dsInfo$keyFields
  
  dsid <- userSelectedDatasetID(keyFields, dsid, dsInfo)
  
  dsInfoRow <- keyFields[keyFields$dsid == dsid, nonInfoColumns(keyFields)]  
  
  timeCondition <- hsSqlExTimeCond(
    tsField = "DateTimeUTC", 
    dateFirst = as.Date(from.UTC), 
    dateLast = as.Date(to.UTC)
  )
  
  sql <- sqlForSelect(
    tablename = "datavalues", 
    fields = fields, 
    whereClause = paste(
      keyValuesToSqlFilter(dsInfoRow), 
      timeCondition, 
      sep = " AND "
    ),
    orderBy = ifelse(time.ordered, "DateTimeUTC", "")
  )
  
  hsSqlQuery(dsInfo$db, sql, as.is = as.is, dbg = dbg)
}

# availableOdmDatasets ---------------------------------------------------------
availableOdmDatasets <- function # availableDatasets
### availableDatasets
(
  db = currentDb(), 
  resolveIDs = TRUE,
  use2007Driver = NULL, 
  ### will be passed to \code{getSqlDialect}
  dbg = FALSE
)
{
  dataValueFields <- names(dbSchema_ODM()$tables$DataValues$fields)
  
  exclude <- c("ValueID", "DataValue", 
               "LocalDateTime", "DateTimeUTC", "UTCOffset","SampleID")
  
  fields <- commaCollapsed(setdiff(dataValueFields, exclude))
  
  fieldValues <- hsSqlQuery(
    mdb = db, 
    sql = sqlForSelect(
      tablename = "datavalues", 
      fields = paste(
        "Count(*) AS nrows,",
        "Min(LocalDateTime) AS minLocalDateTime,",
        "Max(LocalDateTime) AS maxLocalDateTime,",
        fields
      ),
      groupBy = fields,
      sqlDialect = getSqlDialect(db = db, use2007Driver = use2007Driver)
    ), 
    dbg = dbg, 
    stringsAsFactors = FALSE
  )
  
  n <- nrow(fieldValues)
  
  if (n == 0) {
    warning("*** There are no datasets at all in ", db, "!")
    return (NULL)
  }
  
  constValues <- getConstFieldInfo(fieldValues)
  
  colsSelected <- !(names(fieldValues) %in% names(constValues))
  
  keyFields <- cbind(
    dsid = seq_len(nrow(fieldValues)), 
    fieldValues[, colsSelected]
  )
  
  if (resolveIDs) {
    keyFields <- resolveForeignKeys(keyFields, db = db)
  }
  
  list(keyFields = keyFields, constFields = constValues, db = db)
  ### list with elements \emph{keyFields}, \emph{constFields}, \emph{db}
}

# resolveForeignKeys -----------------------------------------------------------
resolveForeignKeys <- function # resolveForeignKeys
### resolveForeignKeys
(
  dataFrame,
  ### data frame
  db = currentDb(),
  ### database. Default: \code{currentDb}
  idsToResolve = c("SiteID", "VariableID", "MethodID", "SourceID"), # names(.getForeignKeyInfo()),
  ### names of foreigen key fields to be resolved. Default:
  ### \code{names(.getForeignKeyInfo())}
  dbg = FALSE
)
{
  # save original, non-informational columns
  columnNames <- nonInfoColumns(dataFrame)
  
  foreignKeyInfos <- .getForeignKeyInfo()
  
  # Resolve foreign key fields for which information are available
  idFields <- intersect(columnNames, idsToResolve)
  
  for (idField in idFields) {
    
    cat("Resolving", idField, "...\n")
    
    foreignKeyInfo <- foreignKeyInfos[[idField]]

    foreignIdField <- foreignKeyInfo$idField
    
    if (is.null(foreignIdField)) {
      foreignIdField <- idField
    }
    
    odmObjects <- do.call(
      what = foreignKeyInfo$FUN, 
      args = list(
        select = commaCollapsed(c(idField, foreignKeyInfo$select)), 
        db = db,
        dbg = dbg
      )
    )

    dataFrame <- merge(dataFrame, odmObjects, by = idField, all.x = TRUE)
  }
    
  if (!is.null(dataFrame$dsid)) {
    columns <- setdiff(names(dataFrame), columnNames)
    dataFrame <- dataFrame[order(dataFrame$dsid), columns]
  }
  
  dataFrame
}

# .getForeignKeyInfo -----------------------------------------------------------
.getForeignKeyInfo <- function()
{
  list(
    SiteID = list(
      FUN = "odm_Sites", 
      select = "SiteCode"
    ),
    VariableID = list(
      FUN = "odm_Variables", 
      select = "VariableCode"
    ),
    OffsetTypeID = list(
      FUN = "odm_OffsetTypes", 
      select = "OffsetDescription"
    ),
    QualifierID = list(
      FUN = "odm_Qualifiers", 
      select = "QualifierCode"
    ),
    MethodID = list(
      FUN = "odm_Methods", 
      select = "MethodDescription"
    ),
    SourceID = list(
      FUN = "odm_Sources", 
      select = "SourceDescription"
    ),
    SampleID = list(
      FUN = "odm_Samples", 
      select = "LabSampleCode"
    ),
    QualityControlLevelID = list(
      FUN = "odm_QualityControlLevels",
      select = "QualityControlLevelCode"
    )
  )
}

# getConstFieldInfo ------------------------------------------------------------
getConstFieldInfo <- function # getConstFieldInfo
### getConstFieldInfo
(
  fieldValues
  ### data frame
) 
{
  pairs <- list()
  
  for (columnName in names(fieldValues)) {
    
    uniqueValues <- unique(fieldValues[[columnName]])
    
    if (length(uniqueValues) == 1) {
      pairs[[columnName]] <- uniqueValues
    }    
  }
  
  as.data.frame(pairs)
  ### data frame with one row and as many columns as there are columns in 
  ### \emph{fieldValues} in which all the values are equal
}

# userSelectedDatasetID --------------------------------------------------------
userSelectedDatasetID <- function # userSelectedDatasetID
### userSelectedDatasetID
(
  keyFields, 
  dsid, 
  dsInfo
)
{
  available <- datasetIdAvailable(keyFields, dsid) 
  
  if (!available) {
    cat(sprintf("Available datasets in \"%s\":\n", dsInfo$db))
    print(resolveForeignKeys(dsInfo$keyFields, db=dsInfo$db))
  }  
  
  while (!available) {
    choice <- readline("Please select a valid dataset identifier (dsid) or Enter to cancel: ")
    
    if (choice == "") {
      return()
    }
    
    dsid <- as.numeric(choice)        
    available <- !is.na(dsid) && datasetIdAvailable(keyFields, dsid) 
  }  
  
  dsid
}

# datasetIdAvailable -----------------------------------------------------------
datasetIdAvailable <- function # datasetIdAvailable
### datasetIdAvailable
(
  keyFields, 
  dsid
)  
{
  dsid %in% keyFields$dsid
}

# .makeIdFieldNamesUnique ------------------------------------------------------
.makeIdFieldNamesUnique <- function(catalogFields, tablesAndIDs)
{
  # make ID field names unique
  for (tableName in names(tablesAndIDs)) {
    id <- tablesAndIDs[tableName]
    uniqueID <- paste(tableName, id, sep = ".")
    catalogFields <- gsub(id, uniqueID, catalogFields)
  }
  catalogFields  
}
