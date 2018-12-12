# create_ODM_objects -----------------------------------------------------------
create_ODM_objects <- function # create_ODM_objects
### create_ODM_objects
(
  objects, 
  ### data frame with each row representing an ODM object and the columns
  ### named as the columns in the ODM tables
  FUN, 
  ### function used to create the ODM objects, must accept arguments named
  ### as columns are named in \emph{objects}
  db
  ### full path to or ODBC name of ODM database
) 
{
  for (i in seq_len(nrow(objects))) {
    do.call(FUN, args = c(objects[i, , drop = FALSE], db = db))
  }    
}

# createNewSampleType ----------------------------------------------------------
createNewSampleType <- function # createNewSampleType
### createNewSampleType
(
  Term, 
  Definition,
  db = currentDb()
)
{
  keyValues <- list(
    Term = Term, 
    Definition = Definition
  )
  
  sqlExpressions <- keyValuesToSqlAssignment2(keyValues)
  
  hsSqlQuery(db, sqlForInsert("sampletypecv", sqlExpressions$fieldList, 
                              paste("(", sqlExpressions$valueList, ")")))
}

# createNewUnit ----------------------------------------------------------------
createNewUnit <- function # createNewUnit
### createNewUnit
(
  UnitsName, 
  UnitsType,
  UnitsAbbreviation,
  db = currentDb()
)
{
  keyValues <- list(
    UnitsName = UnitsName, 
    UnitsType = UnitsType,
    UnitsAbbreviation = UnitsAbbreviation
  )
  
  sqlExpressions <- keyValuesToSqlAssignment2(keyValues)
  
  hsSqlQuery(db, sqlForInsert("units", sqlExpressions$fieldList, 
                              paste("(", sqlExpressions$valueList, ")")))
}

# createNewSite ----------------------------------------------------------------
createNewSite <- function # createNewSite
### createNewSite
(
  SiteCode, 
  SiteName, 
  Latitude, 
  Longitude, 
  LatLongDatumID,
  Elevation_m = NA,
  VerticalDatum = NA,
  LocalX = NA,
  LocalY = NA,
  LocalProjectionID = NA,
  PosAccuracy_m = NA,
  State = NA,
  County = NA,
  Comments = NA,
  db = currentDb()
)
{
  keyValues <- list(  SiteCode = SiteCode, 
                      SiteName = SiteName, 
                      Latitude = Latitude, 
                      Longitude = Longitude, 
                      LatLongDatumID = LatLongDatumID,
                      Elevation_m = Elevation_m,
                      VerticalDatum = VerticalDatum,
                      LocalX = LocalX,
                      LocalY = LocalY,
                      LocalProjectionID = LocalProjectionID,
                      PosAccuracy_m = PosAccuracy_m,
                      State = State,
                      County = County,
                      Comments = Comments)
  
  sqlExpressions <- keyValuesToSqlAssignment2(keyValues)  
  
  hsSqlQuery(db, sqlForInsert("sites", sqlExpressions$fieldList, 
                              paste("(", sqlExpressions$valueList, ")")))
}

# createNewVariable ------------------------------------------------------------
createNewVariable <- function # createNewVariable
### createNewVariable
(
  VariableCode, 
  VariableName, 
  Speciation = "Not Applicable", 
  VariableUnitsID, 
  SampleMedium = "Unknown",
  ValueType = "Unknown",
  IsRegular = FALSE,
  TimeSupport = 0, # instantaneous samples
  TimeUnitsID = 103, # hours
  DataType = "Unknown",
  GeneralCategory = "Unknown",
  NoDataValue = -9999,
  db = currentDb()
)
{
  keyValues <- list(VariableCode = VariableCode, 
                    VariableName = VariableName, 
                    Speciation = Speciation, 
                    VariableUnitsID = VariableUnitsID, 
                    SampleMedium = SampleMedium,
                    ValueType = ValueType,
                    IsRegular = IsRegular,
                    TimeSupport = TimeSupport,
                    TimeUnitsID = TimeUnitsID, 
                    DataType = DataType,
                    GeneralCategory = GeneralCategory,
                    NoDataValue = NoDataValue)
  
  sqlExpressions <- keyValuesToSqlAssignment2(keyValues)  
  
  hsSqlQuery(db, sqlForInsert("variables", sqlExpressions$fieldList, 
                              paste("(", sqlExpressions$valueList, ")")))
}

# createNewMethod --------------------------------------------------------------
createNewMethod <- function # createNewMethod
### createNewMethod
(
  MethodDescription, MethodLink=NA, db = currentDb()
)
{
  keyValues <- list(MethodDescription=MethodDescription,
                    MethodLink=MethodLink)
  
  sqlExpressions <- keyValuesToSqlAssignment2(keyValues)  
  
  hsSqlQuery(db, sqlForInsert("methods", sqlExpressions$fieldList, 
                              paste("(", sqlExpressions$valueList, ")")))
}

# createNewLabMethod --------------------------------------------------------------
createNewLabMethod <- function # createNewLabMethod
### createNewLabMethod
(
  LabName, 
  LabOrganization = "Unknown", 
  LabMethodName = "Unknown", 
  LabMethodDescription = "Unknown", 
  LabMethodLink = NA,
  db = currentDb()
)
{
  keyValues <- list(LabName = LabName,
                    LabOrganization = LabOrganization, 
                    LabMethodName = LabMethodName, 
                    LabMethodDescription = LabMethodDescription, 
                    LabMethodLink = LabMethodLink
  )
  
  sqlExpressions <- keyValuesToSqlAssignment2(keyValues)  
  
  hsSqlQuery(db, sqlForInsert("labmethods", sqlExpressions$fieldList, 
                              paste("(", sqlExpressions$valueList, ")")))
}

# createNewSource --------------------------------------------------------------
createNewSource <- function # createNewSource
### createNewSource
(
  Organization, SourceDescription, SourceLink=NA, ContactName="Unknown",
  Phone="Unknown", Email="Unknown", Address="Unknown", City="Unknown",
  State="Unknown", ZipCode="Unknown", Citation="Unknown", MetadataID=0, 
  db = currentDb()
)
{
  keyValues <- list(Organization = Organization,
                    SourceDescription = SourceDescription,
                    SourceLink = SourceLink,
                    ContactName = ContactName,
                    Phone = Phone, 
                    Email = Email,
                    Address = Address,
                    City = City,
                    State = State,
                    ZipCode = ZipCode,
                    Citation = Citation,
                    MetadataID = MetadataID)
  
  sqlExpressions <- keyValuesToSqlAssignment2(keyValues)  
  
  hsSqlQuery(db, sqlForInsert("sources", sqlExpressions$fieldList, 
                              paste("(", sqlExpressions$valueList, ")")))
}

# createNewDataValue -----------------------------------------------------------
createNewDataValue <- function # createNewDataValue
### createNewDataValue
(
  DataValue,
  ### The numeric value of the observation. For Categorical variables, a number
  ### is stored here. The Variables table has DataType as Categorical and the
  ### Categories table maps from the DataValue onto Category Description.
  ### Example: 34.5
  ValueAccuracy=NA,
  ### Numeric value that describes the measurement accuracy of the data value.
  ### If not given, it is interpreted as unknown.
  LocalDateTime=DateTimeUTC,
  ### Local date and time at which the data value was observed. Represented in
  ### an implementation specific format. Example: 9/4/2003 7:00:00 AM
  UTCOffset=0,
  ### Offset in hours from UTC time of the corresponding LocalDateTime value.
  ### Example: -7
  DateTimeUTC=LocalDateTime,
  ### Universal UTC date and time at which the data value was observed.
  ### Represented in an implementation specific format. Example: 9/4/2003
  ### 2:00:00 PM
  SiteID=NA,
  ### Integer identifier that references the site at which the observation was
  ### measured. This links data values to their locations in the Sites table.
  ### See also \code{\link{odmSites}}.
  VariableID=NA,
  ### Integer identifier that references the variable that was measured. This
  ### links data values to their variable in the Variables table. See also
  ### \code{\link{odmVariables}}.
  OffsetValue=NA,
  ### Distance from a datum or control point to the point at which a data value
  ### was observed. If not given the OffsetValue is inferred to be 0, or not
  ### relevant/necessary. Example: 2.1
  OffsetTypeID=NA,
  ### Integer identifier that references the measurement offset type in the
  ### OffsetTypes table. See also \code{\link{odmOffsetTypes}}.
  CensorCode="nc",
  ### Text indication of whether the data value is censored from the
  ### CensorCodeCV controlled vocabulary. Example: "nc"
  QualifierID=NA,
  ### Integer identifier that references the Qualifiers table. If Null, the data
  ### value is inferred to not be qualified. See also
  ### \code{\link{odmQualifiers}}.
  MethodID=0,
  ### Integer identifier that references method used to generate the data value
  ### in the Methods table. See also \code{\link{odmMethods}}.
  SourceID=NA,
  ### Integer identifier that references the record in the Sources table giving
  ### the source of the data value. See also \code{\link{odmSources}}.
  SampleID=NA,
  ### Integer identifier that references into the Samples table. This is
  ### required only if the data value resulted from a physical sample processed
  ### in a lab. See also \code{\link{odmSamples}}.
  DerivedFromID=NA,
  ### Integer identifier for the derived from group of data values that the
  ### current data value is derived from. This refers to a group of derived from
  ### records in the DerivedFrom table. If NULL, the data value is inferred to
  ### not be derived from another data value.
  QualityControlLevelID=-9999,
  ### Integer identifier giving the level of quality control that the value has
  ### been subjected to. This references the QualityControlLevels table. See
  ### also \code{\link{odmQualityControlLevels}}.
  db = currentDb()
)
{
  keyValues <- list(DataValue = DataValue,
                    ValueAccuracy = ValueAccuracy,
                    LocalDateTime = LocalDateTime,
                    UTCOffset = UTCOffset,
                    DateTimeUTC = DateTimeUTC,
                    SiteID = SiteID,
                    VariableID = VariableID,
                    OffsetValue = OffsetValue,
                    OffsetTypeID = OffsetTypeID,
                    CensorCode = CensorCode,
                    QualifierID = QualifierID,
                    MethodID = MethodID,
                    SourceID = SourceID,
                    SampleID = SampleID,
                    DerivedFromID = DerivedFromID,
                    QualityControlLevelID = QualityControlLevelID
  )
  
  sqlExpressions <- keyValuesToSqlAssignment2(keyValues)  
  
  hsSqlQuery(db, sqlForInsert("datavalues", sqlExpressions$fieldList, 
                              paste("(", sqlExpressions$valueList, ")")))
}
