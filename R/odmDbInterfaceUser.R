# odmDataValues2 ---------------------------------------------------------------
odmDataValues2 <- function # call odmDataValues with arguments from list
### call odmDataValues with arguments from list
(
  argumentList
)
{
  stopifnot(is.list(argumentList))  
  do.call(odmDatavalues, argumentList)
}

# odmDatavalues ----------------------------------------------------------------
odmDatavalues <- function # odmDatavalues
### fetch and filter data from ODM table "datavalues"
(
  ValueID=NULL,
  ### Unique integer identifier for each data value.
  DataValue=NULL,
  ### The numeric value of the observation. For Categorical variables, a number
  ### is stored here. The Variables table has DataType as Categorical and the
  ### Categories table maps from the DataValue onto Category Description.
  ### Example: 34.5
  ValueAccuracy=NULL,
  ### Numeric value that describes the measurement accuracy of the data value.
  ### If not given, it is interpreted as unknown.
  LocalDateTime=NULL,
  ### Local date and time at which the data value was observed. Represented in
  ### an implementation specific format. Example: 9/4/2003 7:00:00 AM
  UTCOffset=NULL,
  ### Offset in hours from UTC time of the corresponding LocalDateTime value.
  ### Example: -7
  DateTimeUTC=NULL,
  ### Universal UTC date and time at which the data value was observed.
  ### Represented in an implementation specific format. Example: 9/4/2003
  ### 2:00:00 PM
  SiteID=NULL,
  ### Integer identifier that references the site at which the observation was
  ### measured. This links data values to their locations in the Sites table.
  ### See also \code{\link{odmSites}}.
  VariableID=NULL,
  ### Integer identifier that references the variable that was measured. This
  ### links data values to their variable in the Variables table. See also
  ### \code{\link{odmVariables}}.
  OffsetValue=NULL,
  ### Distance from a datum or control point to the point at which a data value
  ### was observed. If not given the OffsetValue is inferred to be 0, or not
  ### relevant/necessary. Example: 2.1
  OffsetTypeID=NULL,
  ### Integer identifier that references the measurement offset type in the
  ### OffsetTypes table. See also \code{\link{odmOffsetTypes}}.
  CensorCode=NULL,
  ### Text indication of whether the data value is censored from the
  ### CensorCodeCV controlled vocabulary. Example: "nc"
  QualifierID=NULL,
  ### Integer identifier that references the Qualifiers table. If Null, the data
  ### value is inferred to not be qualified. See also
  ### \code{\link{odmQualifiers}}.
  MethodID=NULL,
  ### Integer identifier that references method used to generate the data value
  ### in the Methods table. See also \code{\link{odmMethods}}.
  SourceID=NULL,
  ### Integer identifier that references the record in the Sources table giving
  ### the source of the data value. See also \code{\link{odmSources}}.
  SampleID=NULL,
  ### Integer identifier that references into the Samples table. This is
  ### required only if the data value resulted from a physical sample processed
  ### in a lab. See also \code{\link{odmSamples}}.
  DerivedFromID=NULL,
  ### Integer identifier for the derived from group of data values that the
  ### current data value is derived from. This refers to a group of derived from
  ### records in the DerivedFrom table. If NULL, the data value is inferred to
  ### not be derived from another data value.
  QualityControlLevelID=NULL,
  ### Integer identifier giving the level of quality control that the value has
  ### been subjected to. This references the QualityControlLevels table. See
  ### also \code{\link{odmQualityControlLevels}}.
  fields="ValueID,SiteID,MethodID,VariableID,LocalDateTime,UTCOffset,DateTimeUTC,DataValue",
  charAsPattern=TRUE,
  odbc = currentDb(),
  dbg = TRUE
) 
{
  arglist <- list(
    ValueID=ValueID,
    DataValue=DataValue,
    ValueAccuracy=ValueAccuracy,
    LocalDateTime=LocalDateTime,
    UTCOffset=UTCOffset,
    DateTimeUTC=DateTimeUTC,
    SiteID=SiteID,
    VariableID=VariableID,
    OffsetValue=OffsetValue,
    OffsetTypeID=OffsetTypeID,
    CensorCode=CensorCode,
    QualifierID=QualifierID,
    MethodID=MethodID,
    SourceID=SourceID,
    SampleID=SampleID,
    DerivedFromID=DerivedFromID,
    QualityControlLevelID=QualityControlLevelID)
  
  getFilteredRecords(odbc, "datavalues", arglist, fields, charAsPattern, dbg=dbg, as.is=TRUE)
}

# odmVariables -----------------------------------------------------------------
odmVariables <- function # odmVariables
### fetch and filter data from ODM table "variables"
(
  VariableID=NULL,
  ### Unique integer identifier for each variable.
  VariableCode=NULL,
  ### Text code used by the organization that collects the data to identify the
  ### variable. Example: "00060" used by USGS for discharge
  VariableName=NULL,
  ### Full text name of the variable that was measured, observed, modeled, etc.
  ### This should be from the VariableNameCV controlled vocabulary table.
  ### Example: "Discharge"
  Speciation=NULL,
  ### Text code used to identify how the data value is expressed (i.e., total
  ### phosphorus concentration expressed as P). This should be from the
  ### SpeciationCV controlled vocabulary table. Example: "P", "N", "NO3"
  VariableunitsID=NULL,
  ### Integer identifier that references the record in the Units table giving
  ### the units of the data values associated with the variable.
  SampleMedium=NULL,
  ### The medium in which the sample or observation was taken or made. This
  ### should be from the SampleMediumCV controlled vocabulary table. Example:
  ### "Surface Water", "Sediment", "Fish Tissue"
  ValueType=NULL,
  ### Text value indicating what type of data value is being recorded. This
  ### should be from the ValueTypeCV controlled vocabulary table. Example:
  ### "Field Observation", "Laboratory Observation", "Model Simulation Results"
  IsRegular=NULL,
  ### Value that indicates whether the data values are from a regularly sampled
  ### time series. Example: "True", "False"
  TimeSupport=NULL,
  ### Numerical value that indicates the time support (or temporal footprint) of
  ### the data values. 0 is used to indicate data values that are instantaneous.
  ### Other values indicate the time over which the data values are implicitly
  ### or explicitly averaged or aggregated. Example: 0, 24
  TimeunitsID=NULL,
  ### Integer identifier that references the record in the Units table giving
  ### the Units of the time support. If TimeSupport is 0, indicating an
  ### instantaneous observation, a unit needs to still be given for
  ### completeness, although it is somewhat arbitrary.
  DataType=NULL,
  ### Text value that identifies the data values as one of several types from
  ### the DataTypeCV controlled vocabulary table. Example: "Continuous",
  ### "Sporadic", "Cumulative", "Incremental", "Average", "Minimum", "Maximum",
  ### "Constant Over Interval", "Categorical"
  GeneralCategory=NULL,
  ### General category of the data values from the GeneralCategoryCV controlled
  ### vocabulary table. Example: "Climate", "Water Quality", "Groundwater
  ### Quality"
  NoDataValue=NULL,  
  ### Numeric value used to encode no data values for this variable. Example:
  ### -9999
  fields="VariableID,VariableCode,VariableName",
  charAsPattern=TRUE, 
  odbc = currentDb(),
  dbg = TRUE
) 
{
  getFilteredRecords(odbc, "variables", 
                     list(VariableID=VariableID,
                          VariableCode=VariableCode,
                          VariableName=VariableName,
                          Speciation=Speciation,
                          VariableunitsID=VariableunitsID,
                          SampleMedium=SampleMedium,
                          ValueType=ValueType,
                          IsRegular=IsRegular,
                          TimeSupport=TimeSupport,
                          TimeunitsID=TimeunitsID,
                          DataType=DataType,
                          GeneralCategory=GeneralCategory,
                          NoDataValue=NoDataValue), 
                     fields, charAsPattern, dbg=dbg)    
}

# odmSources -------------------------------------------------------------------
odmSources <- function # odmSources
### fetch and filter data from ODM table "sources"
(
  SourceID=NULL,
  ### Unique integer identifier that identifies each data source.
  Organization=NULL,
  ### Name of the organization that collected the data. This should be the
  ### agency or organization that collected the data, even if it came out of a
  ### database consolidated from many sources such as STORET. Example: "Utah
  ### Division of Water Quality"
  SourceDescription=NULL,
  ### Full text description of the source of the data. Example: "Text file
  ### retrieved from the EPA STORET system indicating data originally from Utah
  ### Division of Water Quality"
  SourceLink=NULL,
  ### Link that can be pointed at the original data file and/or associated
  ### metadata stored in the digital library or URL of data source.
  ContactName=NULL,
  ### Name of the contact person for the data source. Example: "Jane Adams"
  Phone=NULL,
  ### Phone number for the contact person. Example: "435-797-0000"
  Email=NULL,
  ### Email address for the contact person. Example: "Jane.Adams@dwq.ut"
  Address=NULL,
  ### Street address for the contact person. Example: "45 Main Street"
  City=NULL,
  ### City in which the contact person is located. Example: "Salt Lake City"
  State=NULL,
  ### State in which the contact person is located. Use two letter
  ### abbreviations. Example: "UT"
  ### for US. For other countries give the full country name.
  ZipCode=NULL,
  ### US Zip Code or country postal code. Example: "82323"
  Citation=NULL,
  ### Text string that give the citation to be used when the data from each
  ### source are referenced. Example: "Data collected by USU as part of the
  ### Little Bear River Test Bed Project"
  MetadataID=NULL,
  ### Integer identifier referencing the record in the ISOMetadata table for
  ### this source.
  fields="SourceID,Organization,SourceDescription",
  charAsPattern=TRUE, 
  odbc = currentDb(),
  dbg=TRUE
) 
{
  getFilteredRecords(odbc, "sources", 
                     list(SourceID=SourceID,
                          Organization=Organization,
                          SourceDescription=SourceDescription,
                          SourceLink=SourceLink,
                          ContactName=ContactName,
                          Phone=Phone,
                          Email=Email,
                          Address=Address,
                          City=City,
                          State=State,
                          ZipCode=ZipCode,
                          Citation=Citation,
                          MetadataID=MetadataID), 
                     fields, charAsPattern, dbg=dbg)  
}

# odmQualityControlLevels ------------------------------------------------------
odmQualityControlLevels <- function # odmQualityControlLevels
### fetch and filter data from ODM table "qualitycontrollevels"
(
  QualityControlLevelID=NULL,
  ### Unique integer identifying the quality control level.
  QualityControlLevelCode=NULL,
  ### Code used to identify the level of quality control to which data values
  ### have been subjected. Example: "1", "1.1", "Raw", "QC Checked"
  Definition=NULL,
  ### Definition of Quality Control Level. Example: "Raw Data", "Quality
  ### Controlled Data"
  Explanation=NULL,
  ### Explanation of Quality Control Level. Example: "Raw data is defined as
  ### unprocessed data and data products that have not undergone quality
  ### control."
  fields="QualityControlLevelID,QualityControlLevelCode,Definition", 
  charAsPattern=TRUE, 
  odbc = currentDb(),
  dbg=TRUE
)
{
  getFilteredRecords(odbc, "qualitycontrollevels", 
                     list(QualityControlLevelID=QualityControlLevelID,
                          QualityControlLevelCode=QualityControlLevelCode,
                          Definition=Definition,
                          Explanation=Explanation), 
                     fields, charAsPattern, dbg=dbg)
}

# odmMethods -------------------------------------------------------------------
odmMethods <- function # odmMethods
### fetch and filter data from ODM table "methods"
(
  MethodID=NULL,
  ### Unique integer ID for each method.
  MethodDescription=NULL,
  ### Text description of each method. Example: "Specific conductance measured
  ### using a Hydrolab" or "Streamflow measured using a V notch weir with
  ### dimensions xxx"
  MethodLink=NULL,
  ### Link to additional reference material on the method.
  fields="MethodID,MethodDescription", 
  charAsPattern=TRUE, 
  odbc = currentDb(),
  dbg=TRUE
) 
{
  getFilteredRecords(odbc, "methods", 
                     list(MethodID=MethodID,
                          MethodDescription=MethodDescription,
                          MethodLink=MethodLink), 
                     fields, charAsPattern, dbg=dbg)
}

# odmOffsetTypes ---------------------------------------------------------------
odmOffsetTypes <- function # odmOffsetTypes
### fetch and filter data from ODM table "offsettypes"
(
  OffsetTypeID=NULL,
  ### Unique integer identifier that identifies the type of measurement offset.
  OffsetunitsID=NULL,
  ### Integer identifier that references the record in the Units table giving
  ### the Units of the OffsetValue.
  OffsetDescription=NULL,
  ### Full text description of the offset type. Example: "Below water surface",
  ### "Above Ground Level"
  fields="OffsetTypeID,OffsetDescription", 
  charAsPattern=TRUE, 
  odbc = currentDb(),
  dbg=TRUE
) 
{
  getFilteredRecords(odbc, "offsettypes", 
                     list(OffsetTypeID=OffsetTypeID,
                          OffsetunitsID=OffsetunitsID,
                          OffsetDescription=OffsetDescription), 
                     fields, charAsPattern, dbg=dbg)
}

# odmQualifiers ----------------------------------------------------------------
odmQualifiers <- function # odmQualifiers
### fetch and filter data from ODM table "qualifiers"
(
  QualifierID=NULL,
  ### Unique integer identifying the data qualifier.
  QualifierCode=NULL,
  ### Text code used by organization that collects the data. Example: "e" (for
  ### estimated) or "a" (for approved) or "p" (for provisional)
  QualifierDescription=NULL,
  ### Text of the data qualifying comment. Example: "Holding time for sample
  ### analysis exceeded"
  fields="QualifierID,QualifierDescription", 
  charAsPattern=TRUE, 
  odbc = currentDb(),
  dbg=TRUE
) 
{
  getFilteredRecords(odbc, "qualifiers", 
                     list(QualifierID=QualifierID,
                          QualifierCode=QualifierCode,
                          QualifierDescription=QualifierDescription), 
                     fields, charAsPattern, dbg=dbg)  
}

# odmSamples -------------------------------------------------------------------
odmSamples <- function # odmSamples
### fetch and filter data from ODM table "samples"
(
  SampleID=NULL, 
  ### Unique integer identifier that identifies each physical sample.
  SampleType=NULL, 
  ### Controlled vocabulary specifying the sample type from the SampleTypeCV
  ### table. Example: "FD", "PB", "SW", "Grab Sample"
  LabSampleCode=NULL, 
  ### Code or label used to identify and track lab sample or sample container
  ### (e.g. bottle) during lab analysis. Example: "AB-123"
  LabMethodID=NULL,
  ### Unique identifier for the laboratory method used to process the sample. 
  ### This references the LabMethods table.
  fields="SampleID,SampleType", 
  charAsPattern=TRUE, 
  odbc = currentDb(),
  dbg=TRUE
) 
{
  getFilteredRecords(odbc, "samples", 
                     list(SampleID=SampleID, 
                          SampleType=SampleType, 
                          LabSampleCode=LabSampleCode, 
                          LabMethodID=LabMethodID), 
                     fields, charAsPattern, dbg=dbg)  
}

# odmSites ---------------------------------------------------------------------
odmSites <- function # odmSites
### fetch and filter data from ODM table "sites"
(
  SiteID=NULL, 
  ### Unique identifier for each sampling location.
  SiteCode=NULL, 
  ### Code used by organization that collects the data to identify the site.
  ### Example: "10109000" (USGS Gage number)
  SiteName=NULL, 
  ### Full name of the sampling site. Example: "LOGAN RIVER ABOVE STATE DAM,
  ### NEAR LOGAN,UT"
  fields="SiteID,SiteCode,SiteName", 
  charAsPattern=TRUE, 
  odbc = currentDb(),
  dbg=TRUE
) 
{
  getFilteredRecords(odbc, "sites", 
                     list(SiteID=SiteID, 
                          SiteCode=SiteCode, 
                          SiteName=SiteName), 
                     fields, charAsPattern, dbg=dbg)
}
