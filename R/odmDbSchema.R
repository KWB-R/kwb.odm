# createOdmDatabaseSchemaInMsAccess --------------------------------------------
createOdmDatabaseSchemaInMsAccess <- function # createOdmDatabaseSchemaInMsAccess
### Create ODM database schema in MS Access database.
### This function
### 1. creates all tables defined by the CUAHSI ODM 1.1 data model 
###    (see reference below). 
### 2. populates the tables representing so called "controlled vocabularies",
###    (e.g. for Variable names) with datasets defined by CUAHSI ODM 1.1. 
### Since the table creation is done by running SQL CREATE commands and since
### I did not succeed in defining default values for table fields by using SQL, 
### there are no default values defined in the created MS Access tables. 
##references<< \url{http://his.cuahsi.org/documents/ODM1.1DesignSpecifications.pdf}
(
  db
  ### full path to MS Access database file or ODBC database name
)
{
  sqlFolder <- system.file("sql", package = "kwb.odm")
  
  sqlScript.create <- file.path(sqlFolder, "ODM_Create_MsAccess.txt")
  sqlScript.fill <- file.path(sqlFolder, "ODM_Fill_MsAccess.txt")
  
  # Create ODM tables
  runSqlCommandsFromFile(db, sqlScript.create)
  
  # Fill ODM lookup tables
  runSqlCommandsFromFile(db, sqlScript.fill)
}

# dbSchema_ODM -----------------------------------------------------------------
dbSchema_ODM <- function # dbSchema_ODM
### dbSchema_ODM
(
)   
{
  DataTypes <- list(int = "int", double = "double", text = "text", memo = "memo", date_time = "date_time", boolean = "boolean")
  
  dbTable_Categories <- list (
    description = 'The Categories table defines the categories for categorical variables. Records are required for variables where DataType is specified as "Categorical". Multiple entries for each VariableID, with different DataValues provide the mapping from DataValue to category description',
    
    fields = list (
      VariableID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references the Variables record of a categorical variable.",
        Examples = "45"
      ),
      DataValue = list (
        DataType = DataTypes$double,
        Description = "Numeric value that defines the category",
        Examples = "1.0"
      ),
      CategoryDescription = list (
        DataType = DataTypes$text,
        Description = "Definition of categorical variable value",
        Examples = "Cloudy"
      )
    )
  )
  
  dbTable_CensorCodeCV <- list (
    description = "The CensorCodeCV table contains the controlled vocabulary for censor codes. Only values from the Term field in this table can be used to populate the CensorCode field of the DataValues table.",
    
    fields = list (
      Term = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary for CensorCode",
        Examples = '"lt", "gt", "nc"'
      ),
      
      Definition = list (
        DataType = DataTypes$memo,
        Description = "Definition of CensorCode controlled vocabulary term. The Definition is optional if the term is self explanatory.",
        Examples = '"less than", "greater than", "not censored"'
      )
    ) 
  )
  
  dbTable_DataTypeCV <- list (    
    description = "The DataTypeCV table contains the controlled vocabulary for data types. Only values from the Term field in this table can be used to populate the DataType field in the Variables table.",
    
    fields = list (
      Term = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary for DataType",
        Examples = '"Continuous"'
      ),
      
      Definition = list (
        DataType = DataTypes$memo,
        Description = "Definition of DataType controlled vocabulary term. The Definition is optional if the term is self explanatory.",
        Examples = '"A quantity specified at a particular instant in time measured with sufficient frequency (small spacing) to be interpreted as a continuous record of the phenomenon"'
      )
    )
  )
  
  dbTable_DataValues <- list (
    description = "The DataValues table contains the actual data values.",
    
    fields = list (
      ValueID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifier for each data value.",
        Examples = '43'
      ),
      
      DataValue = list (
        DataType = DataTypes$double,
        Description = "The numeric value of the observation. For Categorical variables, a number is stored her. The Variables table has DataType as Categorical and the Categories table maps from the DataValue onto Category Description",
        Examples = '"34.5"'
      ),
      
      ValueAccuracy = list (
        DataType = DataTypes$double,
        Description = "Numeric value that describes the measurement accuracy of the data value. If not given, it is interpreted as unknown.",
        Examples = '"4"'
      ),
      
      LocalDateTime = list (
        DataType = DataTypes$date_time,
        Description = "Local date and time at which the data value was observed. Represented in an implementation specific format.",
        Examples = '"9/4/2003 7:00:00 AM"'
      ),
      
      UTCOffset = list (
        DataType = DataTypes$double,
        Description = "Offset in hours from UTC time of the corresponding LocalDateTime value.",
        Examples = '"-7"'
      ),
      
      DateTimeUTC = list (
        DataType = DataTypes$date_time,
        Description = "Universal UTC date and time at which the data value was observed. Represented in an implementation specific format.",
        Examples = '"9/4/2003 2:00:00 PM"'
      ),
      
      SiteID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references the site at which the observation was measured. This links data values to their locations in the Sites table.",
        Examples = '"3"'
      ),
      
      VariableID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references the variable that was measured. This links data values to their variable in the Variables table.",
        Examples = '"5"'
      ),
      
      OffsetValue = list (
        DataType = DataTypes$double,
        Description = "Distance from a datum or control point to the point at which a data value was observed. If not given the OffsetValue is inferred to be 0, or not relevant/necessary.",
        Examples = '"2.1"'
      ),
      
      OffsetTypeID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references the measurement offset type in the OffsetTypes table.",
        Examples = '"3"'
      ),
      
      CensorCode = list (
        DataType = DataTypes$text,
        Description = "Text indication of whether the data value is censored from the CensorCodeCV controlled vocabulary.",
        Examples = '""nc""'
      ),
      
      QualifierID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references the Qualifiers table. If Null, the data value is inferred to not be qualified.",
        Examples = '"4"'
      ),
      
      MethodID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references method used to generate the data value in the Methods table.",
        Examples = '"3"'
      ),
      
      SourceID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references the record in the Sources table giving the source of the data value.",
        Examples = '"5"'
      ),
      
      SampleID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references into the Samples table. This is required only if the data value resulted from a physical sample processed in a lab.",
        Examples = '"7"'
      ),
      
      DerivedFromID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier for the derived from group of data values that the current data value is derived from. This refers to a group of derived from records in the DerivedFrom table. If NULL, the data value is inferred to not be derived from another data value.",
        Examples = '"5"'
      ),
      
      QualityControlLevelID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier giving the level of quality control that the value has been subjected to. This references the QualityControlLevels table.",
        Examples = '"1"'
      )
    )
  )
  
  dbTable_DerivedFrom <- list (
    description = "The DerivedFrom table contains the linkage between derived data values and the data values that they were derived from.",
    
    fields = list (
      DerivedFromID = list (
        DataType = DataTypes$int,
        Description = "Integer identifying the group of data values from which a quantity is derived.",
        Examples = '"3"'
      ),
      
      ValueID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier referencing data values that comprise a group from which a quantity is derived. This corresponds to ValueID in the DataValues table.",         
        Examples = '"1,2,3,4,5"'
      )
    )
  )
  
  dbTable_GeneralCategoryCV <- list (
    description = "The GeneralCategoryCV table contains the controlled vocabulary for the general categories associated with Variables. The GeneralCategory field in the Variables table can only be populated with values from the Term field of this controlled vocabulary table.",
    
    fields = list (
      Term = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary for GeneralCategory.",
        Examples = '""Hydrology""'
      ),
      
      Definition = list (
        DataType = DataTypes$memo,
        Description = "Definition of GeneralCategory controlled vocabulary term. The definition is optional if the term is self explanatory.",
        Examples = '""Data associated with hydrologic variables or processes.""'
      )
    )
  )  
  
  dbTable_GroupDescriptions <- list (
    description = "The GroupDescriptions table lists the descriptions for each of the groups of data values that have been formed.",
    
    fields = list (
      GroupID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifier for each group of data values that has been formed. This also references to GroupID in the Groups table.",
        Examples = '"4"'
      ),
      
      GroupDescription = list (
        DataType = DataTypes$memo,
        Description = "Text description of the group.",
        Examples = '""Echo Reservoir Profile 7/7/2005""'
      )
    )
  )  
  
  dbTable_Groups <- list (
    description = "The Groups table lists the groups of data values that have been created and the data values that are within each group.",
    
    fields = list (
      GroupID = list (
        DataType = DataTypes$int,
        Description = "Integer ID for each group of data values that has been formed.",
        Examples = '"4"'
      ),
      
      ValueID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier for each data value that belongs to a group. This corresponds to ValueID in the DataValues table",
        Examples = '"2,3,4"'
      )
    )
  )
  
  dbTable_ISOMetadata <- list (
    description = "The ISOMetadata table contains dataset and project level metadata required by the CUAHSI HIS metadata system (http://www.cuahsi.org/his/documentation.html) for compliance with standards such as the draft ISO 19115 or ISO 8601. The mandatory fields in this table must be populated to provide a complete set of ISO compliant metadata in the database.",  
    
    fields = list (
      MetadataID = list (
        DataType = DataTypes$int,
        Description = "Unique integer ID for each metadata record.",
        Examples = '"4"'
      ),
      
      TopicCategory = list (
        DataType = DataTypes$text,
        Description = "Topic category keyword that gives the broad ISO19115 metadata topic category for data from this source. The controlled vocabulary of topic category keywords is given in the TopicCategoryCV table.",
        Examples = '""inlandWaters""'
      ),
      
      Title = list (
        DataType = DataTypes$text,
        Description = "Title of data from a specific data source.",
        Examples = '""'
      ),
      
      Abstract = list (
        DataType = DataTypes$memo,
        Description = "Abstract of data from a specific data source.",
        Examples = '""'
      ),
      
      ProfileVersion = list (
        DataType = DataTypes$text,
        Description = "Name of metadata profile used by the data source",
        Examples = '""ISO8601""'
      ),
      
      MetadataLink = list (
        DataType = DataTypes$memo,
        Description = "Link to additional metadata reference material.",
        Examples = '""'
      )
    )
  )
  
  dbTable_LabMethods <- list (
    description = "The LabMethods table contains descriptions of the laboratory methods used to analyze physical samples for specific constituents.",
    
    fields = list (
      LabMethodID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifier for each laboratory method. This is the key used by the Samples table to reference a laboratory method.",
        Examples = '"6"'
      ),
      
      LabName = list (
        DataType = DataTypes$text,
        Description = "Name of the laboratory responsible for processing the sample.",
        Examples = '""USGS Atlanta Field Office""'
      ),
      
      LabOrganization = list (
        DataType = DataTypes$text,
        Description = "Organization responsible for sample analysis.",
        Examples = '""USGS""'
      ),
      
      LabMethodName = list (
        DataType = DataTypes$text,
        Description = "Name of the method and protocols used for sample analysis.",
        Examples = '""USEPA-365.1""'
      ),
      
      LabMethodDescription = list (
        DataType = DataTypes$memo,
        Description = "Description of the method and protocols used for sample analysis.",
        Examples = '"Processed through Model *** Mass Spectrometer""'
      ),
      
      LabMethodLink = list (
        DataType = DataTypes$memo,
        Description = "Link to additional reference material on the analysis method.",
        Examples = '""'
      )
    )
  )
  
  dbTable_Methods <- list (
    description = "The Methods table lists the methods used to collect the data and any additional information about the method.",
    
    fields = list (
      MethodID = list (
        DataType = DataTypes$int,
        Description = "Unique integer ID for each method.",
        Examples = '"5"'
      ),
      
      MethodDescription = list (
        DataType = DataTypes$memo,
        Description = "Text description of each method.",
        Examples = '""Specific conductance measured using a Hydrolab" or "Streamflow measured using a V notch weir with dimensions xxx""'
      ), 
      
      MethodLink = list (
        DataType = DataTypes$memo,
        Description = "Link to additional reference material on the method.",
        Examples = '""'
      )
    )
  )
  
  dbTable_ODMVersion <- list (
    description = "The ODM Version table has a single record that records the version of the ODM database. This table must contain a valid ODM version number. This table will be pre-populated and should not be edited.",
    
    fields = list (
      VersionNumber = list (
        DataType = DataTypes$text,
        Description = "String that lists the version of the ODM database.",
        Examples = '""1.1""'
      )
    )
  ) 
  
  dbTable_OffsetTypes <- list (
    description = "The OffsetTypes table lists full descriptive information for each of the measurement offsets.",
    
    fields = list (
      OffsetTypeID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifier that identifies the type of measurement offset.",
        Examples = '"2"'
      ),
      
      OffsetUnitsID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references the record in the Units table giving the Units of the OffsetValue.",
        Examples = '"1"'
      ),
      
      OffsetDescription = list (
        DataType = DataTypes$memo,
        Description = "Full text description of the offset type.",
        Examples = '""Below water surface" "Above Ground Level""'
      )
    )
  )  
  
  dbTable_Qualifiers <- list (
    description = "The Qualifiers table contains data qualifying comments that accompany the data.",
    
    fields = list (
      QualifierID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifying the data qualifier.",
        Examples = '"3"'
      ),
      
      QualifierCode = list (
        DataType = DataTypes$text,
        Description = "Text code used by organization that collects the data.",
        Examples = '""e" (for estimated) or "a" (for approved) or "p" (for provisional)"'
      ),              
      
      QualifierDescription = list (
        DataType = DataTypes$memo,
        Description = "Text of the data qualifying comment.",
        Examples = '""Holding time for sample analysis exceeded""'
      )
    )
  ) 
  
  dbTable_QualityControlLevels <- list (
    description = "The QualityControlLevels table contains the quality control levels that are used for versioning data within the database.",
    
    fields = list (
      QualityControlLevelID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifying the quality control level.",
        Examples = '"0, 1, 2, 3, 4, 5"'
      ),
      
      QualityControlLevelCode = list (
        DataType = DataTypes$text,
        Description = "Code used to identify the level of quality control to which data values have been subjected.",
        Examples = '""1", "1.1", "Raw", "QC Checked""'
      ),
      
      Definition = list (
        DataType = DataTypes$text,
        Description = "Definition of Quality Control Level.",
        Examples = '""Raw Data", "Quality Controlled Data""'
      ),
      
      Explanation = list (
        DataType = DataTypes$memo,
        Description = "Explanation of Quality Control Level",
        Examples = '""Raw data is defined as unprocessed data and data productsthat have not undergone quality control.""'
      )
    )
  ) 
  
  dbTable_SampleMediumCV <- list (
    description = "The SampleMediumCV table contains the controlled vocabulary for sample media.",
    
    fields = list (
      Term = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary forsample media.",
        Examples = '""Surface Water""'
      ),
      
      Definition = list (
        DataType = DataTypes$memo,
        Description = "Definition of sample mediacontrolled vocabulary term. Thedefinition is optional if the termis self explanatory.",
        Examples = '""Sample taken from surface water such as a stream, river, lake, pond, reservoir, ocean, etc.""'
      )
    )
  ) 
  
  dbTable_Samples <- list (
    description = "The Samples table gives information about physical samples analyzed in a laboratory.",
    
    fields = list (
      SampleID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifier that identifies each physical sample.",
        Examples = '"3"'
      ),
      
      SampleType = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary specifying the sample type from the SampleTypeCV table.",
        Examples = '""FD", "PB", "SW", "Grab Sample""'
      ),
      
      LabSampleCode = list (
        DataType = DataTypes$text,
        Description = "Code or label used to identify and track lab sample or sample container (e.g. bottle) during lab analysis.",
        Examples = '""AB-123""'
      ),
      
      LabMethodID = list (
        DataType = DataTypes$int,
        Description = "Unique identifier for the laboratory method used to process the sample. This references the LabMethods table.",
        Examples = '"4"'
      )
    )
  )
  
  dbTable_SampleTypeCV <- list (
    description = "The SampleTypeCV table contains the controlled vocabulary for sample type.",
    
    fields = list (
      Term = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary for sample type.",
        Examples = '""FD", "PB", "Grab Sample""'
      ),
      
      Definition = list (
        DataType = DataTypes$memo,
        Description = "Definition of sample type controlled vocabulary term. The definition is optional if the term is self explanatory.",
        Examples = '""Foliage Digestion", "Precipitation Bulk""'
      )
    )
  ) 
  
  dbTable_SeriesCatalog <- list (
    description = "The SeriesCatalog table lists each separate data series in the database for the purposes of identifying or displaying what data are available at each site and to speed simple queries without querying the main DataValues table. Unique site/variable combinations are defined by unique combinations of SiteID, VariableID, MethodID, SourceID, and QualityControlLevelID. This entire table should be programmatically derived and should be updated every time data is added to the database. Constraints on each field in the SeriesCatalog table are dependent upon the constraints on the fields in the table from which those fields originated.",
    
    fields = list (
      SeriesID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifier for each data series.",
        Examples = '"5"'
      ),
      
      SiteID = list (
        DataType = DataTypes$int,
        Description = "Site identifier from the Sites table.",
        Examples = '"7"'
      ),
      
      SiteCode = list (
        DataType = DataTypes$text,
        Description = "Site code used by organization that collects the data.",
        Examples = '""1002000""'
      ),
      
      SiteName = list (
        DataType = DataTypes$text,
        Description = "Full text name of sampling site.",
        Examples = '""Logan River""'
      ),
      
      VariableID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier for each Variable that references the Variables table.",
        Examples = '"4"'
      ),
      
      VariableCode = list (
        DataType = DataTypes$text,
        Description = "Variable code used by the organization that collects the data.",
        Examples = '""00060""'
      ),
      
      VariableName = list (
        DataType = DataTypes$int,
        Description = "Name of the variable from the variables table.",
        Examples = '""Temperature""'
      ),
      
      Speciation = list (
        DataType = DataTypes$text,
        Description = "Code used to identify how the data value is expressed (i.e., total phosphorus concentration expressed as P). This should be from the SpeciationCV controlled vocabulary table.",
        Examples = '""P", "N", "NO3""'
      ),
      
      VariableUnitsID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references the record in the Units table giving the Units of the data value.",
        Examples = '"5"'
      ),
      
      VariableUnitsName = list (
        DataType = DataTypes$text,
        Description = "Full text name of the variable units from the UnitsName field in the Units table.",
        Examples = '""milligrams per liter""'
      ),
      
      SampleMedium = list (
        DataType = DataTypes$text,
        Description = "The medium of the sample. This should be from the SampleMediumCV controlled vocabulary table.",
        Examples = '""Surface Water""'
      ),
      
      ValueType = list (
        DataType = DataTypes$text,
        Description = "Text value indicating what type of data value is being recorded. This should be from the ValueTypeCV controlled vocabulary table.",
        Examples = '""Field Observation""'
      ),
      
      TimeSupport = list (
        DataType = DataTypes$double,
        Description = "Numerical value that indicates the time support (or temporal footprint) of the data values. 0 is used to indicate data values that are instantaneous. Other values indicate the time over which the data values are implicitly or explicitly averaged or aggregated.", 
        Examples = '"0, 24"'
      ),
      
      TimeUnitsID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that  references the record in the Units table giving the Units of the time support. If TimeSupport is 0, indicating an instantaneous observation, a unit needs to still be given for completeness, although it is somewhat arbitrary.", 
        Examples = '"4"'
      ),
      
      TimeUnitsName = list (
        DataType = DataTypes$text,
        Description = "Full text name of the time support units from the UnitsName field in the Units table.", 
        Examples = '""hours""'
      ),
      
      DataType = list (
        DataType = DataTypes$text,
        Description = "Text value that identifies the data as one of several types from the DataTypeCV controlled vocabulary table.", 
        Examples = '""Continuous" "Instantaneous" "Cumulative" "Incremental" "Average" "Minimum" "Maximum" "Constant Over Interval" "Categorical""'
      ),     
      
      GeneralCategory = list (
        DataType = DataTypes$text,
        Description = "General category of the variable from the GeneralCategoryCV table.",
        Examples = '""Water Quality""'         
      ),         
      
      MethodID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that identifies the method used to generate the data values and references the Methods table.",
        Examples = '"2"'
      ),
      
      MethodDescription = list (
        DataType = DataTypes$memo,
        Description = "Full text description of the method used to generate the data values.",
        Examples = '""Specific conductance measured using a Hydrolab" or "Streamflow measured using a V notch weir with dimensions xxx""'
      ),
      
      SourceID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that identifies the source of the data values and references the Sources table.",
        Examples = '"5"'
      ),
      
      
      Organization = list (
        DataType = DataTypes$text,
        Description = "Text description of the source organization from the Sources table.",
        Examples = '""USGS""'
      ),
      
      SourceDescription = list (
        DataType = DataTypes$memo,
        Description = "Text description of the data source from the Sources table.",
        Examples = '""Text file retrieved from the EPA STORET system indicating data originally from Utah Division of Water Quality""'
      ),
      
      Citation = list (
        DataType = DataTypes$memo,
        Description = "Text string that give the citation to be used when the data from each source are referenced.",
        Examples = '""Slaughter, C. W., D. Marks, G. N. Flerchinger, S. S. Van Vactor and M. Burgess, (2001), "Thirty-five years of research data collection at the Reynolds Creek Experimental Watershed, Idaho, United States," Water Resources Research, 37(11): 2819-2823.""' 
      ),    
      
      QualityControlLevelID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that indicates the level of quality control that the data values have been subjected to.",
        Examples = '"0,1,2,3,4"'
      ),
      
      QualityControlLevelCode = list (
        DataType = DataTypes$text,
        Description = "Code used to identify the level of quality control to which data values have been subjected.", 
        Examples = '""1", "1.1", "Raw", "QC Checked""' 
      ),
      
      BeginDateTime = list (
        DataType = DataTypes$date_time,
        Description = "Date of the first data value in the series. To be programmatically updated if new records are added.",
        Examples = '"9/4/2003 7:00:00 AM"'
      ),
      
      EndDateTime = list (
        DataType = DataTypes$date_time,
        Description = "Date of the last data value in the series. To be programmatically updated if new records are added.",
        Examples = '"9/4/2005 7:00:00 AM"'
      ),
      
      BeginDateTimeUTC = list (
        DataType = DataTypes$date_time,
        Description = "Date of the first data value in the series in UTC. To be programmatically updated if new records are added.",
        Examples = '"9/4/2003 2:00 PM"'
      ),
      
      EndDateTimeUTC = list (
        DataType = DataTypes$date_time,
        Description = "Date of the last data value in the series in UTC. To be programmatically updated if new records are added.", 
        Examples = '"9/4/2003 2:00 PM"'
      ),
      
      ValueCount = list (
        DataType = DataTypes$int,
        Description = "The number of data values in the series identified by the combination of the SiteID, VariableID, MethodID, SourceID and QualityControlLevelID fields. To be programmatically updated if new records are added.",           
        Examples = '"50"'
      )
    )
  )
  
  dbTable_Sites <- list (
    description = "The Sites table provides information giving the spatial location at which data values have been collected.",
    
    fields = list (
      SiteID = list (
        DataType = DataTypes$int,
        Description = "Unique identifier for each sampling location.",
        Examples = '"37"'
      ),
      
      SiteCode = list (
        DataType = DataTypes$text,
        Description = "Code used by organization that collects the data to identify the site",
        Examples = '""10109000" (USGS Gage number)"'
      ),
      
      SiteName = list (
        DataType = DataTypes$text,
        Description = "SiteName",
        Examples = '""LOGAN RIVER ABOVE STATE DAM, NEAR LOGAN,UT""' 
      ),
      
      Latitude = list (
        DataType = DataTypes$double,
        Description = "Latitude in decimal degrees.",
        Examples = '"45.32"'
      ),
      
      Longitude = list (
        DataType = DataTypes$double,
        Description = "Longitude in decimal degrees. East positive, West negative.",
        Examples = '"-100.47"'
      ),
      
      LatLongDatumID = list (
        DataType = DataTypes$int,
        Description = "Identifier that references the Spatial Reference System of the latitude and longitude coordinates in the SpatialReferences table.", 
        Examples = '"1"'
      ),
      
      Elevation_m = list (
        DataType = DataTypes$double,
        Description = "Elevation of sampling location (in m). If this is not provided it needs to be obtained programmatically from a DEM based on location information.",
        Examples = '"1432"'
      ),
      
      VerticalDatum = list (
        DataType = DataTypes$text,
        Description = "Vertical datum of the elevation. Controlled Vocabulary from VerticalDatumCV.",
        Examples = '""NAVD88""'
      ),
      
      LocalX = list (
        DataType = DataTypes$double,
        Description = "Local Projection X coordinate.",
        Examples = '"456700"'
      ),
      
      LocalY = list (
        DataType = DataTypes$double,
        Description = "Local Projection Y Coordinate.",
        Examples = '"232000"'
      ),
      
      LocalProjectionID = list (
        DataType = DataTypes$int,
        Description = "Identifier that references the Spatial Reference System of the local coordinates in the SpatialReferences table. This field is required if local coordinates are given.",
        Examples = '"7"'
      ),
      
      PosAccuracy_m = list (
        DataType = DataTypes$double,
        Description = "Value giving the accuracy with which the positional information is specified in meters.",
        Examples = '"100"'
      ),
      
      State = list (
        DataType = DataTypes$text,
        Description = "Name of state in which the monitoring site is located.",
        Examples = '""Utah""'
      ),
      
      County = list (
        DataType = DataTypes$text,
        Description = "Name of county in which the monitoring site is located.",
        Examples = '""Cache""'
      ),
      
      Comments = list (
        DataType = DataTypes$memo,
        Description = "Comments related to the site.",
        Examples = '""'
      )
    )
  )
  
  dbTable_Sources <- list (
    description = "The Sources table lists the original sources of the data, providing information sufficient to retrieve and reconstruct the data value from the original data files if necessary.",
    
    fields = list (
      SourceID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifier that identifies each data source.",
        Examples = '"5"'
      ),
      
      Organization = list (
        DataType = DataTypes$text,
        Description = "Name of the organization that collected the data. This should be the agency or organization that collected the data, even if it came out of a database consolidated from many sources such as STORET.", 
        Examples = '""Utah Division of Water Quality""'
      ),
      
      SourceDescription = list (
        DataType = DataTypes$memo,
        Description = "Full text description of the source of the data.",
        Examples = '""Text file retrieved from the EPA STORET system indicating data originally from Utah Division of Water Quality""' 
      ),
      
      SourceLink = list (
        DataType = DataTypes$memo,
        Description = "Link that can be pointed at the original data file and/or associated metadata stored in the digital library or URL of data source.", 
        Examples = '""'
      ),
      
      ContactName = list (
        DataType = DataTypes$text,
        Description = "Name of the contact person for the data source.",
        Examples = '""Jane Adams""'
      ),
      
      Phone = list (
        DataType = DataTypes$int,
        Description = "Phone number for the contact person.", 
        Examples = '""435-797-0000""'
      ),
      
      Email = list (
        DataType = DataTypes$text,
        Description = "Email address for the contact person.",
        Examples = '""Jane.Adams@dwq.ut""'
      ),
      
      Address = list (
        DataType = DataTypes$text,
        Description = "Street address for the contact person.",
        Examples = '""45 Main Street""'
      ),
      
      City = list (
        DataType = DataTypes$text,
        Description = "City in which the contact person is located.",
        Examples = '""Salt Lake City""'
      ),
      
      State= list (
        DataType = DataTypes$text,
        Description = "State in which the contact person is located. Use two letter abbreviations for US. For other countries give the full country name.",
        Examples = '""UT""'
      ),
      
      ZipCode = list (
        DataType = DataTypes$text,
        Description = "US Zip Code or country postal code.",
        Examples = '""82323""'
      ),
      
      Citation = list (
        DataType = DataTypes$memo,
        Description = "Text string that give the citation to be used when the data from each source are referenced.",
        Examples = '""Data collected by USU as part of the Little Bear River Test Bed Project""'
      ),
      
      MetadataID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier referencing the record in the ISOMetadata table for this source.",
        Examples = '"5"'
      )
    )
  )
  
  dbTable_SpatialReferences <- list (
    description = "The SpatialReferences table provides information about the Spatial Reference Systems used for latitude and longitude as well as local coordinate systems in the Sites table. This table is a controlled vocabulary.",
    
    fields = list (
      SpatialReferenceID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifier for each Spatial Reference System.",
        Examples = '"37"'
      ),
      
      SRSID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier for the Spatial Reference System from http://www.epsg.org/.",
        Examples = '"4269"'
      ),
      
      SRSName = list (
        DataType = DataTypes$text,
        Description = "Name of the Spatial Reference System.",
        Examples = '""NAD83""'
      ),
      
      IsGeographic = list (
        DataType = DataTypes$boolean,
        Description = "Value that indicates whether the spatial reference system uses geographic coordinates (i.e. latitude and longitude) or not.",
        Examples = '""True", "False""'
      ),
      
      Notes = list (
        DataType = DataTypes$memo,
        Description = "Descriptive information about the Spatial Reference System. This field would be used to define a non-standard study area specific system if necessary and would contain a description of the local projection information. Where possible, this should refer to a standard projection, in which case latitude and longitude can be determined from local projection  information. If the local grid system is non-standard then latitude and longitude need to be included too.", 
        Examples = '""'
      )
    )
  ) 
  
  
  dbTable_SpeciationCV <- list (
    description = "The SpeciationCV table contains the controlled vocabulary for the Speciation field in the Variables table.",
    
    fields = list (
      Term = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary for Speciation.",
        Examples = '""P""'
      ),
      
      Definition = list (
        DataType = DataTypes$memo,
        Description = "Definition of Speciation controlled vocabulary term. The definition is optional if the term is self explanatory.",
        Examples = '""Expressed as phosphorus""'
      )
    )
  ) 
  
  dbTable_SpeciationCV <- list (
    description = "The TopicCategoryCV table contains the controlled vocabulary for the ISOMetaData topic categories.",
    
    fields = list (
      Term = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary for TopicCategory.",
        Examples = '""InlandWaters""'
      ),
      
      Definition = list (
        DataType = DataTypes$memo,
        Description = "Definition of TopicCategory controlled vocabulary term. The definition is optional if the term is self explanatory.",
        Examples = '""Data associated with inland waters""'
      )
    )
  ) 
  
  dbTable_Units <- list (
    description = "The Units table gives the Units and UnitsType associated with variables, time support, and offsets. This is a controlled vocabulary table.",
    
    fields = list (
      UnitsID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifier that identifies each unit.",
        Examples = '"6"'
      ),
      
      UnitsName = list (
        DataType = DataTypes$text,
        Description = "Full text name of the units.",
        Examples = '""Milligrams Per Liter""'
      ),
      
      UnitsType = list (
        DataType = DataTypes$text,
        Description = "Text value that specifies the dimensions of the units.",
        Examples = '""Length" "Time" "Mass""'
      ),
      
      UnitsAbbreviation = list (
        DataType = DataTypes$text,
        Description = "Text abbreviation for the units.",
        Examples = '""mg/L""'
      )
    )
  ) 
  
  dbTable_ValueTypeCV <- list (
    description = "The ValueTypeCV table contains the controlled vocabulary for the ValueType field in the Variables and SeriesCatalog tables.",
    
    fields = list (
      Term = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary for ValueType.",
        Examples = '""Field Observation""'
      ),
      
      Definition = list (
        DataType = DataTypes$memo,
        Description = "Definition of the ValueType controlled vocabulary term. The definition is optional if the term is self explanatory.",
        Examples = '""Observation of a variable using a field instrument""'
      )
    )
  ) 
  
  dbTable_VariableNameCV <- list (
    description = "The VariableName CV table contains the controlled vocabulary for the VariableName field in the Variables and SeriesCatalog tables.",
    
    fields = list (
      Term = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary for Variable names.",
        Examples = '""Temperature", "Discharge", "Precipitation""'
      ),
      
      Definition = list (
        DataType = DataTypes$memo,
        Description = "Definition of the VariableName controlled vocabulary term. The definition is optional if the term is self explanatory.",
        Examples = '""'
      )
    )
  ) 
  
  dbTable_Variables <- list (
    description = "The Variables table lists the full descriptive information about what variables have been measured.",
    
    fields = list (
      VariableID = list (
        DataType = DataTypes$int,
        Description = "Unique integer identifier for each variable.",
        Examples = '"6"'
      ),
      
      VariableCode = list (
        DataType = DataTypes$text,
        Description = "Text code used by the organization that collects the data to identify the variable.",
        Examples = '""00060" used by USGS for discharge"'
      ),
      
      VariableName = list (
        DataType = DataTypes$int,
        Description = "Full text name of the variable that was measured, observed, modeled, etc. This should be from the VariableNameCV controlled vocabulary table.",
        Examples = '""Discharge""'
      ),
      
      Speciation = list (
        DataType = DataTypes$text,
        Description = "Text code used to identify how the data value is expressed (i.e., total phosphorus concentration expressed as P). This should be from the SpeciationCV controlled vocabulary table.",
        Examples = '""P", "N", "NO3""'
      ),
      
      VariableUnitsID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references the record in the Units table giving the units of the data values associated with the variable.",
        Examples = '"4"'
      ),
      
      SampleMedium = list (
        DataType = DataTypes$text,
        Description = "The medium in which the sample or observation was taken or made. This should be from the SampleMediumCV controlled vocabulary table.",
        Examples = '""Surface Water" "Sediment" "Fish Tissue""'
      ),
      
      ValueType = list (
        DataType = DataTypes$text,
        Description = "Text value indicating what type of data value is being recorded. This should be from the ValueTypeCV controlled vocabulary table.",
        Examples = '"Text value indicating what type of data value is being recorded. This should be from the  ValueTypeCV controlled vocabulary table."'
      ),
      
      IsRegular = list (
        DataType = DataTypes$boolean,
        Description = "Value that indicates whether the data values are from a regularly sampled time series.",
        Examples = '""True" "False""'
      ),
      
      TimeSupport = list (
        DataType = DataTypes$double,
        Description = "Numerical value that indicates the time support (or temporal footprint) of the data values. 0 is used to indicate data values that are instantaneous. Other values indicate the time over which the data values are implicitly or explicitly averaged or aggregated.",
        Examples = '"0, 24"'
      ),
      
      TimeUnitsID = list (
        DataType = DataTypes$int,
        Description = "Integer identifier that references the record in the Units table giving the Units of the time support. If TimeSupport is 0, indicating an instantaneous observation, a unit needs to still be given for completeness, although it is somewhat arbitrary.", 
        Examples = '"4"'
      ),
      
      DataType = list (
        DataType = DataTypes$text,
        Description = "Text value that identifies the data values as one of several types from the DataTypeCV controlled vocabulary table.",
        Examples = '""Continuous" "Sporadic" "Cumulative" "Incremental" "Average" "Minimum" "Maximum" "Constant Over Interval" "Categorical""'
      ),
      
      GeneralCategory = list (
        DataType = DataTypes$text,
        Description = "General category of the data values from the GeneralCategoryCV controlled vocabulary table.",
        Examples = '""Climate" "Water Quality" "Groundwater Quality""'
      ),
      
      NoDataValue = list (
        DataType = DataTypes$double,
        Description = "Numeric value used to encode no data values for this variable.",
        Examples = '"-9999"'
      )
    )
  ) 
  
  dbTable_VerticalDatumCV <- list (
    description = "The VerticalDatumCV table contains the controlled vocabulary for the VerticalDatum field in the Sites table.",
    
    fields = list (
      Term = list (
        DataType = DataTypes$text,
        Description = "Controlled vocabulary for VerticalDatum.",
        Examples = '""NAVD88""'
      ),
      
      Definition = list (
        DataType = DataTypes$memo,
        Description = "Definition of the VerticalDatum controlled vocabulary. The definition is optional if the term is self explanatory.",
        Examples = '""North American Vertical Datum of 1988""'
      )
    )
  )
  
  #   ### dummy
  #   dbTable_TABLE <- list (
  #     description = "",
  #
  #     fields = list (
  #       FIELD1 = list (
  #         DataType = DataTypes$,
  #         Description = "",
  #         Examples = '""'
  #       ),
  #       
  #       FIELD2 = list (
  #         DataType = DataTypes$,
  #         Description = "",
  #         Examples = '""'
  #       )
  #     )
  #   )  
  
  ### Return list of table definitions
  list (
    
    tables = list (
      Categories = dbTable_Categories,  
      CensorCodeCV = dbTable_CensorCodeCV,
      DataTypeCV = dbTable_DataTypeCV,
      DataValues = dbTable_DataValues,      
      DerivedFrom = dbTable_DerivedFrom,
      GeneralCategoryCV = dbTable_GeneralCategoryCV,
      GroupDescriptions = dbTable_GroupDescriptions,
      Groups = dbTable_Groups,
      ISOMetadata = dbTable_ISOMetadata,
      LabMethods = dbTable_LabMethods,
      Methods = dbTable_Methods,
      ODMVersion = dbTable_ODMVersion,
      OffsetTypes = dbTable_OffsetTypes,
      Qualifiers = dbTable_Qualifiers,
      QualityControlLevels = dbTable_QualityControlLevels,
      SampleMediumCV = dbTable_SampleMediumCV,
      Samples = dbTable_Samples,
      SampleTypeCV = dbTable_SampleTypeCV,
      SeriesCatalog = dbTable_SeriesCatalog,
      Sites = dbTable_Sites,
      Sources = dbTable_Sources,
      SpatialReferences = dbTable_SpatialReferences,
      SpeciationCV = dbTable_SpeciationCV,
      SpeciationCV = dbTable_SpeciationCV,
      Units = dbTable_Units,
      ValueTypeCV = dbTable_ValueTypeCV,
      VariableNameCV = dbTable_VariableNameCV,
      Variables = dbTable_Variables,
      VerticalDatumCV = dbTable_VerticalDatumCV
    ),
    
    relationships = list (
    )  
  )  
}
