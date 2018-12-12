# odmDeleteDataValues ----------------------------------------------------------
odmDeleteDataValues <- function # odmDeleteDataValues
### odmDeleteDataValues
(
  db = currentDb(), 
  valueIDs, 
  warn = TRUE, 
  dbg = TRUE
)
{
  condition <- sprintf("ValueID IN (%s)", commaCollapsed(valueIDs))
  
  continue <- FALSE
  
  if (warn) {
    
    sql <- sprintf("SELECT * FROM datavalues WHERE %s", condition)
    
    dat <- hsSqlQuery(db, sql, dbg = dbg)
    
    if (nrow(dat) > 0) {
      print(head(dat))
      prompt <- sprintf("Do you really want to delete these %d records (Y, n)? ",
                        nrow(dat))
      answer <- readline(prompt=prompt)
      continue <- (answer == "Y")
    }
  } else {
    continue <- TRUE
  }
  
  if (continue) {
    sql <- sprintf("DELETE FROM datavalues WHERE %s", condition)
    hsSqlQuery(db, sql, dbg=dbg)      
  }
}
