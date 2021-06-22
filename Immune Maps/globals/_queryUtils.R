dbConnector <- function() {
  
  conn_args <- config::get(file = "config/config.yml", "dataconnection")
  
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = conn_args$driver,
                        Server   = conn_args$server,
                        Database = conn_args$database,
                        UID      = conn_args$uid,
                        PWD      = conn_args$pwd,
                        Port     = conn_args$port
  )
  
  return(con)
  
}

getDataframeFromDatabase <- function(queryString,parameters,convertFactorsToStrings = TRUE) {
  
    if(!is.null(parameters)){
      parameters %>%
        mutate_if(is.factor, as.character) -> parameters
    }

    dbhandle <- dbConnector()
  
    query <- dbSendQuery(dbhandle,queryString)
    
    dbBind(query, parameters)
    
    data <- dbFetch(query)
    
    dbClearResult(query)

    if(convertFactorsToStrings){
      data <- data %>%
        mutate_if(is.factor, as.character) -> data
    }

    return(data)

}
