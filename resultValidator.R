resultValidator = function(res){
  
  while(res$get_statement$status_code!=200 || 
        is.null(res$content$`search-results`$`opensearch:totalResults`) ||
        as.numeric(res$content$`search-results`$`opensearch:totalResults`) == 0)
  {

    if(res$get_statement$status_code!=200){
      stop(paste("error, should look into it:", journalIndex, ' ', 1))
    }
    
    if(is.null(res$content$`search-results`$`opensearch:totalResults`) ){
      message(paste("[resultSet is null] Current page index is:", journalIndex, ' ', 1))
    }
    if(as.numeric(res$content$`search-results`$`opensearch:totalResults`) ==0){
      message(paste("[resultSet is zero] Current page index is:", journalIndex, ' ', 1))
    }
    
    journalIndex<-journalIndex+1
    
    res <-getResultSet(journalIndex,dateIndex, 1)
    
    if(journalIndex == nrow(scopusJournalList20171005))
    {
      return()
    }
  }
  
  return(res)

  
}
