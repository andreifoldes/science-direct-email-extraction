

#rm(list = ls(all = TRUE))

#install if not installed
list.of.packages <- c("rscopus", "statcheck", "dplyr", "XML", "RPostgreSQL", "pdftools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#load
lapply(list.of.packages, require, character.only = TRUE)

journalIndex <- 0
dateVal  <- ""
projectid<- 0

id<-''
api_key = get_api_key(id, error = FALSE)
options(elsevier_api_key_filename = id)
options(elsevier_api_key = id)

options(warn = -1)

#create list of ScienceDirect Psychology articles via CopyPaste on the web search interface
#scopusJournalList20171005 <-read.csv("C:\\Users\\User\\Documents\\Projects\\correspondingAuthor\\ScienceDirectJournalList.csv", header = FALSE, sep= ";")
scopusJournalList20171005 <-read.csv("", 
                                     header = FALSE, sep= ";", fileEncoding = "LATIN2")


#dbSetup
pg = dbDriver("PostgreSQL")

# Local Postgres.app database; no password by default
# Of course, you fill in your own database information here.
con = dbConnect(pg, user="", password="",
                host="localhost", port=, dbname="")

# check random table 
dbExistsTable(con, "failedxml")

###code

## import extraction method
source("C:\\Users\\User\\Documents\\Projects\\correspondingAuthor\\extractMetadata.R")
source("C:\\Users\\User\\Documents\\Projects\\correspondingAuthor\\resultValidator.R")


#helperfuncion
getResultSet <- function(journalIndex, dateVal, resultOffset=1)
{
  res = generic_elsevier_api(
    query = paste(
      "DOCTYPE ( ar )  AND",
      "SRCTITLE (",scopusJournalList20171005$V1[journalIndex],")",
      "AND  PUBDATETXT(", dateVal,")"
    ),
    type = "search",
    search_type = "scopus",
    api_key = api_key,
    start = resultOffset,
    count = 100
  )
}

extractMeta = function(journalIndex, dateVal){
  
  while (journalIndex <= nrow(scopusJournalList20171005))
  {
    res <- getResultSet(journalIndex, dateVal)
    
    res <- resultValidator(res)
    
    #handle large resultSet condition
    while (as.numeric(res$content$`search-results`$`opensearch:totalResults`) >= 5000)
    {
      message(paste("[ResCount is above 5000] Current page index is:", journalIndex, ' ', 1))
      
      dbWriteTable(con,'repeatquery',data.frame(journalIndex,dateVal,projectid), row.names=FALSE,append=TRUE)
      
      
      journalIndex<-journalIndex+1
      res <-getResultSet(journalIndex, dateVal)
      res <-resultValidator(result)
      if(journalIndex == nrow(scopusJournalList20171005))
      {
        return() 
      }
    }
    
    totalResCount <-if(is.null(res$content$`search-results`$`opensearch:totalResults`) 
                       || res$content$`search-results`$`opensearch:totalResults` == 0 ) NULL else as.numeric(res$content$`search-results`$`opensearch:totalResults`)
    
    ### MAIN - examine all astracts in resultsSet in 100 length batches
    if(!is.null(totalResCount))
    {
      for (k in seq(1, as.numeric(totalResCount), by = 100))
      {
        if (k == 1)
        {
          res <- res #do nothing
        } else{
          res <- getResultSet(journalIndex, dateVal, k)
        }
        
        
        idSet <- NULL
        
        message(paste("Current page index is:", journalIndex," ", dateVal, " ", k))
        
        message(paste(res$get_statement$headers$`x-els-status`))
        
        if(res$get_statement$headers$`x-els-status`!="OK") stop("request not ok")
        
        if(!is.null(res$content$`search-results`$entry))
        {
          entryCount <- length(res$content$`search-results`$entry)
          
          #Total number of results examined in a given batch
          for (l in 1:entryCount){
            
            #scopusId
            message("batchIndex:", journalIndex," ", dateVal, " ", k, " ", l)
            if (is.null(res$content$`search-results`$entry[[l]]$`dc:identifier`))
            {
              idSet[l] <- NA
            } else{
              idSet[l] <- res$content$`search-results`$entry[[l]]$`dc:identifier`
            }
            
            ##################################
            #Return Abstract of given article
            #lapply(res$content$`search-results`$entry,function(l) l$`dc:identifier`)
            if (is.na(idSet[l]))
            {
              s <- NA
              
            }else{
              
              link<-paste("http://api.elsevier.com/content/article/scopus_id/",substr(idSet[l],11,30),"?apiKey=",id,"&httpAccept=text%2Fxml", sep = "")
              
              resultList <-tryCatch({extractMetadata(link, con, idSet[l])}
                                    ,error = function(err) { message(e)})
              
              if(!is.null(resultList)){
                result <- tryCatch({
                  dbWriteTable(con,'articles',resultList[[1]], row.names=FALSE,append=TRUE)
                },error = function(err) {
                  
                  # error handler picks up where error was generated
                  print(paste("MY_ERROR:  ","ID:",idSet[l], "index: ", l , err))
                  dbWriteTable(con,'duplicates',resultList[[1]], row.names=FALSE, append=TRUE)
                  
                }) # END tryCatch
                
                #if article with given doi does not exist then insert
                if(result==TRUE){
                  
                  #authors always have to be present in result
                  dbWriteTable(con,'authors',resultList[[2]], row.names=FALSE,append=TRUE)
                  #keywords dont always have to be present in result
                  if(!is.null(resultList[[3]])){
                    dbWriteTable(con,'keywords',resultList[[3]], row.names=FALSE,append=TRUE)
                  }
                  
                  if(!is.null(resultList[[4]])){
                    dbWriteTable(con,'statcheck',resultList[[4]], row.names=FALSE,append=TRUE)
                  }
                  
                  if(!is.null(resultList[[5]])){
                    dbWriteTable(con,'statcheck',resultList[[5]], row.names=FALSE,append=TRUE)
                  }
                  
                  if(!is.null(resultList[[6]])){
                    dbWriteTable(con,'emails',resultList[[6]], row.names=FALSE,append=TRUE)
                  }
                  
                }else{
                  if(grepl("Key(.+)already exists", result))
                  {
                    message("Article already in db...")
                  }else{
                    stop(result)
                  }
                }
                
              }
              
              
            }
            
            #metadataBatch <- rbind(metadataBatch,s)
            
            
            
          }
          
          #write.table(metadataBatch,paste(path,"\\",paste("metadata",i,k,dateIndex, sep = "_"), ".csv",sep=""), col.names=FALSE, row.names=FALSE, sep=";")
          
          
        }
        
        
        
      }
    }

    
    journalIndex<- journalIndex+1
    
  }
  
}



#mustRepeat <- NULL


