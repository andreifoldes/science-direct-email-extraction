

#rm(list = ls(all = TRUE))

#install if not installed
list.of.packages <- c("rscopus", "statcheck", "dplyr", "XML", "RPostgreSQL", "pdftools", "stringr", "pryr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#load
lapply(list.of.packages, require, character.only = TRUE)


id<-''
api_key = get_api_key(id, error = FALSE)
options(elsevier_api_key_filename = id)
options(elsevier_api_key = id)

options(warn = -1)

#create list of ScienceDirect Psychology articles via CopyPaste on the web search interface
#scopusJournalList20171005 <-read.csv("C:\\Users\\User\\Documents\\Projects\\correspondingAuthor\\ScienceDirectJournalList.csv", header = FALSE, sep= ";")
scopusJournalList20171005 <-read.csv("ScienceDirectJournalList.csv", 
                                     header = FALSE, sep= ";", fileEncoding = "LATIN2")


#dbSetup
pg = dbDriver("PostgreSQL")

# Local Postgres.app database; no password by default
# Of course, you fill in your own database information here.
con = dbConnect(pg, user="postgres", password="",
                host="localhost", port=5432, dbname="miningDB")

###code

## import extraction method
source("extractMetadata.R")
#months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
years <- c("2010","2011","2012","2013","2014","2015","2016")
#dates <- expand.grid(months, years)
#dates <- paste(dates$Var1,dates$Var2)
dates<- years

journalIndex <- 1

#memory handling
maxMemory <- mem_used()+(6*1e8)
#helperfuncionr
getResultSet <- function(journalIndex, dateIndex, resultOffset=0)
{
  res <- generic_elsevier_api(
    query = paste(
      
      "SRCTITLE (",scopusJournalList20171005$V1[journalIndex],")",
      "AND  PUBYR(", dates[dateIndex],")"
    ),
    type = "search",
    search_type = "scidir",
    api_key = api_key,
    start = resultOffset,
    count = 25
  )
  print(paste(journalIndex,dateIndex))
  
  return(res)
}

#res$content$`search-results`$entry[[1]]$`prism:doi`

extractMeta<-function(i,j=1){
  
  while(j<=length(dates)){ 
    while (i < nrow(scopusJournalList20171005))
    {
      res <- getResultSet(i,j)
      
      #handle empty resultSet condition
      while (is.null(res$content$`search-results`$`opensearch:totalResults`) 
             || as.numeric(res$content$`search-results`$`opensearch:totalResults`) >= 5000
             || as.numeric(res$content$`search-results`$`opensearch:totalResults`) == 0 
             || res$get_statement$status_code!=200)
      {
        
        i<-i+1
        res <-getResultSet(i,j)
        if(i == nrow(scopusJournalList20171005))
        {
          return() 
        }
        
        if(!is.null(res$content$`search-results`$`opensearch:totalResults`) ){
          if(as.numeric(res$content$`search-results`$`opensearch:totalResults`) >= 5000){
            dbWriteTable(con,'repeatquery',data.frame(cbind(i,j)), row.names=FALSE, append=TRUE)
          }
        }
        
      }
      
      
      totalResCount <-
        as.numeric(res$content$`search-results`$`opensearch:totalResults`)
      
      ### MAIN - examine all abstracts in resultsSet in 25 length batches
      
      for (k in seq(0, as.numeric(totalResCount), by = 25))
      {
        if (k == 0)
        {
          res <- res #do nothing
        } else{
          res <- getResultSet(i, j, k)
        }
        
        mem <- system('systeminfo', intern=TRUE)
        available<-str_extract_all(mem[26],"\\(?[0-9˙]+\\)?")
        available<-strtoi(gsub("˙","",available))
        total<-str_extract_all(mem[25],"\\(?[0-9˙]+\\)?")
        total<-strtoi(gsub("˙","",total))
        mem<-available/total*100
        
        ###MEMORY POLICE
        if(mem>85){
          stop("Memory limit reached")
        }else{
          dbWriteTable(con,'memoryuse',data.frame("memoryused"=as.numeric(mem_used()/1048576), projectid), row.names=FALSE, append=TRUE)
        }
        
        idSet <- NULL
        
        message(paste("Current page index is:", "date",j, "journal", i, 'offset ', k))
        
        message(paste(res$get_statement$headers$`x-els-status`))
        
        if(res$get_statement$headers$`x-els-status`!="OK"){
          dbWriteTable(con,'repeatquery',data.frame(cbind(i,j)), row.names=FALSE, append=TRUE)
        }
        
        if(!is.null(res$content$`search-results`$entry))
        {
          entryCount <- length(res$content$`search-results`$entry)
          
          #Total number of results examined in a given batch
          for (l in 1:entryCount){
            
            #scopusId
            message("batchIndex:","date ",j," ", i, " ", k, " ", l)
            if (is.null(res$content$`search-results`$entry[[l]]$`prism:doi`))
            {
              idSet[l] <- NA
            } else{
              idSet[l] <- res$content$`search-results`$entry[[l]]$`prism:doi`
            }
            
            
            ##################################
            #Return Abstract of given article
            #lapply(res$content$`search-results`$entry,function(l) l$`dc:identifier`)
            if (is.na(idSet[l]))
            {
              s <- NA
              
            }
            else{
              
              link<-paste("http://api.elsevier.com/content/article/doi/",idSet[l],"?apiKey=",id,"&httpAccept=text%2Fxml", sep = "")
              
              resultList<-NULL
              
              tryCatch({resultList <- extractMetadata(link, con, idSet[l])},error=function(e){resultList<-NULL})
              
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
            
          }
          
          
        }
        
        
        
      }
      
      i<- i+1
      do.call(file.remove, list(list.files("pdfs", full.names = TRUE)))
      cat("\014")
      .rs.restartR()
      
      
    }
    j<-j+1
    i<-1
    cat("\014")  
    .rs.restartR()
    print("nextdate")
  } 
}



#extractMeta(i)

lastInsert<- dbGetQuery(con, "SELECT to_number(s.row, '99'), to_number(y.row, '99')
           FROM articles a
           LEFT JOIN scidirjournal s
           ON a.publicationname = s.publicationname
           LEFT JOIN years y
           ON to_char(EXTRACT(YEAR FROM coverdate), '9999') LIKE '%'|| y.years || '%'
           ORDER BY createdon DESC
           LIMIT 1")

extractMeta(lastInsert[[1]],lastInsert[[2]])


