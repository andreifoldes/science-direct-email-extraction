extractMetadata <- function(link, con, ar_id){
  
  tryCatch({
    xml_parse <- xmlParse(link)
    xml_data <- xmlToList(xml_parse)
  }, error = function(e){
    dbWriteTable(con,'failedfetch',data.frame(ar_id), row.names=FALSE,append=TRUE)
    message("xml error")
    return(e)
    
  })
  
  #extract CA email address
  if(is.null(xml_data$originalText$doc$`serial-item`$`converted-article`$head))
  {
    head<-xml_data$originalText$doc$`serial-item`$`article`$head
    if(is.null(head)) 
    {
      head<-xml_data$originalText$doc$`serial-item`$`article`$head
      if(is.null(head)){
        head<-xml_data$originalText$doc$`serial-item`$`simple-article`$`simple-head`
        if(is.null(head)){
          head<-xml_data$coredata
        }
        if(is.null(head)){stop(paste("head not found for",link))}
      }
    }
  }else{
    head<-xml_data$originalText$doc$`serial-item`$`converted-article`$head
  }
  
  email <-NA
  index <- NA
  
  core<- xml_data$coredata
  
  tmp<-core[which(names(xml_data$coredata)=="creator")]
  #authors preparation
  #authors <- data.frame(names=matrix(unlist(tmp), byrow=T))
  authors <- data.frame(names = do.call("rbind", tmp))
  authors <- data.frame(do.call('rbind', strsplit(as.character(authors$names),',',fixed=TRUE)))
  authors <- cbind(authors,index=rownames(authors))
  

  
  doi       <-  if(!is.null(core$doi)) core$doi else NA
  scopusId  <-  if(!is.null(xml_data$`scopus-id`)) xml_data$`scopus-id` else NA
  title     <-  if(!is.null(core$title)) core$title else NA
  publicationname <- if(!is.null(core$publicationName)) core$publicationName else NA
  type      <-  if(!is.null(core$aggregationType)) core$aggregationType else NA
  volume    <- if(!is.null(core$volume)) core$volume else NA
  issueidentifier <- if(!is.null(core$issueIdentifier)) core$issueIdentifier else NA
  pagerange <- if(!is.null(core$pageRange)) core$pageRange else NA
  number    <- if(!is.null(core$number)) core$number else NA
  coverdate <- if(!is.null(core$coverDate)) core$coverDate else NA
  publisher <- if(!is.null(core$publisher)) core$publisher else NA
  pubtype   <- if(!is.null(core$pubType)) core$pubType else NA
  
  tmp<-core[which(names(xml_data$coredata)=="subject")]
  
  if(length(tmp)!=0){
    if(length(tmp)==1)
    {
      keywords<-data.frame(keywords = matrix(unlist(strsplit(tmp$subject, ",")), byrow=TRUE))
    }else{
      keywords <- data.frame(keywords = do.call("rbind", tmp))
    }
    keywords <- cbind(keywords,index=rownames(keywords))
  }else{
    keywords        <-  NULL
  }
  
  sourceid <- scopusId
  sourceidtype <- "scopusID"


  
  authorsDF <- data.frame(doi, sourceid, sourceidtype, givenname=authors$X2, surname = authors$X1, index=authors$index)
  
  if(!is.null(keywords))
  {
    keywordsDF <- data.frame(doi, sourceid, sourceidtype, keyword=keywords$keywords, index= keywords$index)
  }else{
    keywordsDF <- NULL
  }
  
  Sys.sleep(sample(1:5, 1))
  
  link<-paste("http://api.elsevier.com/content/article/doi/",doi,"?apiKey=",api_key,"&httpAccept=application%2Fpdf", sep = "")
  output<-paste("C:\\Users\\User\\Documents\\Projects\\correspondingAuthor\\tmp\\",gsub("/","_",doi),".pdf",sep="")
  
  tryCatch({

  download.file(link,output, mode="wb")
  pagenum<-pdf_info(output)$pages
  fulltextStatcheck <- checkPDF(output)
  }, error = function(e){
    dbWriteTable(con,'failedDownloads',data.frame(doi, link), row.names=FALSE,append=TRUE)
    fulltextStatcheck <- NULL
    message("failed downloading article :(")
    print(e)
  })
  
  if(!is.null(fulltextStatcheck))
  {
    fulltextStatcheckDF<- data.frame(doi=doi, fulltextStatcheck, fromfulltext=1, pagenum)
    names(fulltextStatcheckDF) <- gsub("\\.","",tolower(names(fulltextStatcheckDF)))
    fulltextStatcheckDF$source<-NULL
    
  }else{
    fulltextStatcheckDF<-NULL
  }
  
  abstractStatcheck <- statcheck(xml_data$coredata$description)
  
  if(!length(abstractStatcheck)==0)
  {
    abstractStatcheckDF<- data.frame(doi=doi, abstractStatcheck, fromfulltext=0)
    names(abstractStatcheckDF) <- gsub("\\.","",tolower(names(abstractStatcheckDF)))
    abstractStatcheckDF$source<-NULL
    
  }else{
    abstractStatcheckDF<-NULL
  }
  
  for(i in 1:length(head$`author-group`))
  {
    if (!is.null(head$`author-group`[i]$author$`e-address`$text))
    {
      email<-head$`author-group`[i]$author$`e-address`$text
      index<-i
    }
  }
  
  cEmail <- if(is.na(email)) NULL else email
  cName <-  if(!is.null(head$`author-group`[index]$author)) paste(head$`author-group`[index]$author$surname, head$`author-group`[index]$author$`given-name`, sep=",") else NA
  cName <- data.frame(do.call('rbind', strsplit(as.character(cName),',',fixed=TRUE)))
  
  if(is.null(cEmail)){
    #stop("email missing")
    emailDF<-NULL
  }else{
    emailDF<-data.frame(doi,email=cEmail,givenname=cName$X2, surname = cName$X1)
  }
  
  
  articleDF <- data.frame(doi, sourceid, sourceidtype, title, publicationname, type, volume, issueidentifier, pagerange
                          ,number, coverdate, publisher, pubtype, projectid, pagenum)
  
  return(list(articleDF, authorsDF, keywordsDF, fulltextStatcheckDF, abstractStatcheckDF, emailDF))
  
}




