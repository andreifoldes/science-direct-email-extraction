# science-direct-email-extraction
Currently available R packages don't seem to provide corresponding author contact, so one must dive deep in the abyss that is XML

example usage:

dates <- list("February 2010", "March 2010", "April 2010")

for(d in dates){
  
  extractMeta(1,d)
  
}



