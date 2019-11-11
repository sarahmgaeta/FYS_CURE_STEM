#Load the rvest library
library(rvest)
library(RCurl)

#get webpage for professional programs
profProgURL <- "https://www.ucdavis.edu/academics/professional-programs"

textFileNumber <- 1

getProgramLinks <- function(url) {
  prof_prog <- read_html(url)
  
  # extract the nodes with links
  prog_nodes <- html_nodes(prof_prog, ".box-wrapper--small a")
  
  # get the attribute values (links) from nodes saved in collapse_nodes
  prog_links <- html_attr(prog_nodes, "href")
  
  prog_links
}

getAcademicLinks <- function(url) {
  academics <- read_html(url)
  
  # extract the nodes with links
  acad_nodes <- html_nodes(academics, ".panel-panel-inner .menu a")
  
  # get the attribute values (links) from nodes saved in collapse_nodes
  acad_links <- html_attr(acad_nodes, "href")

  i <- 1

  for(el in acad_links) {
    acad_links[i] <- paste("https://ucdavis.edu", el, sep = "")
    i = i + 1
  }
  
  acad_links
}

programLinks <- getProgramLinks(profProgURL)
academicLinks <- getAcademicLinks(profProgURL)

convertPage <- function(link) {
  text <- extractText(link)
  writeToFile(text)
}

convertPages <- function(links) {
  for(el in links) {
    convertPage(el)
  }
}

extractText <- function(URL) {
  xData <- read_html(URL)

  #Use XPath to extract the body (go through every page, get the body tag, and run the XML)
  #Value function which returns the value (aka) what's inside of that
  #obj_page_text <- lapply(obj_webpage['//body'], xmlValue)
  
  nodes <- html_nodes(xData, "body")
  text_nodes <- html_text(nodes)
    
  #define a pattern
  pattern <- "<.*?>"
    
  #replace pattern with blank space
  var_plain_text <- gsub(pattern, " ", text_nodes)
    
  #convert multiple spaces to single space
  var_plain_text_trimming <- gsub("\\s+", " ", var_plain_text, TRUE)
    
  return(var_plain_text_trimming)
}


writeToFile <- function(urlText) {
  fileName <- paste(getwd(), "/data/profProgData", textFileNumber, ".txt", sep = "")
  
  file.create(fileName) 
  
  write(urlText, fileName)
  
  textFileNumber <<- textFileNumber + 1
}

convertPage(profProgURL)
convertPages(programLinks)
convertPages(academicLinks)
