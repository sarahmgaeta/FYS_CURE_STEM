# Load the rvest library
library(rvest)
library(RCurl)

# get webpage for academic departments
acad_dept_URL <- "https://www.ucdavis.edu/academics/academic-departments/"

acad_dept_text <- read_html(acad_dept_URL)

# extract the nodes with links
nodes <- html_nodes(acad_dept_text, ".box-wrapper a")

# get the attribute values (links) from nodes saved in collapse_nodes
dept_links <- html_attr(nodes, "href")

textFileNumber <- 1

convertPage <- function(link) {
  text <- extractBody(link)
  if (text != "") {
  writeToFile(text)
  }
}

convertPages <- function(links) {
  for(el in links) {
    convertPage(el)
  }
}

extractBody <- function(URL) {
  tryCatch({
    xData <- read_html(URL)
    
    nodes <- html_nodes(xData, "body")
    text_nodes <- html_text(nodes)
    
    #define a pattern
    pattern <- "<.*?>"
    
    #replace pattern with blank space
    var_plain_text <- gsub(pattern, " ", text_nodes)
    
    #convert multiple spaces to single space
    var_plain_text_trimming <- gsub("\\s+", " ", var_plain_text, TRUE)
    
    return(var_plain_text_trimming)
  },
  error = function(error) {
    print("error in link parsing")
    print(error)
    return("")
  })
}


writeToFile <- function(urlText) {
  fileName <- paste(getwd(), "/data/acadDeptData", textFileNumber, ".txt", sep = "")
  
  file.create(fileName) 
  
  write(urlText, fileName)
  
  textFileNumber <<- textFileNumber + 1
}

convertPage(acad_dept_URL)
convertPages(dept_links)
