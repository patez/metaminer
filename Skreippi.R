library(tidyverse)
library(urltools)
#library(tidyjson)
#library(xml2)


setwd("C:/Users/pauli_000/OneDrive - Oulun yliopisto/gradu/csv")
setwd("/Users/patez/OneDrive - Oulun yliopisto/gradu/csv")

#helper function to read files with the file name column added
my_read_csv <- function(x) {
  out <- read_csv2(x, col_types = cols(.default = "c"))
  cbind(Search=x, out)
}

add_robot_path <- function (x) {
  x <- x %>% 
    # clean urls and add robots.txt to the end
    mutate(Url_robot_path = paste(scheme(Url),"://", domain(Url), "/", "robots.txt",sep = ""))
}

get_robot_txt <- function (x) {
  
  content <- try(read_file(x))
  if (class(content) == "try-error")
    content <- "file not found"
  content
}

#Get pages for dataframe, must contain column Url.
#Saves to working directory numbered html files.
#Using PhantomJS and a simple javascript snippet scrape_final.js

getPages <- function (df, from, to) {
  
  for (i in from:to) {
    ## change Phantom.js scrape file
    
    lines <- lines <- readLines("scrape_final.js")
    lines[1] <- paste0("var url ='", df$Url[i] ,"';")
    lines[2] <- paste0("var i ='", i ,"';")
    writeLines(lines, "scrape_final.js")
    
    ## Download website
    system("phantomjs scrape_final.js")
  }
  
}

#add JSON data from the path 
addJson <- function(df, path) {
  
  df <- df %>%
    mutate(JSON = "")
  
  
  for (i in df$id) {
    raw <- try(read_file(paste0(path, i ,".json")))
    if (class(raw) == "try-error")
      raw <- "{\"microdata\": [], \"json-ld\": [], \"opengraph\": [], \"microformat\": [], \"rdfa\": []}"
    df$JSON[i] <- raw
  }
  df
}


#load csv files saved from google result pages manually, using SEMrush tools
ptrn = "1.*.csv"
filenames <- list.files(pattern = ptrn)
data <- lapply(filenames, my_read_csv) %>% 
  bind_rows()

#remove some unwanted columns
data <- data %>%
  rename(Rank = "\u0023") %>% 
    select('Search','Url','Rank','Webarchive age','Bing index','SEMrush Rank')
   
data <- add_robot_path(data)

#add id column
id <- rownames(data)
data <- cbind(id=id, data)


# get the actual robot.txt
mutate(data, robot_content = "")    
for (i in 1:nrow(data)) {
  data$robot_content[i] <- get_robot_txt(data$Url_robot_path[i])
}



data <- data %>%
  mutate(Rank = as.numeric(Rank), `SEMrushRank` = as.numeric(as.character(`SEMrush Rank`))) %>%
  select(-`SEMrush Rank`)

data <- data %>%
  mutate(Domain = domain(Url))


# Example how to get more data for a dataframe, column Url is mandatory
# Get all links, filter disallowed sites

dataUuvuksissa <- filter(dataExtra, Search == "1uuvuksissa kirja.csv") 
dontCrawl <- c("www.tori.fi","www.facebook.com","twitter.com","www.instagram.com")

dataCrawled <- data1 %>% 
  filter(!Domain %in% dontCrawl)

dataCrawled$id <- 1:nrow(dataCrawled)

#didnt get www.ksml.fi 25, 75, 514, 549 ... elaparemmin.fi 208
getPages(dataCrawled, 800, 814) #actually download the pages, saves to working directory

# Path to html files from working directory, run the script with it as argument, need to escape some quotes for terminal

path <- paste0("\"", getwd(), "/html/", "\"")
system(paste0("python metaextractor.py ", path))
# couldnt parse 732 

path <- gsub('"',  "", path) #remove extra quotes
dataUu <- addJson(dataCrawled, path) #add metadata from the JSON files in directory

##add id column


##file loading and saving examples

saveRDS(data, file="data.Rda")
saveRDS(data6,file="dataExtra.Rda")
saveRDS(dataUuvuksissa,file="uuvuksissa/dataUuvuksissa.Rda")

saveRDS(dataHighest,file="highest/dataHighest.Rda")
saveRDS(parsedJSONHigh,file="highest/parsedJSONHigh.Rda")
saveRDS(dataCrawled,file="dataCrawled.Rda")
saveRDS(parsedJSON,file="parsedJSON.Rda")
saveRDS(parsedJSONuu,file="uuvuksissa/parsedJSONuu.Rda")

data <- readRDS("data.Rda")
dataHighest <- readRDS("highest/dataHighest.Rda")
dataCrawled <- readRDS("dataCrawled.Rda")


### some extra code, propably not needed
###
###
dataCrawledd <- dataCrawled %>%
  mutate(JSON = replace(JSON, JSON == "file not found", "{\"microdata\": [], \"json-ld\": [], \"opengraph\": [], \"microformat\": [], \"rdfa\": []}"))






