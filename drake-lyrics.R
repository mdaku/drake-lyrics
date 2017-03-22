# Need to scrape lyrics from here: http://www.azlyrics.com/d/drake.html

# All of the links should start withL http://www.azlyrics.com/lyrics/drake/

# Libraries
library(rvest)      #for webscraping 
library(stringr)
library(tidyverse)

# Here are the helper functions

get_links <- function(url, ll=NULL, ld="/", next_text="Next", prefix="http://") {
  # url = the page to grab
  # ll = the list of links to append to (needed for recursion)
  # ld = the text that denotes a link of interest (could be all links, so default is '/')
  # prefix = the website prefix - often the links will be relational, so you will have to add the absolute path
  
  # This returns a list of all the urls for all of the statements
  
  next_page <- NULL
  link_list <- ll
  
  
  # We want to grab all the data from that URL
  main_page <- read_html(url)
  
  # Once we have the statement values, we want to parse them
  names <- main_page %>%
    html_nodes("a") %>%
    html_text()
  
  hrefs <- main_page %>%
    html_nodes("a") %>%
    html_attr("href")
  
  
  
  all_links <- data.frame(names, hrefs, stringsAsFactors = FALSE)
  interesting_links <- all_links %>% filter(str_detect(hrefs, ld))
  
  # This website does not need a prefix
  # link_list <- interesting_links$hrefs
  link_list <- append(link_list, paste0(prefix, interesting_links$hrefs))
  
  # There are multiple pages, but the last page of speeches will not have a "Next" element
  next_page <- all_links$hrefs[all_links$names == next_text]
  
  # If there is a next page
  if (length(next_page) == 1) {
    message("Moving on to next page")
    return (get_links(
      paste(prefix, next_page, sep = ""),
      ll=link_list,
      ld=ld,
      next_text=next_text,
      prefix=prefix
    ))
  }
  else{
    message("No more pages")
    return (link_list)
  }
}


lex <- function(f, dat="data", md=NULL, sd=NULL, kw=NULL, win=NULL, files=NULL, out="temp.tab"){
  require(tidyverse)
  
  if (is.null(dat) | is.null(f)){
    message("Must provide values for f and dat")
    return (NULL)
  }
  cmd <- str_c("java -jar Lexicoder.jar ", f, " ", "dat=", dat)
  if(!is.null(md)) cmd <- str_c(cmd, " md=", md)
  if(!is.null(sd)) cmd <- str_c(cmd, " sd=", sd)
  if(!is.null(kw)) cmd <- str_c(cmd, " kw=", kw)
  if(!is.null(win)) cmd <- str_c(cmd, " win=", win)
  if(!is.null(files)) cmd <- str_c(cmd, " files=", files)
  cmd <- str_c(cmd,">",out,sep=" ")
  
  # Displays the actual Lexicoder call
  message(cmd)
  system(cmd)
  
  # Reads the temp file in so it can be returned 
  df <- read_delim(out, "\t")
}

download_links <- function(links, new_dir="data", prefix=NULL, suffix=".txt"){
  # Note, this may cause problems if files have the same name or strange names, but they work for now
  # Make sure that there is a directory called data, if not, create it
  if (!dir.exists(new_dir)){
    message("Create new directory")
    dir.create(new_dir)
  }
  # Build the list of file names
  file_names <- str_replace(links, "https://","")
  file_names <- str_replace(links, "http://","")
  file_names <- str_replace(file_names, prefix,"") %>% str_replace_all("/","-") %>% str_c(suffix) 
  
  # file_names <- str_replace(links, "http://www.whitehouse.gov","") %>% str_replace_all("/","-") %>% str_c(suffix) 
  # We may have some strange file_names
  
  file_names <- str_c(new_dir, file_names, sep="/")
  
  #I think this works a little bit better as a named list
  links_list <- as.list(links)
  names(links_list) <- file_names
  
  #check if they exist already 
  exists_logical <-
    map(names(links_list), function(x)
      file.exists(x)) %>% unlist()
  
  #This code chunk will scrape and output if the file doesn't exist already
  if (all(exists_logical)==FALSE) {
    no_exist <- links_list[!exists_logical]
 #   link_text <- map(no_exist, read_html) %>% map(html_text)
    
    for (i in 1:length(link_text)) {
      message(link_text[i])
      #      write_file(link_text[[i]], path = names(link_text[i]))
    }
  }  
  else {
    message("All new links have been downloaded.")
  }
}

all_lyrics <- get_links(url="http://www.azlyrics.com/d/drake.html", ld="lyrics/drake/", next_text="Next", prefix="http://www.azlyrics.com/lyrics/")
download_links(all_lyrics, new_dir="data", prefix="http://www.azlyrics.com/lyrics/drake/")