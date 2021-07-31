library(rvest)     # Library for web scraping
library(stringr)   # Library for manipulating character (string) vectors
library(tidyverse)
library(lubridate)
library(anytime)
library(openxlsx)


rm(list=ls())

# MAIN URL SITE
main_site_url <- "https://konservative.dk/partiet/vaelgerforeninger/"

#Read html and specify which "data" to scrape
main_site_url_html <- read_html(main_site_url)

#this is the sepcified data to scrape
data_table_selector <- ".name"


#scraping data from url and specified CSS
data_table <- main_site_url_html %>%
  html_elements(css = data_table_selector) %>%
  html_text2()

#scraping data from url and specified CSS
page_table <- main_site_url_html %>%
  html_elements(css = data_table_selector) %>%
  html_element("a") %>% 
  html_attr("href")


D <- list()
DD <- list()

for(i in c(2:16, 18:33, 35:60, 64:99)) {
  # Visual representation of current politiicans R loads followers from:
  cat("Loading adresser", i , "...\n")
  
  # Actual code for extracting the followers of the polticians. First part takes the name of the file and converts it into a individual list for the user_id without "...csv.bz2"
  # Afterwards it reads the csv of the politicians followers into this empty list
  current_adress <- page_table[[i]]
  
  current_adress <- read_html(current_adress)
  adress_id <- ".mail .social-icon , .name"
  
  D[[data_table[i]]] <- current_adress %>%
    html_elements(css = adress_id) %>%
    html_text2()
  
  adress_id <- ".social.mail"
  
  DD[[data_table[i]]] <- current_adress %>%
    html_elements(css = adress_id) %>%
    html_element("a") %>% 
    html_attr("href")
  
  
 #date_time<-Sys.time()
  #while((as.numeric(Sys.time()) - as.numeric(date_time))<0.3){}
 
   tryCatch({
    print(i)
    if (i==c(18,32,62)) stop("Urgh, the iphone is in the blender !")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }

all_D <- unlist(D)

all_DD <- unlist(DD)

all_D <- all_D[!all_D %in% "Send e-mail"]

all_D <- split(all_D, ceiling(seq_along(all_D)/1))

all_DD <- split(all_DD, ceiling(seq_along(all_DD)/1))

rm(k)

i=1

#for(i in 1:length(all_D)) {
 # cat("Loading adresser", i , "...\n")
  #
  # all_D[[paste0(i, sep="")]] <- append(all_D[[paste0(i, sep="")]], attr(all_D[[paste0(i, sep="")]], "names"))

#}

for(i in 1:length(all_DD)) {
  cat("Loading adresser", i , "...\n")
  
  all_DD[[paste0(i, sep="")]] <- append(all_DD[[paste0(i, sep="")]], attr(all_DD[[paste0(i, sep="")]], "names"))
  
}

#converts list into another chr variable which (somehow) can be converted into a data.frame
X <- do.call(rbind, all_D)

X2 <- do.call(rbind, all_DD)

X <- as.data.frame(X)

colnames(X)[1] <- "Navn"

X <- X %>% separate(Navn, c("Navn", "Position"), "\n", extra = "merge")

X2 <- as.data.frame(X2)

colnames(X2)[1:2] <- c("Email", "Forening")

X2$Forening <- str_remove_all(X2$Forening, "[0123456789]")

test <- bind_cols(X, X2)

write.xlsx(test, "/Users/anderstoft/Downloads/web_kons_v2.xlsx")

