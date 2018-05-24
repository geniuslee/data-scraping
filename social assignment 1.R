install.packages("sysfonts")
library("showtext")
library("rtweet")
library("wordcloud")
library("tm")
Sys.setlocale('LC_CTYPE',"en_US.UTF-8")
appname <- "lituntun"
consumerKey <- "Na5eSKM2YlAO0gZBbe8IkmXlE"
consumerSecret <- "QNckLP5sNf3aQB4EwSpnQka8Dk3hwN7zNChlmEvdW1uAZF3sif"
twitter_token <- create_token(app = appname, consumer_key = consumerKey, consumer_secret = consumerSecret, set_renv = TRUE)
taiwan<-search_tweets("#Taiwan",n=1000,include_rts=FALSE)
str(taiwan)
taiwan$text
china<-search_tweets("#China",n=1000,include_rts=FALSE)
class(china)
china$text
Preprocessing <- function(doc){
  #create corpus
  doc.corpus <- Corpus(VectorSource(doc))
  #clean up
  doc.corpus <- tm_map(doc.corpus, function(x)tolower(x))
  doc.corpus <- tm_map(doc.corpus, removePunctuation)   ### remove punctuation
  doc.corpus <- tm_map(doc.corpus, function(x)removeWords(x,stopwords("english")))  #### remove stopwords
  return(doc.corpus)
}

china_text<-paste0(china$text)
china.p<-Preprocessing(china_text)
taiwan_text<-paste0(taiwan$text)
taiwan.p<-Preprocessing(taiwan_text)
class(china.p$content)
combine<-function(content_list){
  out=""
  for (content in content_list){
    out<-paste0(content,out)
  }
  return(out)
}
china.p.combined<-combine(china.p$content)
china.p.combined
taiwan.p.combined<-combine(taiwan.p$content)
tdm <- TermDocumentMatrix(Corpus(VectorSource(c(china.p.combined,taiwan.p.combined)))) 
tdm <- as.matrix(tdm) 
nrow(tdm)
ncol(tdm)
colnames(tdm) <- c("China on Twitter","Taiwan on Twitter")
min.freq <- 10
par(mfrow=c(1,2)) 
par(mar=c(1, 1, 1, 1)) 
par(bg="black") 
par(col.main="white") 
wordcloud(china.p.combined, scale=c(4,.7),min.freq=min.freq, max.words=Inf, random.order=F, colors=brewer.pal(7, "Dark2"))   
title("China on Twitter")
wordcloud(taiwan.p.combined, scale=c(4,.5),min.freq=min.freq, max.words=Inf, random.order=F, colors=brewer.pal(8, "Dark2"))   
title("Taiwan on Twitter")


if (!require("RCurl")) install.packages("RCurl", repos="https://cran.cnr.berkeley.edu/", dependencies = TRUE)
if (!require("RJSONIO")) install.packages("RJSONIO", repos="https://cran.cnr.berkeley.edu/", dependencies = TRUE)
if (!require("plotly")) install.packages("plotly", repos="https://cran.cnr.berkeley.edu/", dependencies = TRUE)
library(RJSONIO)
library(RCurl)
library(plotly)
api<- "a6599a65d21c44f9aea55c50a6a127c2" 
search_q <- URLencode("'Taiwan'")
year_range <- 1987:2017 
nyt_taiwan <- data.frame(year=character(0),hits=numeric(0))
for(year in year_range){
  url <- paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',search_q,'&begin_date=',year,'0101&end_date=',year,'1231&api-key=',api,sep="")
  nyt_robj <- fromJSON(getURL(url))  
  hits <- nyt_robj$response$meta["hits"]
  nyt_taiwan <- rbind(nyt_taiwan,data.frame(year=year,hits=hits)) 
  print(paste(year,hits,sep=":")) 
  Sys.sleep(5) 
}
search_q <- URLencode("'China'")
nyt_china <- data.frame(year=character(0),hits=numeric(0))
for(year in year_range){
  url <- paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',search_q,'&begin_date=',year,'0101&end_date=',year,'1231&api-key=',api,sep="")
  nyt_robj <- fromJSON(getURL(url))  
  hits <- nyt_robj$response$meta["hits"]
  nyt_china <- rbind(nyt_china,data.frame(year=year,hits=hits)) 
  print(paste(year,hits,sep=":")) 
  Sys.sleep(5) }
p <- plot_ly(x=nyt_taiwan$year, y=nyt_taiwan$hits, name = "taiwan", type = 'scatter', mode = 'lines')
p <- add_trace(p, y= nyt_china$hits, name= "china")
layout(p, title = "Region's Names mentioned in New York Times (1987 to 2017)", xaxis= list(title = "Year"), yaxis= list (title = "Number of hits"))

if (!require("RCurl")) install.packages("RCurl", repos="https://cran.cnr.berkeley.edu/", dependencies = TRUE)
if (!require("RJSONIO")) install.packages("RJSONIO", repos="https://cran.cnr.berkeley.edu/", dependencies = TRUE)
if (!require("httr")) install.packages("httr", repos="https://cran.cnr.berkeley.edu/", dependencies = TRUE)

library(RCurl)
library(RJSONIO)
library(httr)
token_fb <- function(){ 
  token <- httr::GET("https://graph.facebook.com/oauth/access_token", 
                     query = list(client_id="183110145784544",
                                  client_secret="eaddd8645924c2395c909afaf728453b",            
                                  grant_type = "client_credentials"))
  return(httr::content(token))
}

read_fbpgposts_ALL <- function(pid,limit=100){
  result <- list()
  url <- paste0("https://graph.facebook.com/v2.12/",pid,"/posts?fields=id,parent_id,updated_time,created_time,link,message,comments.summary(true),likes.summary(true),shares&limit=",limit,"&access_token=",token_fb())
  while (!is.null(url)){  # The while loop and check if the url is null.
    fbposts <- getURL(url)  # if not null, read the url
    if (!is.null(fbposts)){ # if at least one post
      out <- fromJSON(fbposts)
      result <- c(result,out$data) # Append the data to the list
      print(length(result)) # Show number of posts in each loop
    }
    url <- out$paging$`next`
  }
  return(result)
}
politic<- read_fbpgposts_ALL("62317591679")
politic[[1]]$message
politic_list<-sapply(1:500,function(x){politic[[x]]$message},simplify=FALSE)
x<-1
read_fbpgposts <- function(pid,limit=100){
  result<-list()
  url <- paste0("https://graph.facebook.com/v2.12/",pid,"/posts?fields=name,fan_count,about,message,shares&limit=",limit,"&access_token=",token_fb())
  for(x in 1:5){
    out <- fromJSON(getURL(url))
    url <- out$paging$`next`
    result <- c(result,out$data)
    x<-x+1
  }
  return(result)
}
ABC<-read_fbpgposts("86680728811")
ABC_list<-sapply(1:500,function(x){ABC[[x]]$message},simplify=FALSE)
889307941125736

cnn<-read_fbpgposts("5550296508")
cnn_list<-sapply(1:500,function(x){cnn[[x]]$message},simplify=FALSE)

bbc<-read_fbpgposts("228735667216")
bbc_list<-sapply(1:500,function(x){bbc[[x]]$message},simplify=FALSE)
bbc_list
cnn_list
