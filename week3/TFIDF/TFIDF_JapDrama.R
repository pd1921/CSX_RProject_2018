library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)

# get all the address from ptt: https://www.ptt.cc/bbs/marvel/index.html
from <- 2170
to <- 2178
prefix = "https://www.ptt.cc/bbs/marvel/index"

data <- list()
for( id in c(from:to)){
  url <- paste0( prefix, as.character(id), ".html")
  html <- htmlParse( GET(url) )
  url.list <- xpathSApply( html, "//div[@class='title']/a[@herf]", xmlAttrs )
  data <- rbind( data, as.matrix(paste('https://www.ptt.cc', url.list, sep='')) )
}
data <- unlist(data)

head(data)

# use the address to get the data
library(dplyr)
getdoc <- function(url){
  html <- htmlParse( getURL(url))
  doc <- xpathSApply( html, "div[@id='main-content']", xmlValue )
  temp <- gsub( " ", " 0", unlist(time) )
  part <- strsplit( temp, split=" ", fixed=T )
  timestamp <- part[[1]][4]
  timestamp <- strsplit( timestamp, split=":", fixed=T )
  hour <- timestamp[[1]][1]
  print(hour)
  name <- paste0('./DATA/', hour, ".txt")
  write(doc, name, append = TRUE)
}
sapply(data, getdoc)

# clearfiy the data 
d.corpus <- Corpus( DirSource("./DATA") )
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, 
                   function(word) {gsub("[A-Za-z0-9]", "", word)}
                   )

mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)


n = length(seg)
TDM = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0
library(knitr)
kable(head(TDM))
