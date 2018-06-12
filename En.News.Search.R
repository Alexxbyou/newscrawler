require(tidyverse)
require(rvest)


# Variables
KW="uob"
from<-"20180610"
to<-"20180611"

# Functions
date.parse<-function(dt){
  result<-c(
    substring(dt,1,4),
    substring(dt,5,6),
    substring(dt,7,8)
  )
  names(result)<-c("year","month","day")
  return(result)
}

srch.date.fmt<-function(dt){
  str_c(
    substring(dt,5,6),"/",
    substring(dt,7,8),"/",
    substring(dt,1,4)
  )
}


search.date.range<-function(from,to){
  str_c(
    "&tbs=cdr:1,cd_min:",
    srch.date.fmt(from),
    ",cd_max:",
    srch.date.fmt(to)
  )
}

search.date.range(from,to)


extr.href<-function(href){
  href%>%
    str_extract("(?<=\\/url\\?q=)[^\\&]*")
}

KW.srch.news<-function(KW,from,to){
  html.addr<-str_c(
    "https://www.google.com.sg/search?q=",KW,
    "&source=lnt&tbm=nws&lr=lang_en&num=100",
    search.date.range(from,to)
  )
  
  rslt.box<-html.addr%>%
    read_html%>%
    html_nodes("h3 a")
  
  srch.result<-tibble(
    title=rslt.box%>%
      html_text,
    link=rslt.box%>%
      html_attr("href")%>%
      extr.href
  )
  
  return(srch.result)
}

srch.result<-KW.srch.news("uob",from,to)

read_html_t<-function(addr){
  tryCatch(
    read_html(addr), 
    error = function(e){
      NULL
    }
  )
}

get.article<-function(addr){
  html.obj<-addr%>%
    read_html_t
  if(!is.null(html.obj))
    html.obj%>%
    html_nodes("p")%>%
    html_text%>%
    paste(collapse="\n")
}



#sapply(srch.result$link[1:10],get.article)

#srch.result$link[3]%>%get.article

