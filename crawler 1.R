require(tidyverse)
require(rvest)
extract.list<-function(html.obj){
  tibble(
    title=html.obj%>%
      html_nodes("h3")%>%
      html_text,
    href=html.obj%>%
      html_nodes("h3")%>%
      html_nodes("a")%>%
      html_attr("href")%>%
      urlclean
  )
}
class.filter<-function(xmlnodes,class){
  ind<-xmlnodes%>%html_attr("class","NA")==class
  return(xmlnodes[ind])
}

extract.time<-function(html.obj){
  divs<-html.obj%>%html_nodes("div")
  ind<-divs%>%html_attr("class","NA")=="g"
  spans<-divs[ind]%>%html_nodes("span")
  spans%>%attr
}

urlclean<-function(url){
  url%>%
    str_extract("(?<=\\/url\\?q\\=).*(?=\\&sa\\=.*)")
}

top.daily<-function(SearchTerm,Site,N=100,lang="zh-CH"){
  endp<-N%/%10
  addr.head<-str_c(
    "https://www.google.com.sg/search?q=",
    SearchTerm%>%URLencode,
    "+site:",
    Site,
    "&hl=",
    lang,
    "&start="
  )
  top.news<-str_c(addr.head,0:endp*10)%>% 
    map(~read_html(.,encoding="GB2312")%>%
          extract.list)%>%
    bind_rows%>%
    head(100)
  return(top.news)
}

site.list<-c(
  "ftchinese.com",  # FT中文网
  "cn.wsj.com",    # 华尔街时报
  "huanqiu.com",  # 环球网
  "www.bbc.com/zhongwen/simp/business",   # BBC中文
  "cn.reuters.com", # 路透中文
  "wallstreetcn.com",  #华尔街见闻
  "xueqiu.com",   # 雪球
  "moer.cn",    #  摩尔金融
  "www.hexun.com" #  和讯网
)

top.gglnews.daily<-function(SearchTerm,lang="zh-CH",date=Sys.Date(),country="CN",N=20){
  date<-as.Date(date)
  start<-format(date-1,"%m/%d/%Y")
  end<-format(date,"%m/%d/%Y")
  encoding<-ifelse(lang=="zh-CH","GB2312","UTF-8")
  addr.head<-str_c(
    "https://www.google.com.sg/search?",
    "tbm=nws",          # News search
    "&num=",  N,       # N result in one page
    "&q=",SearchTerm,   # Search Term
    "&tbs=ctr:country",country,   # Country
    "&hl=",lang,        # Language
    "&tbs=cdr:1,cd_min:", start,",cd_max:",end  # Search date
  )%>%URLencode
  top.news<-read_html(addr.head,encoding=encoding)%>%
    extract.list%>%
    mutate(Date=date)
  return(top.news)
}

top.gglnews.daily.en<-function(SearchTerm,lang="en",date=Sys.Date(),country="SG",N=20)top.gglnews.daily(SearchTerm,lang,date,country,N)


