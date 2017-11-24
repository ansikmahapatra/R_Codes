library(rvest)
library(httr)
library(RCurl)
library(XML)
#install.packages("twitteR")
library(twitteR)
base_url<-"https://etherscan.io"
url<-"https://etherscan.io/tokens"
urls<-xpathSApply(htmlParse(getURL(url)),path = "//a",xmlGetAttr,"href")
token_links<-c()
index<-1
setup_twitter_oauth(consumer_key = "f7AGFmO36jqK47UrM7XRCUokK",consumer_secret = "VIoWNlE6DakbA29NZSfQ2YIdqoaU7VGgW4JEzWvn6FhfVbgS5T",access_token = "120812257-tAKX4JWz5PHYCg6EqGixinGKyz4scVCF4d123WdR",access_secret = "vQMczjAfM9S4O44JQQVWJbaHzMSOFUlzPovXNmRP1Y2Sj")
for(i in 1:length(urls)){
  if(grepl("/token/",urls[i])==TRUE){
    token_links[index]<-urls[i]
    index<-index+1
  }
}
tokens<-token_links[seq(1,length(token_links),2)]
tokens<-gsub("/token/","",tokens)
contact_address<-twitter_id<-facebook_id<-follower_count<-num_tweet<-c()
index<-1
for(i in 1:length(token_links)){
  web<-read_html(paste0(base_url,"/token/",tokens[i]))
  contact_address[i]<-try({html_text(html_nodes(web,"#ContentPlaceHolder1_trContract a"))},silent = TRUE)
  temp<-xpathSApply(htmlParse(getURL(paste0(base_url,"/token/",tokens[i]))),path = "//a",xmlGetAttr,"href")
  num_tweet<-grep("https://twitter.com/",temp)
  twitter_id[i]<-temp[num_tweet[1]]
  if(twitter_id[i]=="https://twitter.com/etherscan"){
    follower_count[i]<-0
  }
  else{
    follower_count[i]<-followersCount(getUser(gsub("https://twitter.com/","",twitter_id[i])))
  }
}

crypto_currencies<-data.frame(tokens,contact_address,follower_count)
write.csv(crypto_currencies,"~/Desktop/etherscan.csv")

#######################################################################################
##############  New Data Set  ############## All Tokens Must be Downloaded ############
################################### ERC20 Contracts ###################################
#######################################################################################

number<-page_numbers<-c()
one<-"https://etherscan.io/tokens?q="
alphabets<-c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
for(alphabet in 1:length(alphabets)){
  web<-read_html(paste0(one,alphabets[alphabet]))
  number[alphabet]<-html_text(html_nodes(web,"#ContentPlaceHolder1_divpagingpanel .hidden-xs span"))
  page_numbers[alphabet]<-html_text(html_nodes(web,"#ContentPlaceHolder1_PagingPanel span"))
  page_numbers[alphabet]<-as.numeric(gsub("\nPage 1 of ","",page_numbers[alphabet]))
  number[alphabet]<-gsub("A total of ","",number[alphabet])
  number[alphabet]<-gsub(" records found","",number[alphabet])
  number[alphabet]<-as.numeric(number[alphabet])
}
numbers<-0
for(i in 1:length(number)){numbers<-numbers+as.numeric(number[i])}
page<-0
for(j in 1:length(page_numbers)){
  page<-Page+as.numeric(page_numbers[j])
}
table<-c(1:page)
index<-1
for(alphabet in 1:length(alphabets)){
  for(Page in 1:page_numbers[alphabet]){
    two<-paste0(one,alphabets[alphabet],"&p=",as.character(Page))
    table[index]<-html_text(html_nodes(read_html(two),".table-responsive"))
    index<-index+1
  }
}
token_tracker<-data.frame(token_contract=1:numbers,token_name=1:numbers,symbol=1:numbers,decimals=1:numbers,official_site=1:numbers)
index<-1
table<-gsub("\nToken Contract\nTokenName\nSymbol\nDecimals\nOfficial Site\n","",table)
for(i in 1:page){

}

library(RSelenium)
number<-page_numbers<-c()
one<-"https://etherscan.io/tokens?q="
alphabets<-c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
for(alphabet in 1:length(alphabets)){
  web<-read_html(paste0(one,alphabets[alphabet]))
  number[alphabet]<-html_text(html_nodes(web,"#ContentPlaceHolder1_divpagingpanel .hidden-xs span"))
  page_numbers[alphabet]<-html_text(html_nodes(web,"#ContentPlaceHolder1_PagingPanel span"))
  page_numbers[alphabet]<-as.numeric(gsub("\nPage 1 of ","",page_numbers[alphabet]))
  number[alphabet]<-gsub("A total of ","",number[alphabet])
  number[alphabet]<-gsub(" records found","",number[alphabet])
  number[alphabet]<-as.numeric(number[alphabet])
}
numbers<-0
for(i in 1:length(number)){numbers<-numbers+as.numeric(number[i])}
page<-0
for(j in 1:length(page_numbers)){
  page<-Page+as.numeric(page_numbers[j])
}
table_selenium<-c(1:numbers)
index<-1
rD<-rsDriver(port = 4567L, browser = c("chrome"))
remDr<-rD[["client"]]
for(alphabet in 1:length(alphabets)){
  for(Page in 1:page_numbers[alphabet]){
    three<-paste0(one,alphabets[alphabet],"&p=",as.character(Page))
    remDr$navigate(three)
    table_selenium[index]<-remDr$findElement("css selector",".table-responsive")$getElementText()
    table_selenium[index]<-gsub("Token Contract TokenName Symbol Decimals Official Site\n","",table_selenium[index])
    index<-index+1
  }
}
remDr$close()
index<-1
table_selenium<-table_selenium[1:708]
table_frame<-data.frame(token_contract=1:numbers,token_name=1:numbers,
                        symbol=1:numbers,decimals=1:numbers,official_site=1:numbers)
for(i in 1:length(table_selenium)){
    le<-gregexpr("\n",table_selenium[i])[[1]]
    len<-attr(le, "match.length")
    start<-1
    for(j in 1:(length(len)-1)){
      end<-le[j]
      table_frame$token_contract[index]<-as.character(substr(table_selenium[i],start,end))
      start<-le[j]
      index<-index+1
    }
}
for(i in 1:length(table_frame$token_contract)){
  try({
    eins<-gregexpr("https://",table_frame$token_contract[i])[[1]][1]
    zwei<-gregexpr("http://",table_frame$token_contract[i])[[1]][1]
    if(eins>zwei){
      table_frame$official_site[i]<-as.character(substr(table_frame$token_contract[i],eins,nchar(table_frame$token_contract[i])))
    }
    else if(eins<zwei){
      table_frame$official_site[i]<-as.character(substr(table_frame$token_contract[i],zwei,nchar(table_frame$token_contract[i])))
    }
    else{
      table_frame$official_site[i]<-""
    }
    table_frame$token_contract[i]<-gsub(table_frame$official_site[i],"",table_frame$token_contract[i])
    table_frame$token_contract[i]<-trimws(table_frame$token_contract[i])
    table_frame$token_contract[i]<-gsub("-","",table_frame$token_contract[i])
    table_frame$token_contract[i]<-trimws(table_frame$token_contract[i])
    table_frame$decimals[i]<-substr(table_frame$token_contract[i],nchar(table_frame$token_contract[i])-1,nchar(table_frame$token_contract[i]))
    table_frame$token_contract[i]<-gsub(table_frame$decimals[i],"",table_frame$token_contract[i])
    table_frame$token_contract[i]<-trimws(table_frame$token_contract[i])
    table_frame$symbol[i]<-substr(table_frame$token_contract[i],regexpr(" ",table_frame$token_contract[i]),nchar(table_frame$token_contract[i]))
    table_frame$token_contract[i]<-trimws(table_frame$token_contract[i])
    table_frame$token_contract[i]<-gsub(table_frame$symbol[i],"",table_frame$token_contract[i])
    table_frame$token_contract[i]<-trimws(table_frame$token_contract[i])
  },silent = TRUE)
}

library(stringr)
for(i in 1:length(table_frame$token_contract)){
  table_frame$symbol[i]<-trimws(table_frame$symbol[i])
  table_frame$token_name[i]<-word(table_frame$symbol[i],-1)
}
for(i in 1:length(table_frame$token_contract)){
  try({
    table_frame$symbol[i]<-gsub(table_frame$token_name[i],"",table_frame$symbol[i])
  },silent = TRUE)

}
write.csv(table_frame,"~/Desktop/table_frame.csv")
