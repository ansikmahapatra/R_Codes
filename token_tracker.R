library(RSelenium)
library(stringr)
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
table_selenium<-table_selenium[1:708]
table_frame<-data.frame(token_contract=1:numbers,token_name=1:numbers,
                        symbol=1:numbers,decimals=1:numbers,official_site=1:numbers)
index<-1
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

table_frame$token_contract<-trimws(table_frame$token_contract)
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
    }},silent = TRUE)
  table_frame$token_contract[i]<-gsub(table_frame$official_site[i],"",table_frame$token_contract[i])
}
table_frame$token_contract<-trimws(table_frame$token_contract)
table_frame$decimals<-""
for(i in 1:length(table_frame$token_contract)){
  try({
    table_frame$token_contract[i]<-gsub("-","",table_frame$token_contract[i])
    table_frame$token_contract[i]<-gsub("(","",table_frame$token_contract[i])
    table_frame$token_contract[i]<-gsub(")","",table_frame$token_contract[i])
    table_frame$token_contract[i]<-trimws(table_frame$token_contract[i])
  },silent = TRUE)
}
table_frame$token_contract<-trimws(table_frame$token_contract)
for(i in 1:length(table_frame$token_contract)){
  try({
    table_frame$decimals[i]<-word(table_frame$token_contract[i],-1)
    table_frame$token_contract[i]<-gsub(table_frame$decimals[i],"",table_frame$token_contract[i])
  },silent = TRUE)
}
table_frame$token_contract<-trimws(table_frame$token_contract)
for(i in 1:length(table_frame$token_contract)){
  try({
    table_frame$symbols[i]<-word(table_frame$token_contract[i],-1)
    table_frame$token_contract[i]<-gsub(table_frame$symbols[i],"",table_frame$token_contract[i])
  },silent = TRUE)
}
table_frame$token_contract<-trimws(table_frame$token_contract)
for(i in 1:length(table_frame$token_contract)){
    table_frame$symbols[i]<-word(table_frame$token_contract[i],-1)
}
write.csv(table_frame,"~/Desktop/table_frame.csv")
#4187, 4189