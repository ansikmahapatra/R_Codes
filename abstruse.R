library(rvest)
library(httr)
library(RCurl)
library(XML)
comic<-572
ur<-"http://abstrusegoose.com/"
while(comic>1){
  abstruse<-paste0(ur,as.character(comic))
  xpaths<-"//section//img"
  try({
    urls<-xpathSApply(htmlParse(getURL(abstruse)),xpaths,xmlGetAttr,"src")
    download.file(urls,paste0("~/OneDrive/WebComix/AbstruseGoose/",as.character(comic),".png"))
  },silent = TRUE)
  comic<-comic-1
}
