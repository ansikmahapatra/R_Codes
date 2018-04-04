#New Way to Download things off LexisNexis.

library(RSelenium)
require(RSelenium)
library(XML)

rD<-rsDriver(port = 4567L, browser = c("chrome")) 
remDr<-rD[["client"]]

rD_2<-rsDriver(port = 4568L, browser = c("chrome")) 
remDr_2<-rD_2[["client"]]

search<-c("bitcoin")
folders<-c("LexisNexis_New")

source_dir<-"~/Downloads/"
directory<-paste0("~/OneDrive/",folders[1],"/")

start_date<-"2017-01-01"
end_date<-Sys.Date()

one<-"https://www.nexis.com/api/version1/au?la=en"

remDr$navigate(one)
remDr_2$navigate(one)
remDr$findElement('xpath',"/html/body/table/tbody/tr[2]/td/table/tbody/tr/td[2]/table/tbody/tr[4]/td[2]/table/tbody/tr/td/table/tbody/tr[1]/td/a[1]")$clickElement()
remDr_2$findElement('xpath',"/html/body/table/tbody/tr[2]/td/table/tbody/tr/td[2]/table/tbody/tr[4]/td[2]/table/tbody/tr/td/table/tbody/tr[1]/td/a[1]")$clickElement()
remDr$findElement('css selector',"#secondarytabs .active+ li .null")$clickElement()
remDr_2$findElement('css selector',"#secondarytabs .active+ li .null")$clickElement()
remDr$sendKeysToActiveElement(list(search[element]))
remDr_2$sendKeysToActiveElement(list(search[element]))
id<-c("groupDuplicates","searchNaturalStyle")
for(i in 1:length(id)){remDr$findElement('id',id[i])$clickElement()
  remDr_2$findElement('id',id[i])$clickElement()
  }
remDr$findElement('id',"sourceSelectDDStyle")$clickElement()
remDr_2$findElement('id',"sourceSelectDDStyle")$clickElement()
remDr$findElement('xpath', "//*/option[@value = 'F_GB00NBGenSrch.CS00900470;All English Language News']")$clickElement() 
remDr_2$findElement('xpath', "//*/option[@value = 'F_GB00NBGenSrch.CS00900470;All English Language News']")$clickElement() 
remDr$findElement('xpath', "//*/option[@value = 'from']")$clickElement() 
remDr_2$findElement('xpath', "//*/option[@value = 'from']")$clickElement() 
remDr$findElement('id',"fromDate")$clickElement()
remDr_2$findElement('id',"fromDate")$clickElement()
for(t in 1:6){
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$sendKeysToActiveElement(list(key = "backspace"))
  remDr_2$sendKeysToActiveElement(list(key = "delete"))
  remDr_2$sendKeysToActiveElement(list(key = "backspace"))
}
date_<-paste0(
  as.character(format(as.Date(start_date,format="%d/%m/%y"),"%d")),"/",
  as.character(format(as.Date(start_date,format="%d/%m/%y"),"%m")),"/","20",
  as.character(format(as.Date(start_date,format="%d/%m/%y"),"%y")))
remDr$sendKeysToActiveElement(list(date_))
remDr_2$sendKeysToActiveElement(list(date_))
remDr$findElement('id',"toDate")$clickElement()
remDr_2$findElement('id',"toDate")$clickElement()
for(t in 1:6){
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$sendKeysToActiveElement(list(key = "backspace"))
  remDr_2$sendKeysToActiveElement(list(key = "delete"))
  remDr_2$sendKeysToActiveElement(list(key = "backspace"))
}
date_<-paste0(
  as.character(format(as.Date(end_date,format="%d/%m/%y"),"%d")),"/",
  as.character(format(as.Date(end_date,format="%d/%m/%y"),"%m")),"/","20",
  as.character(format(as.Date(end_date,format="%d/%m/%y"),"%y")))
remDr$sendKeysToActiveElement(list(date_))
remDr_2$sendKeysToActiveElement(list(date_))
remDr$findElement('id',"enableSearchImg")$clickElement()
remDr_2$findElement('id',"enableSearchImg")$clickElement()

#All New Relevant ID's
Ref_ID<-"delivery_refworks"
string_to_be_removed<-"1 - 25 of " #followed by *
num_of_results<-remDr$findElement('id',"pagination")$getElementText()
num_of_results<-gsub("*","",num_of_results)
num_of_results<-gsub(string_to_be_removed,"",num_of_results)
num_of_results<-as.integer(num_of_results)

num_of_loops<-ceiling(num_of_results/200)

start<-1
end<-200

if(num_of_results<200){end<-num_of_results}

download_text<-function(){
  remDr$findElement('id',"delivery_DnldRender")$clickElement()
  remDr$findElement('link text',"Download Options")$clickElement() 
  remDr$findElement('id',"sel")$clickElement()
  remDr$findElement('id',"selDocs")$clickElement()
  remDr$sendKeysToActiveElement(list(start,"-",end))
  Sys.sleep(2)
  id<-c("chk1","chk6","chk7","chk8","chk9","chk10","chk12","chk13",
        "chk21","chk22","chk23","chk24","chk25","chk27","chk35","chk37",
        "chk38","chk41","chk45","chk46","chk48","chk49")
  Sys.sleep(2)
  remDr$findElement('link text',"Format Options")$clickElement() 
  remDr$findElement('xpath',"//*/option[@value = 'QDS_EF_GENERICTYPE']")$clickElement() 
  remDr$findElement('xpath',"//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"deliverBtn\", \" \" ))]//img")$clickElement() 
}

download_info<-function(){
  currWindow<-remDr_2$getCurrentWindowHandle()
  windows<-remDr_2$getWindowHandles()
  for (window in windows[[1]]) {
    if (window != currWindow[[1]]) 
      remDr_2$switchToWindow(window)
  }
  remDr_2$findElement('id',Ref_ID)$clickElement()
  remDr_2$findElement('id',"selDocs")$clickElement()
  remDr_2$sendKeysToActiveElement(list(start,"-",end))
  remDr_2$findElement('id',"dnldBiblio")$clickElement()
  remDr_2$findElement('link text',"Export")$clickElement()
  Sys.sleep(2)
  remDr_2$closeWindow()
}

remainder<-num_of_results%%200

for(i in 1:num_of_loops){
  download_text()
  while(length(list.files(source_dir))==0){}
  remDr$findElement('id',"closeBtn")$clickElement()
  Sys.sleep(15)
  source_text<-dir(source_dir,pattern = ".TXT")
  files.text<-dir(directory, pattern = ".TXT")
  source_text_data<-paste0(source_dir,source_text[1])
  string_text<-paste0("News Articles ",as.character(start)," to ",as.character(end),".TXT")
  string_text<-paste0(directory,string_text)
  file.copy(source_text_data,string_text)
  file.remove(source_text_data[1])
  download_info()
  while(length(list.files(source_dir))==0){}
  source_text<-dir(source_dir,pattern = ".TXT")
  files.text<-dir(directory, pattern = ".TXT")
  source_text_data<-paste0(source_dir,source_text[1])
  string_text<-paste0("News Info ",as.character(start)," to ",as.character(end),".TXT")
  string_text<-paste0(directory,string_text)
  file.copy(source_text_data,string_text)
  file.remove(source_text_data[1])
  start<-(i*200)+1
  end<-(i+1)*200
  if(i==num_of_loops-1){end<-(i*200)+remainder}
}