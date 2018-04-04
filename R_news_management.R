parent_directory<-"~/Dropbox/thesis/7Text/"
folders<-c("dzbank","deutschebank","wgzbank","commerzbank","bayernlb","hshnordbank"
           ,"landesbankbaden","landesbankhessen","norddeutschelandesbank")
num<-c()

for(folder in 1:length(folders)){
  directory<-paste0(parent_directory,folders[folder],"/")
  f<-list.files(directory)
  setwd(directory)
  num[folder]<-0
  for(files in 1:length(dir(directory))){
    try({
      con=file(f[files],open = "r")
      lines=readLines(con)
      file_length<-length(lines)
      num[folder]<-num[folder]+as.integer(substr(trimws(lines[5]),37,nchar(trimws(lines[5]))-1))
      close(con)
    },silent = TRUE)
  }
}

deutschebank<-data.frame(
                      date=1:num[2],
                      load_date=1:num[2],
                      len=1:num[2],
                      pub_type=1:num[2],
                      news=1:num[2],
                      file=1:num[2],
                      line=1:num[2])
for(index in 1:num[2]){
  deutschebank$news[index]<-""
  deutschebank$pub_type[index]<-""
}
write.csv(deutschebank,file="~/Dropbox/thesis/12Project/deutschebank.csv")

wgzbank<-data.frame(
                      date=1:num[3],
                      load_date=1:num[3],
                      len=1:num[3],
                      pub_type=1:num[3],
                      news=1:num[3],
                      file=1:num[3],
                      line=1:num[3])
for(index in 1:num[3]){
  wgzbank$news[index]<-""
  wgzbank$pub_type[index]<-""
}
write.csv(wgzbank,file="~/Dropbox/thesis/12Project/wgzbank.csv")


commerzbank<-data.frame(
                      date=1:num[4],
                      load_date=1:num[4],
                      len=1:num[4],
                      pub_type=1:num[4],
                      news=1:num[4],
                      file=1:num[4],
                      line=1:num[4])
for(index in 1:num[4]){
  commerzbank$news[index]<-""
  commerzbank$pub_type[index]<-""
}
write.csv(commerzbank,file="~/Dropbox/thesis/12Project/commerzbank.csv")


bayernlb<-data.frame(
        	      date=1:num[5],
                      load_date=1:num[5],
                      len=1:num[5],
                      pub_type=1:num[5],
                      news=1:num[5],
                      file=1:num[5],
                      line=1:num[5])
for(index in 1:num[5]){
  bayernlb$news[index]<-""
  bayernlb$pub_type[index]<-""
}
write.csv(bayernlb,file="~/Dropbox/thesis/12Project/bayernlb.csv")


hshnordbank<-data.frame(
                      date=1:num[6],
                      load_date=1:num[6],
                      len=1:num[6],
                      pub_type=1:num[6],
                      news=1:num[6],
                      file=1:num[6],
                      line=1:num[6])
for(index in 1:num[6]){
  hshnordbank$news[index]<-""
  hshnordbank$pub_type[index]<-""
}
write.csv(hshnordbank,file="~/Dropbox/thesis/12Project/hshnordbank.csv")


landesbankbaden<-data.frame(
                      date=1:num[7],
                      load_date=1:num[7],
                      len=1:num[7],
                      pub_type=1:num[7],
                      news=1:num[7],
                      file=1:num[7],
                      line=1:num[7])
for(index in 1:num[7]){
  landesbankbaden$news[index]<-""
  landesbankbaden$pub_type[index]<-""
}
write.csv(landesbankbaden,file="~/Dropbox/thesis/12Project/landesbankbaden.csv")


landesbankhessen<-data.frame(
                      date=1:num[8],
                      load_date=1:num[8],
                      len=1:num[8],
                      pub_type=1:num[8],
                      news=1:num[8],
                      file=1:num[8],
                      line=1:num[8])
for(index in 1:num[8]){
  landesbankhessen$news[index]<-""
  landesbankhessen$pub_type[index]<-""
}
write.csv(landesbankhessen,file="~/Dropbox/thesis/12Project/landesbankhessen.csv")

norddeutschelandesbank<-data.frame(
                      date=1:num[9],
                      load_date=1:num[9],
                      len=1:num[9],
                      pub_type=1:num[9],
                      news=1:num[9],
                      file=1:num[9],
                      line=1:num[9])
for(index in 1:num[9]){
  norddeutschelandesbank$news[index]<-""
  norddeutschelandesbank$pub_type[index]<-""
}
write.csv(norddeutschelandesbank,file="~/Dropbox/thesis/12Project/norddeutschelandesbank.csv")


dzbank<-data.frame(   date=1:num[1],
                      load_date=1:num[1],
                      len=1:num[1],
                      pub_type=1:num[1],
                      news=1:num[1],
                      file=1:num[1],
                      line=1:num[1])
for(index in 1:num[1]){
  dzbank$news[index]<-""
  dzbank$pub_type[index]<-""
}
write.csv(dzbank,file="~/Dropbox/thesis/12Project/dzbank.csv")

directory<-paste0(parent_directory,folders[2],"/")
f<-list.files(directory)
setwd(directory)
p<-1
for(files in 1:length(dir(directory))){
  try({
    con=file(f[files],open = "r")
    lines=readLines(con)
    file_length<-length(lines)
    limit<-as.integer(substr(trimws(lines[5]),37,nchar(trimws(lines[5]))-1))
    limit_index<-1
    for(l in 1:file_length){
      if(paste0(as.character(limit_index)," of ",as.character(limit)," DOCUMENTS")%in%trimws(lines[l])){
        deutschebank$line[p]<-l
        deutschebank$file[p]<-files
        p<-p+1
        limit_index<-limit_index+1
      }
    }  
    close(con)
  },silent = TRUE)
}
write.csv(deutschebank,file = "~/Dropbox/thesis/12Project/deutschebank.csv")

directory<-paste0(parent_directory,folders[3],"/")
f<-list.files(directory)
setwd(directory)
p<-1
for(files in 1:length(dir(directory))){
  try({
    con=file(f[files],open = "r")
    lines=readLines(con)
    file_length<-length(lines)
    limit<-as.integer(substr(trimws(lines[5]),37,nchar(trimws(lines[5]))-1))
    limit_index<-1
    for(l in 1:file_length){
      if(paste0(as.character(limit_index)," of ",as.character(limit)," DOCUMENTS")%in%trimws(lines[l])){
        wgzbank$line[p]<-l
        wgzbank$file[p]<-files
        p<-p+1
        limit_index<-limit_index+1
      }
    }  
    close(con)
  },silent = TRUE)
}
write.csv(wgzbank,file = "~/Dropbox/thesis/12Project/wgzbank.csv")

directory<-paste0(parent_directory,folders[4],"/")
f<-list.files(directory)
setwd(directory)
p<-1
for(files in 1:length(dir(directory))){
  try({
    con=file(f[files],open = "r")
    lines=readLines(con)
    file_length<-length(lines)
    limit<-as.integer(substr(trimws(lines[5]),37,nchar(trimws(lines[5]))-1))
    limit_index<-1
    for(l in 1:file_length){
      if(paste0(as.character(limit_index)," of ",as.character(limit)," DOCUMENTS")%in%trimws(lines[l])){
        commerzbank$line[p]<-l
        commerzbank$file[p]<-files
        p<-p+1
        limit_index<-limit_index+1
      }
    }  
    close(con)
  },silent = TRUE)
}
write.csv(commerzbank,file = "~/Dropbox/thesis/12Project/commerzbank.csv")

directory<-paste0(parent_directory,folders[5],"/")
f<-list.files(directory)
setwd(directory)
p<-1
for(files in 1:length(dir(directory))){
  try({
    con=file(f[files],open = "r")
    lines=readLines(con)
    file_length<-length(lines)
    limit<-as.integer(substr(trimws(lines[5]),37,nchar(trimws(lines[5]))-1))
    limit_index<-1
    for(l in 1:file_length){
      if(paste0(as.character(limit_index)," of ",as.character(limit)," DOCUMENTS")%in%trimws(lines[l])){
        bayernlb$line[p]<-l
        bayernlb$file[p]<-files
        p<-p+1
        limit_index<-limit_index+1
      }
    }  
    close(con)
  },silent = TRUE)
}
write.csv(bayernlb,file = "~/Dropbox/thesis/12Project/bayernlb.csv")

directory<-paste0(parent_directory,folders[6],"/")
f<-list.files(directory)
setwd(directory)
p<-1
for(files in 1:length(dir(directory))){
  try({
    con=file(f[files],open = "r")
    lines=readLines(con)
    file_length<-length(lines)
    limit<-as.integer(substr(trimws(lines[5]),37,nchar(trimws(lines[5]))-1))
    limit_index<-1
    for(l in 1:file_length){
      if(paste0(as.character(limit_index)," of ",as.character(limit)," DOCUMENTS")%in%trimws(lines[l])){
        hshnordbank$line[p]<-l
        hshnordbank$file[p]<-files
        p<-p+1
        limit_index<-limit_index+1
      }
    }  
    close(con)
  },silent = TRUE)
}
write.csv(hshnordbank,file = "~/Dropbox/thesis/12Project/hshnordbank.csv")

directory<-paste0(parent_directory,folders[7],"/")
f<-list.files(directory)
setwd(directory)
p<-1
for(files in 1:length(dir(directory))){
  try({
    con=file(f[files],open = "r")
    lines=readLines(con)
    file_length<-length(lines)
    limit<-as.integer(substr(trimws(lines[5]),37,nchar(trimws(lines[5]))-1))
    limit_index<-1
    for(l in 1:file_length){
      if(paste0(as.character(limit_index)," of ",as.character(limit)," DOCUMENTS")%in%trimws(lines[l])){
        landesbankbaden$line[p]<-l
        landesbankbaden$file[p]<-files
        p<-p+1
        limit_index<-limit_index+1
      }
    }  
    close(con)
  },silent = TRUE)
}
write.csv(landesbankbaden,file = "~/Dropbox/thesis/12Project/landesbankbaden.csv")

directory<-paste0(parent_directory,folders[8],"/")
f<-list.files(directory)
setwd(directory)
p<-1
for(files in 1:length(dir(directory))){
  try({
    con=file(f[files],open = "r")
    lines=readLines(con)
    file_length<-length(lines)
    limit<-as.integer(substr(trimws(lines[5]),37,nchar(trimws(lines[5]))-1))
    limit_index<-1
    for(l in 1:file_length){
      if(paste0(as.character(limit_index)," of ",as.character(limit)," DOCUMENTS")%in%trimws(lines[l])){
        landesbankhessen$line[p]<-l
        landesbankhessen$file[p]<-files
        p<-p+1
        limit_index<-limit_index+1
      }
    }  
    close(con)
  },silent = TRUE)
}
write.csv(landesbankhessen,file = "~/Dropbox/thesis/12Project/landesbankhessen.csv")

directory<-paste0(parent_directory,folders[9],"/")
f<-list.files(directory)
setwd(directory)
p<-1
for(files in 1:length(dir(directory))){
  try({
    con=file(f[files],open = "r")
    lines=readLines(con)
    file_length<-length(lines)
    limit<-as.integer(substr(trimws(lines[5]),37,nchar(trimws(lines[5]))-1))
    limit_index<-1
    for(l in 1:file_length){
      if(paste0(as.character(limit_index)," of ",as.character(limit)," DOCUMENTS")%in%trimws(lines[l])){
        norddeutschelandesbank$line[p]<-l
        norddeutschelandesbank$file[p]<-files
        p<-p+1
        limit_index<-limit_index+1
      }
    }  
    close(con)
  },silent = TRUE)
}
write.csv(norddeutschelandesbank,file = "~/Dropbox/thesis/12Project/norddeutschelandesbank.csv")

directory<-paste0(parent_directory,folders[1],"/")
f<-list.files(directory)
setwd(directory)
p<-1
for(files in 1:length(dir(directory))){
  try({
    con=file(f[files],open = "r")
    lines=readLines(con)
    file_length<-length(lines)
    limit<-as.integer(substr(trimws(lines[5]),37,nchar(trimws(lines[5]))-1))
    limit_index<-1
    for(l in 1:file_length){
      if(paste0(as.character(limit_index)," of ",as.character(limit)," DOCUMENTS")%in%trimws(lines[l])){
        dzbank$line[p]<-l
        dzbank$file[p]<-files
        p<-p+1
        limit_index<-limit_index+1
      }
    }  
    close(con)
  },silent = TRUE)
}
write.csv(dzbank,file = "~/Dropbox/thesis/12Project/dzbank.csv")

directory<-paste0(parent_directory,folders[1],"/")
setwd(directory)
f<-list.files(directory)
for(index in 16912:length(dzbank$date)){
  try({
    con=file(f[dzbank$file[index-1]],open ="r")
    lines=readLines(con)
    if(dzbank$file[index-1]==dzbank$file[index]){
      for(l in dzbank$line[index-1]:dzbank$line[index]){
        dzbank$news[index-1]<-paste0(dzbank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          dzbank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          dzbank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          dzbank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }
      }
    }
    else{
      for(l in dzbank$line[index-1]:length(lines)){
        dzbank$news[index-1]<-paste0(dzbank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          dzbank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          dzbank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          dzbank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }      
      }
    }
    close(con)  
  },silent = TRUE)
}
write.csv(dzbank,file = "~/Dropbox/thesis/12Project/dzbank.csv")
directory<-paste0(parent_directory,folders[2],"/")
setwd(directory)
f<-list.files(directory)
for(index in 2:num[2]){
  try({
    con=file(f[deutschebank$file[index-1]],open ="r")
    lines=readLines(con)
    if(deutschebank$file[index-1]==deutschebank$file[index]){
      for(l in deutschebank$line[index-1]:deutschebank$line[index]){
        deutschebank$news[index-1]<-paste0(deutschebank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          deutschebank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          deutschebank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          deutschebank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }
      }
    }
    else{
      for(l in deutschebank$line[index-1]:length(lines)){
        deutschebank$news[index-1]<-paste0(deutschebank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          deutschebank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          deutschebank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          deutschebank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }      
      }
    }
    close(con)  
  },silent = TRUE)
}
write.csv(deutschebank,file = "~/Dropbox/thesis/12Project/deutschebank.csv")
directory<-paste0(parent_directory,folders[3],"/")
setwd(directory)
f<-list.files(directory)
for(index in 2:num[3]){
  try({
    con=file(f[wgzbank$file[index-1]],open ="r")
    lines=readLines(con)
    if(wgzbank$file[index-1]==wgzbank$file[index]){
      for(l in wgzbank$line[index-1]:wgzbank$line[index]){
        wgzbank$news[index-1]<-paste0(wgzbank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          wgzbank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          wgzbank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          wgzbank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }
      }
    }
    else{
      for(l in wgzbank$line[index-1]:length(lines)){
        wgzbank$news[index-1]<-paste0(wgzbank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          wgzbank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          wgzbank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          wgzbank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }      
      }
    }
    close(con)  
  },silent = TRUE)
}
write.csv(wgzbank,file = "~/Dropbox/thesis/12Project/wgzbank.csv")
directory<-paste0(parent_directory,folders[4],"/")
setwd(directory)
f<-list.files(directory)
for(index in 12752:length(commerzbank$date)){
  try({
    con=file(f[commerzbank$file[index-1]],open ="r")
    lines=readLines(con)
    if(commerzbank$file[index-1]==commerzbank$file[index]){
      for(l in commerzbank$line[index-1]:commerzbank$line[index]){
        commerzbank$news[index-1]<-paste0(commerzbank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          commerzbank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          commerzbank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          commerzbank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }
      }
    }
    else{
      for(l in commerzbank$line[index-1]:length(lines)){
        commerzbank$news[index-1]<-paste0(commerzbank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          commerzbank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          commerzbank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          commerzbank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }      
      }
    }
    close(con)  
  },silent = TRUE)
}
write.csv(commerzbank,file = "~/Dropbox/thesis/12Project/commerzbank.csv")
directory<-paste0(parent_directory,folders[5],"/")
setwd(directory)
f<-list.files(directory)
for(index in 2:num[5]){
  try({
    con=file(f[bayernlb$file[index-1]],open ="r")
    lines=readLines(con)
    if(bayernlb$file[index-1]==bayernlb$file[index]){
      for(l in bayernlb$line[index-1]:bayernlb$line[index]){
        bayernlb$news[index-1]<-paste0(bayernlb$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          bayernlb$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          bayernlb$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          bayernlb$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }
      }
    }
    else{
      for(l in bayernlb$line[index-1]:length(lines)){
        bayernlb$news[index-1]<-paste0(bayernlb$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          bayernlb$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          bayernlb$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          bayernlb$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }      
      }
    }
    close(con)  
  },silent = TRUE)
}
write.csv(bayernlb,file = "~/Dropbox/thesis/12Project/bayernlb.csv")
directory<-paste0(parent_directory,folders[6],"/")
setwd(directory)
f<-list.files(directory)
for(index in 2:num[6]){
  try({
    con=file(f[hshnordbank$file[index-1]],open ="r")
    lines=readLines(con)
    if(hshnordbank$file[index-1]==hshnordbank$file[index]){
      for(l in hshnordbank$line[index-1]:hshnordbank$line[index]){
        hshnordbank$news[index-1]<-paste0(hshnordbank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          hshnordbank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          hshnordbank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          hshnordbank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }
      }
    }
    else{
      for(l in hshnordbank$line[index-1]:length(lines)){
        hshnordbank$news[index-1]<-paste0(hshnordbank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          hshnordbank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          hshnordbank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          hshnordbank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }      
      }
    }
    close(con)  
  },silent = TRUE)
}
write.csv(hshnordbank,file = "~/Dropbox/thesis/12Project/hshnordbank.csv")
directory<-paste0(parent_directory,folders[7],"/")
setwd(directory)
f<-list.files(directory)
for(index in 2:length(landesbankbaden$date)){
  try({
    con=file(f[landesbankbaden$file[index-1]],open ="r")
    lines=readLines(con)
    if(landesbankbaden$file[index-1]==landesbankbaden$file[index]){
      for(l in landesbankbaden$line[index-1]:landesbankbaden$line[index]){
        landesbankbaden$news[index-1]<-paste0(landesbankbaden$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          landesbankbaden$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          landesbankbaden$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          landesbankbaden$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }
      }
    }
    else{
      for(l in landesbankbaden$line[index-1]:length(lines)){
        landesbankbaden$news[index-1]<-paste0(landesbankbaden$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          landesbankbaden$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          landesbankbaden$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          landesbankbaden$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }      
      }
    }
    close(con)  
  },silent = TRUE)
}
write.csv(landesbankbaden,file = "~/Dropbox/thesis/12Project/landesbankbaden.csv")
directory<-paste0(parent_directory,folders[8],"/")
setwd(directory)
f<-list.files(directory)
for(index in 2:length(landesbankhessen$date)){
  try({
    con=file(f[landesbankhessen$file[index-1]],open ="r")
    lines=readLines(con)
    if(landesbankhessen$file[index-1]==landesbankhessen$file[index]){
      for(l in landesbankhessen$line[index-1]:landesbankhessen$line[index]){
        landesbankhessen$news[index-1]<-paste0(landesbankhessen$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          landesbankhessen$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          landesbankhessen$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          landesbankhessen$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }
      }
    }
    else{
      for(l in landesbankhessen$line[index-1]:length(lines)){
        landesbankhessen$news[index-1]<-paste0(landesbankhessen$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          landesbankhessen$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          landesbankhessen$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          landesbankhessen$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }      
      }
    }
    close(con)  
  },silent = TRUE)
}
write.csv(landesbankhessen,file = "~/Dropbox/thesis/12Project/landesbankhessen.csv")
directory<-paste0(parent_directory,folders[9],"/")
setwd(directory)
f<-list.files(directory)
for(index in 2:length(norddeutschelandesbank$date)){
  try({
    con=file(f[norddeutschelandesbank$file[index-1]],open ="r")
    lines=readLines(con)
    if(norddeutschelandesbank$file[index-1]==norddeutschelandesbank$file[index]){
      for(l in norddeutschelandesbank$line[index-1]:norddeutschelandesbank$line[index]){
        norddeutschelandesbank$news[index-1]<-paste0(norddeutschelandesbank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          norddeutschelandesbank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          norddeutschelandesbank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          norddeutschelandesbank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }
      }
    }
    else{
      for(l in norddeutschelandesbank$line[index-1]:length(lines)){
        norddeutschelandesbank$news[index-1]<-paste0(norddeutschelandesbank$news[index-1]," ",lines[l])
        if(grepl("LOAD-DATE:",lines[l])==TRUE){
          norddeutschelandesbank$load_date[index-1]<-substr(trimws(lines[l]),12,nchar(trimws(lines[l])))
        }
        if(grepl("LENGTH:",lines[l])==TRUE){
          norddeutschelandesbank$len[index-1]<-trimws(substr(trimws(lines[l]),8,nchar(trimws(lines[l]))-5))
        }
        if(grepl("PUBLICATION-TYPE:",lines[l])==TRUE){
          norddeutschelandesbank$pub_type[index-1]<-substr(trimws(lines[l]),18,nchar(trimws(lines[l])))
        }      
      }
    }
    close(con)  
  },silent = TRUE)
}
write.csv(norddeutschelandesbank,file = "~/Dropbox/thesis/12Project/norddeutschelandesbank.csv")

month_<-"January|February|March|April|May|June|July|August|September|October|November|December"
year_<-"2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017"
day_<-"Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday"
date_<-"1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31"

hshnordbank$len<-trimws(hshnordbank$len)
hshnordbank$news<-gsub("NA","",hshnordbank$news)
hshnordbank$news<-gsub("[[:digit:]]+ of [[:digit:]]+ DOCUMENTS","",hshnordbank$news)
hshnordbank$news<-trimws(hshnordbank$news)
hshnordbank$news<-gsub("LANGUAGE: ENGLISH","",hshnordbank$news)

deutschebank$len<-trimws(deutschebank$len)
deutschebank$news<-gsub("NA ","",deutschebank$news)
deutschebank$news<-gsub("[[:digit:]]+ of [[:digit:]]+ DOCUMENTS","",deutschebank$news)
deutschebank$news<-trimws(deutschebank$news)
deutschebank$news<-gsub("LANGUAGE: ENGLISH","",deutschebank$news)

wgzbank$len<-trimws(wgzbank$len)
wgzbank$news<-gsub("NA ","",wgzbank$news)
wgzbank$news<-gsub("[[:digit:]]+ of [[:digit:]]+ DOCUMENTS","",wgzbank$news)
wgzbank$news<-trimws(wgzbank$news)
wgzbank$news<-gsub("LANGUAGE: ENGLISH","",wgzbank$news)

bayernlb$len<-trimws(bayernlb$len)
bayernlb$news<-gsub("NA ","",bayernlb$news)
bayernlb$news<-gsub("[[:digit:]]+ of [[:digit:]]+ DOCUMENTS","",bayernlb$news)
bayernlb$news<-trimws(bayernlb$news)
bayernlb$news<-gsub("LANGUAGE: ENGLISH","",bayernlb$news)

landesbankbaden$len<-trimws(landesbankbaden$len)
landesbankbaden$news<-gsub("NA ","",landesbankbaden$news)
landesbankbaden$news<-gsub("[[:digit:]]+ of [[:digit:]]+ DOCUMENTS","",landesbankbaden$news)
landesbankbaden$news<-trimws(landesbankbaden$news)
landesbankbaden$news<-gsub("LANGUAGE: ENGLISH","",landesbankbaden$news)

landesbankhessen$len<-trimws(landesbankhessen$len)
landesbankhessen$news<-gsub("NA ","",landesbankhessen$news)
landesbankhessen$news<-gsub("[[:digit:]]+ of [[:digit:]]+ DOCUMENTS","",landesbankhessen$news)
landesbankhessen$news<-trimws(landesbankhessen$news)
landesbankhessen$news<-gsub("LANGUAGE: ENGLISH","",landesbankhessen$news)

norddeutschelandesbank$len<-trimws(norddeutschelandesbank$len)
norddeutschelandesbank$news<-gsub("NA ","",norddeutschelandesbank$news)
norddeutschelandesbank$news<-gsub("[[:digit:]]+ of [[:digit:]]+ DOCUMENTS","",norddeutschelandesbank$news)
norddeutschelandesbank$news<-trimws(norddeutschelandesbank$news)
norddeutschelandesbank$news<-gsub("LANGUAGE: ENGLISH","",norddeutschelandesbank$news)

dzbank$len<-trimws(dzbank$len)
dzbank$news<-gsub("NA ","",dzbank$news)
dzbank$news<-trimws(dzbank$news)
dzbank$news<-gsub("[[:digit:]]+ of [[:digit:]]+ DOCUMENTS","",dzbank$news)
dzbank$news<-gsub("LANGUAGE: ENGLISH","",dzbank$news)

commerzbank$len<-trimws(commerzbank$len)
commerzbank$news<-gsub("NA ","",commerzbank$news)
commerzbank$news<-trimws(commerzbank$news)
commerzbank$news<-gsub("[[:digit:]]+ of [[:digit:]]+ DOCUMENTS","",commerzbank$news)
commerzbank$news<-gsub("LANGUAGE: ENGLISH","",commerzbank$news)

bayernlb$news<-trimws(bayernlb$news)
bayernlb$news<-gsub("Copyright [[:digit:]]+","",bayernlb$news)
bayernlb$news<-gsub("All Rights Reserved","",bayernlb$news)
bayernlb$news<-gsub("[[:digit:]]+:[[:digit:]]+ AM|PM GMT","",bayernlb$news)

commerzbank$news<-trimws(commerzbank$news)
commerzbank$news<-gsub("Copyright [[:digit:]]+","",commerzbank$news)
commerzbank$news<-gsub("All Rights Reserved","",commerzbank$news)
commerzbank$news<-gsub("[[:digit:]]+:[[:digit:]]+ AM|PM GMT","",commerzbank$news)

deutschebank$news<-trimws(deutschebank$news)
deutschebank$news<-gsub("Copyright [[:digit:]]+","",deutschebank$news)
deutschebank$news<-gsub("All Rights Reserved","",deutschebank$news)
deutschebank$news<-gsub("[[:digit:]]+:[[:digit:]]+ AM|PM GMT","",deutschebank$news)

dzbank$news<-trimws(dzbank$news)
dzbank$news<-gsub("Copyright [[:digit:]]+","",dzbank$news)
dzbank$news<-gsub("All Rights Reserved","",dzbank$news)
dzbank$news<-gsub("[[:digit:]]+:[[:digit:]]+ AM|PM GMT","",dzbank$news)

hshnordbank$news<-trimws(hshnordbank$news)
hshnordbank$news<-gsub("Copyright [[:digit:]]+","",hshnordbank$news)
hshnordbank$news<-gsub("All Rights Reserved","",hshnordbank$news)
hshnordbank$news<-gsub("[[:digit:]]+:[[:digit:]]+ AM|PM GMT","",hshnordbank$news)

landesbankbaden$news<-trimws(landesbankbaden$news)
landesbankbaden$news<-gsub("Copyright [[:digit:]]+","",landesbankbaden$news)
landesbankbaden$news<-gsub("All Rights Reserved","",landesbankbaden$news)
landesbankbaden$news<-gsub("[[:digit:]]+:[[:digit:]]+ AM|PM GMT","",landesbankbaden$news)

landesbankhessen$news<-trimws(landesbankhessen$news)
landesbankhessen$news<-gsub("Copyright [[:digit:]]+","",landesbankhessen$news)
landesbankhessen$news<-gsub("All Rights Reserved","",landesbankhessen$news)
landesbankhessen$news<-gsub("[[:digit:]]+:[[:digit:]]+ AM|PM GMT","",landesbankhessen$news)

norddeutschelandesbank$news<-trimws(norddeutschelandesbank$news)
norddeutschelandesbank$news<-gsub("Copyright [[:digit:]]+","",norddeutschelandesbank$news)
norddeutschelandesbank$news<-gsub("All Rights Reserved","",norddeutschelandesbank$news)
norddeutschelandesbank$news<-gsub("[[:digit:]]+:[[:digit:]]+ AM|PM GMT","",norddeutschelandesbank$news)

wgzbank$news<-trimws(wgzbank$news)
wgzbank$news<-gsub("Copyright [[:digit:]]+","",wgzbank$news)
wgzbank$news<-gsub("All Rights Reserved","",wgzbank$news)
wgzbank$news<-gsub("[[:digit:]]+:[[:digit:]]+ AM|PM GMT","",wgzbank$news)

bayernlb$agency<-bayernlb$sentiment<-commerzbank$agency<-commerzbank$sentiment<-NA
deutschebank$agency<-deutschebank$sentiment<-dzbank$agency<-dzbank$sentiment<-NA
hshnordbank$agency<-hshnordbank$sentiment<-landesbankbaden$agency<-landesbankbaden$sentiment<-NA
landesbankhessen$agency<-landesbankhessen$sentiment<-norddeutschelandesbank$agency<-NA
norddeutschelandesbank$sentiment<-wgzbank$agency<-wgzbank$sentiment<-NA

########################################################
# EVERYTHING COMPLETED TILL HERE
########################################################

for(i in 1:length(wgzbank$news)){
    wgzbank$news[i]<-gsub(paste0("PUBLICATION-TYPE:",wgzbank$pub_type[i]),"",wgzbank$news[i])
    wgzbank$news[i]<-gsub(paste0("LOAD-DATE: ",wgzbank$load_date[i]),"",wgzbank$news[i])
    wgzbank$news[i]<-gsub(paste0("LENGTH: ",wgzbank$len[i]," words"),"",wgzbank$news[i])
    wgzbank$agency[i]<-substr(wgzbank$news[i],1,gregexpr(pattern = month_,wgzbank$news[i])[[1]][1]-1)  
}
wgzbank$agency<-trimws(wgzbank$agency)
wgzbank$news<-trimws(wgzbank$news)
for(i in 1:length(wgzbank$news)){
  string<-gsub(wgzbank$agency[i],"",wgzbank$news[i])
  string<-trimws(string)
  qwe<-strsplit(string," ")[[1]]
  wgzbank$date[i]<-paste(qwe[1],qwe[2],qwe[3],qwe[4],sep = " ")
}

for(i in 1:length(deutschebank$news)){
  deutschebank$news[i]<-gsub(paste0("PUBLICATION-TYPE:",deutschebank$pub_type[i]),"",deutschebank$news[i])
  deutschebank$news[i]<-gsub(paste0("LOAD-DATE: ",deutschebank$load_date[i]),"",deutschebank$news[i])
  deutschebank$news[i]<-gsub(paste0("LENGTH: ",deutschebank$len[i]," words"),"",deutschebank$news[i])
  deutschebank$agency[i]<-substr(deutschebank$news[i],1,gregexpr(pattern = month_,deutschebank$news[i])[[1]][1]-1)
}
deutschebank$agency<-trimws(deutschebank$agency)
deutschebank$news<-trimws(deutschebank$news)
for(i in 1:length(deutschebank$news)){
  try({
    string<-gsub(deutschebank$agency[i],"",deutschebank$news[i])
    string<-trimws(string)
    qwe<-strsplit(string," ")[[1]]
    deutschebank$date[i]<-paste(qwe[1],qwe[2],qwe[3],qwe[4],sep = " ")
  },silent = TRUE)
}

for(i in 1:length(bayernlb$news)){
  bayernlb$news[i]<-gsub(paste0("PUBLICATION-TYPE:",bayernlb$pub_type[i]),"",bayernlb$news[i])
  bayernlb$news[i]<-gsub(paste0("LOAD-DATE: ",bayernlb$load_date[i]),"",bayernlb$news[i])
  bayernlb$news[i]<-gsub(paste0("LENGTH: ",bayernlb$len[i]," words"),"",bayernlb$news[i])
  bayernlb$agency[i]<-substr(bayernlb$news[i],1,gregexpr(pattern = month_,bayernlb$news[i])[[1]][1]-1)  
}
bayernlb$agency<-trimws(bayernlb$agency)
bayernlb$news<-trimws(bayernlb$news)
for(i in 1:length(bayernlb$news)){
  try({
    string<-gsub(bayernlb$agency[i],"",bayernlb$news[i])
    string<-trimws(string)
    qwe<-strsplit(string," ")[[1]]
    bayernlb$date[i]<-paste(qwe[1],qwe[2],qwe[3],qwe[4],sep = " ")
  },silent = TRUE)
}

for(i in 1:length(commerzbank$news)){
  commerzbank$news[i]<-gsub(paste0("PUBLICATION-TYPE:",commerzbank$pub_type[i]),"",commerzbank$news[i])
  commerzbank$news[i]<-gsub(paste0("LOAD-DATE: ",commerzbank$load_date[i]),"",commerzbank$news[i])
  commerzbank$news[i]<-gsub(paste0("LENGTH: ",commerzbank$len[i]," words"),"",commerzbank$news[i])
  commerzbank$agency[i]<-substr(commerzbank$news[i],1,gregexpr(pattern = month_,commerzbank$news[i])[[1]][1]-1)  
}
commerzbank$agency<-trimws(commerzbank$agency)
commerzbank$news<-trimws(commerzbank$news)
for(i in 1:length(commerzbank$news)){
  try({
    string<-gsub(commerzbank$agency[i],"",commerzbank$news[i])
    string<-trimws(string)
    qwe<-strsplit(string," ")[[1]]
    commerzbank$date[i]<-paste(qwe[1],qwe[2],qwe[3],qwe[4],sep = " ")
  },silent = TRUE)
}

for(i in 1:length(dzbank$news)){
  dzbank$news[i]<-gsub(paste0("PUBLICATION-TYPE:",dzbank$pub_type[i]),"",dzbank$news[i])
  dzbank$news[i]<-gsub(paste0("LOAD-DATE: ",dzbank$load_date[i]),"",dzbank$news[i])
  dzbank$news[i]<-gsub(paste0("LENGTH: ",dzbank$len[i]," words"),"",dzbank$news[i])
  dzbank$agency[i]<-substr(dzbank$news[i],1,gregexpr(pattern = month_,dzbank$news[i])[[1]][1]-1)  
}
dzbank$agency<-trimws(dzbank$agency)
dzbank$news<-trimws(dzbank$news)
for(i in 1:length(dzbank$news)){
  try({
    string<-gsub(dzbank$agency[i],"",dzbank$news[i])
    string<-trimws(string)
    qwe<-strsplit(string," ")[[1]]
    dzbank$date[i]<-paste(qwe[1],qwe[2],qwe[3],qwe[4],sep = " ")
  },silent = TRUE)
}

for(i in 1:length(hshnordbank$news)){
  hshnordbank$news[i]<-gsub(paste0("PUBLICATION-TYPE:",hshnordbank$pub_type[i]),"",hshnordbank$news[i])
  hshnordbank$news[i]<-gsub(paste0("LOAD-DATE: ",hshnordbank$load_date[i]),"",hshnordbank$news[i])
  hshnordbank$news[i]<-gsub(paste0("LENGTH: ",hshnordbank$len[i]," words"),"",hshnordbank$news[i])
  hshnordbank$agency[i]<-substr(hshnordbank$news[i],1,gregexpr(pattern = month_,hshnordbank$news[i])[[1]][1]-1)  
}
hshnordbank$agency<-trimws(hshnordbank$agency)
hshnordbank$news<-trimws(hshnordbank$news)
for(i in 1:length(hshnordbank$news)){
  try({
    string<-gsub(hshnordbank$agency[i],"",hshnordbank$news[i])
    string<-trimws(string)
    qwe<-strsplit(string," ")[[1]]
    hshnordbank$date[i]<-paste(qwe[1],qwe[2],qwe[3],qwe[4],sep = " ")
  },silent = TRUE)
}

for(i in 1:length(landesbankbaden$news)){
  landesbankbaden$news[i]<-gsub(paste0("PUBLICATION-TYPE:",landesbankbaden$pub_type[i]),"",landesbankbaden$news[i])
  landesbankbaden$news[i]<-gsub(paste0("LOAD-DATE: ",landesbankbaden$load_date[i]),"",landesbankbaden$news[i])
  landesbankbaden$news[i]<-gsub(paste0("LENGTH: ",landesbankbaden$len[i]," words"),"",landesbankbaden$news[i])
  landesbankbaden$agency[i]<-substr(landesbankbaden$news[i],1,gregexpr(pattern = month_,landesbankbaden$news[i])[[1]][1]-1)  
}
landesbankbaden$agency<-trimws(landesbankbaden$agency)
landesbankbaden$news<-trimws(landesbankbaden$news)
for(i in 1:length(landesbankbaden$news)){
  string<-gsub(landesbankbaden$agency[i],"",landesbankbaden$news[i])
  string<-trimws(string)
  qwe<-strsplit(string," ")[[1]]
  landesbankbaden$date[i]<-paste(qwe[1],qwe[2],qwe[3],qwe[4],sep = " ")
}

for(i in 1:length(landesbankhessen$news)){
  landesbankhessen$news[i]<-gsub(paste0("PUBLICATION-TYPE:",landesbankhessen$pub_type[i]),"",landesbankhessen$news[i])
  landesbankhessen$news[i]<-gsub(paste0("LOAD-DATE: ",landesbankhessen$load_date[i]),"",landesbankhessen$news[i])
  landesbankhessen$news[i]<-gsub(paste0("LENGTH: ",landesbankhessen$len[i]," words"),"",landesbankhessen$news[i])
  landesbankhessen$agency[i]<-substr(landesbankhessen$news[i],1,gregexpr(pattern = month_,landesbankhessen$news[i])[[1]][1]-1)  
}
landesbankhessen$agency<-trimws(landesbankhessen$agency)
landesbankhessen$news<-trimws(landesbankhessen$news)
for(i in 1:length(landesbankhessen$news)){
  string<-gsub(landesbankhessen$agency[i],"",landesbankhessen$news[i])
  string<-trimws(string)
  qwe<-strsplit(string," ")[[1]]
  landesbankhessen$date[i]<-paste(qwe[1],qwe[2],qwe[3],qwe[4],sep = " ")
}

for(i in 1:length(norddeutschelandesbank$news)){
  norddeutschelandesbank$news[i]<-gsub(paste0("PUBLICATION-TYPE:",norddeutschelandesbank$pub_type[i]),"",norddeutschelandesbank$news[i])
  norddeutschelandesbank$news[i]<-gsub(paste0("LOAD-DATE: ",norddeutschelandesbank$load_date[i]),"",norddeutschelandesbank$news[i])
  norddeutschelandesbank$news[i]<-gsub(paste0("LENGTH: ",norddeutschelandesbank$len[i]," words"),"",norddeutschelandesbank$news[i])
  norddeutschelandesbank$agency[i]<-substr(norddeutschelandesbank$news[i],1,gregexpr(pattern = month_,norddeutschelandesbank$news[i])[[1]][1]-1)  
}
norddeutschelandesbank$agency<-trimws(norddeutschelandesbank$agency)
norddeutschelandesbank$news<-trimws(norddeutschelandesbank$news)
for(i in 1:length(norddeutschelandesbank$news)){
  string<-gsub(norddeutschelandesbank$agency[i],"",norddeutschelandesbank$news[i])
  string<-trimws(string)
  qwe<-strsplit(string," ")[[1]]
  norddeutschelandesbank$date[i]<-paste(qwe[1],qwe[2],qwe[3],qwe[4],sep = " ")
}


write.csv(dzbank,file = "~/Desktop/dzbank2.csv")
write.csv(deutschebank,file = "~/Desktop/deutschebank2.csv")
write.csv(wgzbank,file = "~/Desktop/wgzbank2.csv")
write.csv(commerzbank,file = "~/Desktop/commerzbank2.csv")
write.csv(bayernlb,file = "~/Desktop/bayernlb2.csv")
write.csv(hshnordbank,file = "~/Desktop/hshnordbank2.csv")
write.csv(landesbankbaden,file = "~/Desktop/landesbankbaden2.csv")
write.csv(landesbankhessen,file = "~/Desktop/landesbankhessen2.csv")
write.csv(norddeutschelandesbank,file = "~/Desktop/norddeutschelandesbank2.csv")

bayernlb <- read_csv("~/Dropbox/thesis/12Project/bayernlb2.csv", col_types = cols(X1 = col_skip()))
deutschebank <- read_csv("~/Dropbox/thesis/12Project/deutschebank2.csv", col_types = cols(X1 = col_skip()))
hshnordbank <- read_csv("~/Dropbox/thesis/12Project/hshnordbank2.csv", col_types = cols(X1 = col_skip()))
landesbankbaden <- read_csv("~/Dropbox/thesis/12Project/landesbankbaden2.csv", col_types = cols(X1 = col_skip()))
landesbankhessen <- read_csv("~/Dropbox/thesis/12Project/landesbankhessen2.csv", col_types = cols(X1 = col_skip()))
norddeutschelandesbank <- read_csv("~/Dropbox/thesis/12Project/norddeutschelandesbank2.csv", col_types = cols(X1 = col_skip()))
commerzbank <- read_csv("~/Dropbox/thesis/12Project/commerzbank2.csv", col_types = cols(X1 = col_skip()))
wgzbank <- read_csv("~/Dropbox/thesis/12Project/wgzbank2.csv", col_types = cols(X1 = col_skip()))
dzbank <- read_csv("~/Dropbox/thesis/12Project/dzbank2.csv", col_types = cols(X1 = col_skip()))

###################################################
#   MAIN THING STARTS NOW
#   Fear Index
#   Sentiment Analysis
install.packages("tm",lib = "~/Library/R/3.4/library/",repos = "http://cran.rstudio.com/")
install.packages("data.table",lib = "~/Library/R/3.4/library/",repos = "http://cran.rstudio.com/")
install.packages("plyr",lib = "~/Library/R/3.4/library/",repos = "http://cran.rstudio.com/")
install.packages("stringr",lib = "~/Library/R/3.4/library/",repos = "http://cran.rstudio.com/")
install.packages("SnowballC",lib = "~/Library/R/3.4/library/",repos = "http://cran.rstudio.com/")
###################################################

library(tm)
require(tm)
library(SnowballC)
library(NLP)
library(data.table)
require(plyr)
require(stringr)

hu.liu.pos = readLines('https://www.dropbox.com/sh/3xctszdxx4n00xq/AAA_Go_Y3kJxQACFaVBem__ea/positive-words.txt?dl=1')
hu.liu.neg = readLines('https://www.dropbox.com/sh/3xctszdxx4n00xq/AABTGWHitlRZcddq1pPXOSqca/negative-words.txt?dl=1')

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myStopwords_de  <- c(setdiff(stopwords('german'), c("r", "big")), "ver","f?r", "ver","ungen", "ung","ae","?","oe","?","ue","e?","?","eue","a?n","auen","e?n","euen", "ens", "en", "fiir","??","????z")
myStopwords<- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")
replaceWord <- function(corpus, oldwords, newword) {
  for (i in 1:length(oldwords) )
  {
    tm_map(corpus, content_transformer(gsub),pattern=oldwords[i], replacement=newword) }
}
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,function(x) { grep(as.character(x), pattern=paste0("nn<",word)) })
  sum(unlist(results))
}
replaceWord2 <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),pattern=oldword, replacement=newword) 
}
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores);
  return(scores.df);
}

Textprocess <- function(mytext){
  myCorpus_eng <- Corpus(VectorSource(mytext),readerControl = list(language = "en"))
  myCorpus_eng <- tm_map(myCorpus_eng, content_transformer(tolower))
  myCorpus_eng <- tm_map(myCorpus_eng, content_transformer(removeURL))
  myCorpus_eng <- tm_map(myCorpus_eng, content_transformer(removeNumPunct))
  myCorpus_eng <- tm_map(myCorpus_eng, removeWords, stopwords("english"))
  myCorpus_eng <- tm_map(myCorpus_eng, stripWhitespace)
  myCorpus_eng <- tm_map(myCorpus_eng, stemDocument)
  tdm_eng <- TermDocumentMatrix(myCorpus_eng,  control = list(wordLengths = c(1, Inf)))
  idx_eng <- which(dimnames(tdm_eng)$Terms %in% keywords_main) 
  results_eng<-data.frame(t(as.matrix(tdm_eng[idx_eng,])))
}

keywords_main<-c("dzbank","deutschebank","wgzbank","commerzbank","bayernlb","hshnordbank"
                 ,"landesbank baden","landesbank hessen","norddeutschelandesbank")

index_df<-Textprocess(bayernlb$news) 
bayernlb<-cbind(bayernlb,index_df)
bayernlb$sentiment<-score.sentiment(bayernlb$news,hu.liu.pos,hu.liu.neg)

index_df<-Textprocess(commerzbank$news) 
commerzbank<-cbind(commerzbank,index_df)
commerzbank$sentiment<-score.sentiment(commerzbank$news,hu.liu.pos,hu.liu.neg)

index_df<-Textprocess(deutschebank$news) 
deutschebank<-cbind(deutschebank,index_df)
deutschebank$sentiment<-score.sentiment(deutschebank$news,hu.liu.pos,hu.liu.neg)

index_df<-Textprocess(dzbank$news) 
dzbank<-cbind(dzbank,index_df)
dzbank$sentiment<-score.sentiment(dzbank$news,hu.liu.pos,hu.liu.neg)

index_df<-Textprocess(hshnordbank$news) 
hshnordbank<-cbind(hshnordbank,index_df)
hshnordbank$sentiment<-score.sentiment(hshnordbank$news,hu.liu.pos,hu.liu.neg)

index_df<-Textprocess(landesbankbaden$news) 
landesbankbaden<-cbind(landesbankbaden,index_df)
landesbankbaden$sentiment<-score.sentiment(landesbankbaden$news,hu.liu.pos,hu.liu.neg)

index_df<-Textprocess(landesbankhessen$news) 
landesbankhessen<-cbind(landesbankhessen,index_df)
landesbankhessen$sentiment<-score.sentiment(landesbankhessen$news,hu.liu.pos,hu.liu.neg)

index_df<-Textprocess(norddeutschelandesbank$news) 
norddeutschelandesbank<-cbind(norddeutschelandesbank,index_df)
norddeutschelandesbank$sentiment<-score.sentiment(norddeutschelandesbank$news,hu.liu.pos,hu.liu.neg)

index_df<-Textprocess(wgzbank$news) 
wgzbank<-cbind(wgzbank,index_df)
wgzbank$sentiment<-score.sentiment(wgzbank$news,hu.liu.pos,hu.liu.neg)

write.csv(dzbank,file = "~/Dropbox/thesis/12Project/dzbank2.csv")
write.csv(deutschebank,file = "~/Dropbox/thesis/12Project/deutschebank2.csv")
write.csv(wgzbank,file = "~/Dropbox/thesis/12Project/wgzbank2.csv")
write.csv(commerzbank,file = "~/Dropbox/thesis/12Project/commerzbank2.csv")
write.csv(bayernlb,file = "~/Dropbox/thesis/12Project/bayernlb2.csv")
write.csv(hshnordbank,file = "~/Dropbox/thesis/12Project/hshnordbank2.csv")
write.csv(landesbankbaden,file = "~/Dropbox/thesis/12Project/landesbankbaden2.csv")
write.csv(landesbankhessen,file = "~/Dropbox/thesis/12Project/landesbankhessen2.csv")
write.csv(norddeutschelandesbank,file = "~/Dropbox/thesis/12Project/norddeutschelandesbank2.csv")

deutschebank$date<-gsub(day_,"",deutschebank$date)
bayernlb$date<-gsub(day_,"",bayernlb$date)
commerzbank$date<-gsub(day_,"",commerzbank$date)
dzbank$date<-gsub(day_,"",dzbank$date)
hshnordbank$date<-gsub(day_,"",hshnordbank$date)
landesbankbaden$date<-gsub(day_,"",landesbankbaden$date)
landesbankhessen$date<-gsub(day_,"",landesbankhessen$date)
norddeutschelandesbank$date<-gsub(day_,"",norddeutschelandesbank$date)
wgzbank$date<-gsub(day_,"",wgzbank$date)

deutschebank$news<-gsub(day_,"",deutschebank$news)
bayernlb$news<-gsub(day_,"",bayernlb$news)
commerzbank$news<-gsub(day_,"",commerzbank$news)
dzbank$news<-gsub(day_,"",dzbank$news)
hshnordbank$news<-gsub(day_,"",hshnordbank$news)
landesbankbaden$news<-gsub(day_,"",landesbankbaden$news)
landesbankhessen$news<-gsub(day_,"",landesbankhessen$news)
norddeutschelandesbank$news<-gsub(day_,"",norddeutschelandesbank$news)
wgzbank$news<-gsub(day_,"",wgzbank$news)


for(i in 1:length(deutschebank$date)){
  try({
    deutschebank$date[i]<-gsub(deutschebank$agency[i],"",deutschebank$date[i])
    deutschebank$news[i]<-gsub(deutschebank$agency[i],"",deutschebank$news[i]) 
    deutschebank$news[i]<-gsub(deutschebank$date[i],"",deutschebank$news[i])  
  },silent = TRUE)
}
for(i in 1:length(bayernlb$date)){
  try({
    bayernlb$date[i]<-gsub(bayernlb$agency[i],"",bayernlb$date[i])  
    bayernlb$news[i]<-gsub(bayernlb$agency[i],"",bayernlb$news[i])  
    bayernlb$news[i]<-gsub(bayernlb$date[i],"",bayernlb$news[i])    
  },silent=TRUE)
}
for(i in 1:length(commerzbank$date)){
  try({
    commerzbank$date[i]<-gsub(commerzbank$agency[i],"",commerzbank$date[i])  
    commerzbank$news[i]<-gsub(commerzbank$agency[i],"",commerzbank$news[i])  
    commerzbank$news[i]<-gsub(commerzbank$date[i],"",commerzbank$news[i])  
  },silent=TRUE)
}
for(i in 1:length(dzbank$date)){
  try({
    dzbank$date[i]<-gsub(dzbank$agency[i],"",dzbank$date[i])  
    dzbank$news[i]<-gsub(dzbank$agency[i],"",dzbank$news[i])  
    dzbank$news[i]<-gsub(dzbank$date[i],"",dzbank$news[i])  
  },silent=TRUE)
}
for(i in 1:length(hshnordbank$date)){
  try({
    hshnordbank$date[i]<-gsub(hshnordbank$agency[i],"",hshnordbank$date[i])
    hshnordbank$news[i]<-gsub(hshnordbank$agency[i],"",hshnordbank$news[i])
    hshnordbank$news[i]<-gsub(hshnordbank$date[i],"",hshnordbank$news[i])  
  },silent = TRUE)
}
for(i in 1:length(landesbankbaden$date)){
  try({
    landesbankbaden$date[i]<-gsub(landesbankbaden$agency[i],"",landesbankbaden$date[i])  
    landesbankbaden$news[i]<-gsub(landesbankbaden$agency[i],"",landesbankbaden$news[i])  
    landesbankbaden$news[i]<-gsub(landesbankbaden$date[i],"",landesbankbaden$news[i])  
  },silent = TRUE)
}
for(i in 1:length(landesbankhessen$date)){
  try({
    landesbankhessen$date[i]<-gsub(landesbankhessen$agency[i],"",landesbankhessen$date[i])  
    landesbankhessen$news[i]<-gsub(landesbankhessen$agency[i],"",landesbankhessen$news[i])  
    landesbankhessen$news[i]<-gsub(landesbankhessen$date[i],"",landesbankhessen$news[i])  
  },silent = TRUE)
}
for(i in 1:length(norddeutschelandesbank$date)){
  try({
    norddeutschelandesbank$date[i]<-gsub(norddeutschelandesbank$agency[i],"",norddeutschelandesbank$date[i])    
    norddeutschelandesbank$news[i]<-gsub(norddeutschelandesbank$agency[i],"",norddeutschelandesbank$news[i])    
    norddeutschelandesbank$news[i]<-gsub(norddeutschelandesbank$date[i],"",norddeutschelandesbank$news[i])    
  },silent = TRUE)
}
for(i in 1:length(wgzbank$date)){
  try({
    wgzbank$date[i]<-gsub(wgzbank$agency[i],"",wgzbank$date[i])  
    wgzbank$news[i]<-gsub(wgzbank$agency[i],"",wgzbank$news[i])  
    wgzbank$news[i]<-gsub(wgzbank$date[i],"",wgzbank$news[i])  
  },silent = TRUE)
}

data_cleaner<-"APA-Economic News Service (APA-ENS)|The Independent (London)|The Telegraph (LONDON)|
Herald Sun (Australia)|Mail (Queensland, Australia)|Telegraph (Australia)|Times (South Africa)|
3327|4688|\\||APA-Economic News Service (APA-ENS)|Asian News International (ANI)|Augusta State University|
Austin American-Statesman (Texas)|AWP OTS (Original text|Azeri-Press news agency (APA)|
Banking and Stock Exchange,|Business World (Digest)|Canberra Times (Australia)|
Cape Argus (South Africa)| China Economic Review (CER)|Coventry Evening Telegraph (England)|
Daily Mail (London)|DAILY MAIL (London)|Daily News (South Africa)|Daily Post (North Wales)|
[[:digit:]]+:[[:digit:]]+|Economic News (Information Agency|Edmonton Journal (Alberta)|
EMBIN (Emerging Markets Business|EQS Newsfeed (English)|Evening Standard (UK)|
Evening Times (Glasgow)|FD (Fair Disclosure) Wire|Financial Mail (South Africa)|
Geelong Advertiser (Australia)|Ghana News Agency (GNA)|Global English (Middle East|
GlobalAdSource (English)|Globes [online] - Israel's|Herald Sun (Australia)|
Hobart Mercury (Australia)|Independent on (London)|Institutional Investor (International Edition)|
SECTION:|Khaleej Times (United Arab|Legal Week (Online)|London Stock Exchange Aggregated|
MAIL ON SUNDAY (London)|MEEnglish (Middle East and|Metro (UK)|MX (Australia)|
National Post's Financial Post|NetIndia123.com|New Straits Times (Malaysia)|Northern Territory News (Australia)|
Original text service (ots)|OTS Deutschland (Englisch)|Plus Company Updates(PCU)|
Pretoria News (South Africa)|RTT News (United States)|Russian Financial Control Monitor(RFCM)|
Saudi Press Agency (SPA)|Shanghai Daily (Benchmark)|Sharenet|Sify|Sydney Morning Herald (Australia)|
The Independent (South|The Star-Times (Auckland,|The Telegraph (London)|The Telegraph (LONDON)|
The Times (London)|The Advertiser (Australia)|The Age (Melbourne, Australia)|The Australian (Australia)|
The Bangkok Post (Thailand)|The Calgary Herald (Alberta)|The Courier Mail (Australia)|The Daily Gleaner (New|
The Daily Telegraph (London)|The Daily Telegraph (LONDON)|THE DAILY TELEGRAPH(LONDON)|
The Evening Standard (London)|The Gazette (Montreal)|The Globe and Mail|The Gold Coast Bulletin|
The Guardian(London)|The Hawk Eye (Burlington,|The Herald (Glasgow)|The Independent (London)|
The Independent (United Kingdom)|The Journal (Newcastle, UK)|THE JOURNAL (Newcastle, UK)|
The Leader-Post (Regina, Saskatchewan)|The Mercury (South Africa)|The Nation (Thailand)|
The New York Times|The Observer (London)|The Russian Business Monitor|The Russian Oil and|
The Star (South Africa)|The Straits Times (Singapore)|The Sun Herald (Sydney,|The Telegraph-Journal (New Brunswick)|
The Timaru Herald (New|The West Australian (Perth)|Times of India (Electronic|TODAY (Singapore)|
Townsville Sun (Australia)|Waikato Times (Hamilton, New|Western Morning News (Plymouth,|
Windsor Star (Ontario)"
try({
  bayernlb$date<-gsub(data_cleaner,"",bayernlb$date)
},silent = TRUE)

try({
  bayernlb$date<-as.Date(bayernlb$date,"%B %d, %Y")
  commerzbank$date<-as.Date(commerzbank$date,"%B %d, %Y")
  deutschebank$date<-as.Date(deutschebank$date,"%B %d, %Y")
  dzbank$date<-as.Date(dzbank$date,"%B %d, %Y")
  hshnordbank$date<-as.Date(hshnordbank$date,"%B %d, %Y")
  landesbankbaden$date<-as.Date(landesbankbaden$date,"%B %d, %Y")
  landesbankhessen$date<-as.Date(landesbankhessen$date,"%B %d, %Y")
  norddeutschelandesbank$date<-as.Date(norddeutschelandesbank$date,"%B %d, %Y")
  wgzbank$date<-as.Date(wgzbank$date,"%B %d, %Y")
},silent=TRUE)
try({  
  bayernlb$load_date<-as.Date(bayernlb$load_date,"%B %d, %Y")
  commerzbank$load_date<-as.Date(commerzbank$load_date,"%B %d, %Y")
  deutschebank$load_date<-as.Date(deutschebank$load_date,"%B %d, %Y")
  dzbank$load_date<-as.Date(dzbank$load_date,"%B %d, %Y")
  hshnordbank$load_date<-as.Date(hshnordbank$load_date,"%B %d, %Y")
  landesbankbaden$load_date<-as.Date(landesbankbaden$load_date,"%B %d, %Y")
  landesbankhessen$load_date<-as.Date(landesbankhessen$load_date,"%B %d, %Y")
  norddeutschelandesbank$load_date<-as.Date(norddeutschelandesbank$load_date,"%B %d, %Y")
  wgzbank$load_date<-as.Date(wgzbank$load_date,"%B %d, %Y")
},silent = TRUE)

#####################################################################
# PLOTTING DATA
#####################################################################

library(ggplot2)
install.packages("caret")
library(caret)

relevant<-which(!is.na(bayernlb$date))
temp.data<-data.frame(date=1:length(relevant),score=length(relevant))
for(i in 1:length(relevant)){
  temp.data$date[i]<-as.Date(bayernlb$date[[relevant[i]]])
  temp.data$score[i]<-bayernlb$sentiment[[1]][relevant[i]]
}
ggplot(temp.data)
plot(x=temp.data$date,y=temp.data$score,type = "l",xlab = "Date",ylab = "Sentiment")

relevant<-which(!is.na(commerzbank$date))
temp.data<-data.frame(date=1:length(relevant),score=length(relevant))
for(i in 1:length(relevant)){
  temp.data$date[i]<-as.Date(commerzbank$date[[relevant[i]]])
  temp.data$score[i]<-commerzbank$sentiment[[1]][relevant[i]]
}
plot(x=temp.data$date,y=temp.data$score,type = "l",xlab = "Date",ylab = "Sentiment")

relevant<-which(!is.na(deutschebank$date))
temp.data<-data.frame(date=1:length(relevant),score=length(relevant))
for(i in 1:length(relevant)){
  temp.data$date[i]<-as.Date(deutschebank$date[[relevant[i]]])
  temp.data$score[i]<-deutschebank$sentiment[[1]][relevant[i]]
}
plot(x=temp.data$date,y=temp.data$score,type = "l",xlab = "Date",ylab = "Sentiment")

relevant<-which(!is.na(dzbank$date))
temp.data<-data.frame(date=1:length(relevant),score=length(relevant))
for(i in 1:length(relevant)){
  temp.data$date[i]<-as.Date(dzbank$date[[relevant[i]]])
  temp.data$score[i]<-dzbank$sentiment[[1]][relevant[i]]
}
plot(x=temp.data$date,y=temp.data$score,type = "l",xlab = "Date",ylab = "Sentiment")

relevant<-which(!is.na(hshnordbank$date))
temp.data<-data.frame(date=1:length(relevant),score=length(relevant))
for(i in 1:length(relevant)){
  temp.data$date[i]<-as.Date(hshnordbank$date[[relevant[i]]])
  temp.data$score[i]<-hshnordbank$sentiment[[1]][relevant[i]]
}
plot(x=temp.data$date,y=temp.data$score,type = "l",xlab = "Date",ylab = "Sentiment")

relevant<-which(!is.na(landesbankbaden$date))
temp.data<-data.frame(date=1:length(relevant),score=length(relevant))
for(i in 1:length(relevant)){
  temp.data$date[i]<-as.Date(landesbankbaden$date[[relevant[i]]])
  temp.data$score[i]<-landesbankbaden$sentiment[[1]][relevant[i]]
}
plot(x=temp.data$date,y=temp.data$score,type = "l",xlab = "Date",ylab = "Sentiment")

relevant<-which(!is.na(landesbankhessen$date))
temp.data<-data.frame(date=1:length(relevant),score=length(relevant))
for(i in 1:length(relevant)){
  temp.data$date[i]<-as.Date(landesbankhessen$date[[relevant[i]]])
  temp.data$score[i]<-landesbankhessen$sentiment[[1]][relevant[i]]
}
plot(x=temp.data$date,y=temp.data$score,type = "l",xlab = "Date",ylab = "Sentiment")

relevant<-which(!is.na(norddeutschelandesbank$date))
temp.data<-data.frame(date=1:length(relevant),score=length(relevant))
for(i in 1:length(relevant)){
  temp.data$date[i]<-as.Date(norddeutschelandesbank$date[[relevant[i]]])
  temp.data$score[i]<-norddeutschelandesbank$sentiment[[1]][relevant[i]]
}
plot(x=temp.data$date,y=temp.data$score,type = "l",xlab = "Date",ylab = "Sentiment")

relevant<-which(!is.na(wgzbank$date))
temp.data<-data.frame(date=1:length(relevant),score=length(relevant))
for(i in 1:length(relevant)){
  temp.data$date[i]<-as.Date(wgzbank$date[[relevant[i]]])
  temp.data$score[i]<-wgzbank$sentiment[[1]][relevant[i]]
}
plot(x=temp.data$date,y=temp.data$score,type = "l",xlab = "Date",ylab = "Sentiment")

bayernlb<-bayernlb[order(bayernlb$date),]

Daily_impact_median<-aggregate(bayernlb$deutschebank, by= list(Category=bayernlb$date), FUN=count)

myCorpus <- Corpus(VectorSource(deutschebank$news),readerControl = list(language = "en"))

dtm <- DocumentTermMatrix(myCorpus)
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
m<-as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
freq <- colSums(as.matrix(dtm))
length(freq)

dtmr <-DocumentTermMatrix(myCorpus, control=list(wordLengths=c(1, 10),   bounds = list(global = c(1,10))))
findAssocs(dtmr,"bayernlb",0.8)
