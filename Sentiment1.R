library(tm)
require(tm)
library(NLP)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myStopwords_de  <- c(setdiff(stopwords('german'), c("r", "big")), "ver","für", "ver","ungen", "ung","ae","ä","oe","ö","ue","eü","ü","eue","aün","auen","eün","euen", "ens", "en", "fiir","ãÿ","â???z")
myStopwords<- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")


#######################
replaceWord <- function(corpus, oldwords, newword) {
  for (i in 1:length(oldwords) )
  {
    tm_map(corpus, content_transformer(gsub),pattern=oldwords[i], replacement=newword) }
}

###############
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,function(x) { grep(as.character(x), pattern=paste0("nn<",word)) })
  sum(unlist(results))
}
##################################################################f
replaceWord2 <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),pattern=oldword, replacement=newword) 
}
###################################################################

hu.liu.pos = readLines('https://www.dropbox.com/sh/3xctszdxx4n00xq/AAA_Go_Y3kJxQACFaVBem__ea/positive-words.txt?dl=1');
hu.liu.neg = readLines('https://www.dropbox.com/sh/3xctszdxx4n00xq/AABTGWHitlRZcddq1pPXOSqca/negative-words.txt?dl=1');

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr);
  require(stringr);
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
                 ,"landesbankbaden","landesbankhessen","norddeutschelandesbank")
index_df<-Textprocess(bayernlb$news) 
bayernlb<-cbind(bayernlb,index_df)
bayernlb$score_sentiment<-score.sentiment(bayernlb$news,hu.liu.pos,hu.liu.neg)
bayernlb<-bayernlb[order(bayernlb$date),]
library(data.table)

Daily_impact_median<-aggregate(bayernlb$deutschebank, by= list(Category=bayernlb$date), FUN=count)

myCorpus <- Corpus(VectorSource(bayernlb$news),readerControl = list(language = "en"))

dtm <- DocumentTermMatrix(myCorpus)
freq <- colSums(as.matrix(dtm))
length(freq)

dtmr <-DocumentTermMatrix(myCorpus, control=list(wordLengths=c(1, 10),   bounds = list(global = c(1,10))))
findAssocs(dtmr,"bayernlb",0.8)



