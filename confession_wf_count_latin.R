library("tm")
source('Schinke_Latin_Stemming.R')
# setwd('~/Documents/UChicago/Literature/5/')

dir_name = list(Latin=c("Latin",10), Latin_10_cut=c("Latin",5), Latin_2_cut=c("Latin",1), English=c("English",3), English_10_cut=c("English",5),English_2_cut=c("English",1))

for (k in names(dir_name)) {
  v = dir_name[[k]]
  lan = v[1]
  half_cut_point = as.double(v[2])
  print(c(k, lan, half_cut_point))

  dir <- paste("/Users/dt/Documents/UChicago/Literature/5/",k, sep="")
  #load corpus
  corpus1 <- VCorpus(DirSource(dir), readerControl=list(language=lan))
  
  #remove timestamp
  corpus1 <- tm_map(corpus1, content_transformer(gsub), pattern = "\\d+:\\d+:\\d+", replacement = "")
  corpus1 <- tm_map(corpus1, content_transformer(gsub), pattern = "(CAPUT \\d+|Liber \\w+)", replacement = "")
  
  #run transformations
  corpus1 <- tm_map(corpus1, (stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, (removePunctuation))
  corpus1 <- tm_map(corpus1, (removeNumbers))
  
  #remove problems
  problems<-c("apparat","datumsangaben","seite","page","erl??uterungen", "kommentar")
  corpus1 <- tm_map(corpus1, removeWords, problems)

  #get total number of each book
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  total<-rowSums(corpus1.matrix)
  
  if(lan=='English'){
  #remove english stopwords
    stop = stopwords(lan)
  }
  else if(lan=='Latin'){
  #Latin stopwords
    stop = c("ab", "ac", "ad ", "adhic ", "aliqui ", "aliquis ", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "unus", "ut")
  }
  corpus1 <- tm_map(corpus1, removeWords, stop)
  
  #stem
  if(lan=='English'){
    corpus1 <- tm_map(corpus1, stemDocument, language = lan)
  }
  else if(lan == "Latin"){
    corpus1 <- tm_map(corpus1, content_transformer(schinke_latin_stemming_passage))
  }
    
  #remake DTM
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  #remove sparse terms
  corpus1.dtm.sparse<-removeSparseTerms(corpus1.dtm, .4)
  corpus1.sparse.matrix<-as.matrix(corpus1.dtm.sparse, stringsAsFactors=F)
  #scale
  conversion.scaled<-corpus1.sparse.matrix
  conversion.scaled[,1:ncol(corpus1.sparse.matrix)]<- corpus1.sparse.matrix[,1:ncol(corpus1.sparse.matrix)]/total
  
  non_zero_sum = rowSums(conversion.scaled)
  df = data.frame(distinct=rowSums(corpus1.sparse.matrix!=0), total)
  df['ratio'] = df$distinct/df$total
  
  # Euclidean Distance
  conversion.dist<-dist(conversion.scaled, method = "euclidean")
  freq.dist<-as.matrix(conversion.dist)
  arr = in_cross_half(freq.dist, half_cut_point)
  write.table(df,'wf_count_v2.csv',append=T,sep=',')
}

