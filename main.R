corpus_url = read.table("corpus.txt") # get just .htm and .html files

# load packages
library(tm)
library(RCurl)
library(XML)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
source("htmlToText.R")

#get html pages one by one
for (i in 1:length(corpus_url[,1])){

    html = getURL(as.character(corpus_url[i,1]))
    print(as.character(corpus_url[i,1]))
    #html = getURL(as.character(corpus[3,1]))
    
    # convert HTML to text
    html2txt = lapply(html, htmlToText)
    # clean out non-ASCII characters
    html2txtclean = sapply(html2txt, function(x) iconv(x, "latin1", "ASCII", sub=""))
    
    # make corpus for text mining
    corpus = Corpus(VectorSource(html2txtclean),list(language="en"))
    
    #
    # Step 1: Lexical analysis and Skip words
    #
    skipWords = function(x) removeWords(x, stopwords("english"))
    funcs = list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
    corpus_lexical = tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
    corpus_text = tm_map(corpus_lexical, PlainTextDocument)
    #a[[1]]$content
    
    #
    # Step 2: Stemm Document
    #
    
    stemmed = tm_map(corpus_text,stemDocument)
    
    #
    # Step 3: get frequencies
    #
    
    dtm = DocumentTermMatrix(stemmed)
    
    
    m = as.matrix(dtm)
    # get word counts in decreasing order
    word_freqs = sort(colSums(m), decreasing=TRUE) 
    # create a data frame with words and their frequencies
    dm = data.frame(word=names(word_freqs), freq=word_freqs)
    
    #generate the matrix
    #first document
    if (i == 1) {
      corpus_matrix = dm
      corpus_matrix[,1] = as.character(corpus_matrix[,1])
      corpus_matrix[,2] = as.numeric(corpus_matrix[,2])
      
    } else {
      #create a column
      corpus_matrix[,(i+1)] = rep(0,length(corpus_matrix[,1]))
      for(j in 1:length(dm[,1])){
        #if word doesnt exist add row
        if(length(which(as.character(dm$word[j]) == corpus_matrix[,1]))==0){
          row = c(as.character(dm$word[j]),rep(0,(length(corpus_matrix)-2)),as.numeric(dm$freq[j]))
          corpus_matrix = rbind(corpus_matrix,row)
        }else{
          corpus_matrix[which(as.character(dm$word[j]) == as.character(corpus_matrix$word)),(i+1)] = as.numeric(dm$freq[j])
        }
      }
      
    }
} 

headers = c("word")
for (i in 2:length(corpus_matrix[1,])){
    headers = cbind(headers,paste("doc",(i-1),sep=""))
}
names(corpus_matrix) = headers

#format output
for (i in 2:length(corpus_matrix[1,])) {
  corpus_matrix[,i] = as.numeric(corpus_matrix[,i])
}
#total corpus frequencies
corpus_matrix$total = rowSums(corpus_matrix[2:length(corpus_matrix[1,])])
#order by frequency
by_frequency = corpus_matrix[order(-corpus_matrix$total),]

write(corpus_matrix$word,file="Vocabulario.txt",ncolumns=length(corpus_matrix$word))

#number of docs
N = length(corpus_url[,1])
#get number of docs where each word appear
corpus_matrix$n_docs = apply(corpus_matrix[2:(length(corpus_matrix[1,])-1)],1,function(x) length(which((x!=0)==TRUE)))
#get global weights TF -IDF
corpus_matrix$TF_IDF = corpus_matrix$total * log(rep(N,length(corpus_matrix$n_docs))/corpus_matrix$n_docs)
write(paste(format(round(as.numeric(corpus_matrix$TF_IDF),3),nsmall=2),corpus_matrix$word,sep=""),
      file="Pesos_Globales.txt",ncolumns=length(corpus_matrix$word))

#local weights
local_total_freq = colSums(corpus_matrix[2:(length(corpus_matrix[1,])-3)])
for (i in 1: N) {
  fichero = paste("Pesos_Locales_doc",i,".txt",sep="")
  weighted_term_freq = corpus_matrix[,(i+1)] /rep(local_total_freq[i],length(corpus_matrix$word))
  #take words which belong to the document
  w = corpus_matrix$word[which(corpus_matrix[,(i+1)]!=0)]
  f = weighted_term_freq[which(weighted_term_freq!=0)]
  write(paste(format(round(as.numeric(f),5),nsmall=5),w,sep=""),
        file=fichero,ncolumns=length(w))
}

#get frequencies by document
frequencies = cbind(corpus_matrix$word,rep("->",length(corpus_matrix$word)))
for (i in 1:N) {
  doc = rep(paste("doc",i,":",sep=""),length(corpus_matrix$word))  
  f_doc = paste(doc,corpus_matrix[,(i+1)],sep="")  
  frequencies = cbind(frequencies,f_doc)
}

for (i in 1:length(frequencies[,1])) {
  write(frequencies[i,],file="Fichero_Invertido.txt",ncolumns=length(frequencies[i,]),append=TRUE)
}

# plot wordcloud
file = "TF_IDF.pdf"
pdf(file)
wordcloud(corpus_matrix$word, corpus_matrix$TF_IDF, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

file = "freq.pdf"
pdf(file)
wordcloud(corpus_matrix$word, corpus_matrix$total, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
