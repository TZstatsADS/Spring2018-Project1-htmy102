speechesCloud <- function(filenames,max=100,color=brewer.pal(9,"Blues"), rm.words = character(0)){
  mydoc<- NULL
  for(i in 1:length(filenames)){
    newdoc<-readLines(filenames[i])
    mydoc <- rbind(mydoc,newdoc)
  }

  #create corpus
  docs <- Corpus(VectorSource(mydoc))

  #remove potentially problematic symbols
  docs <-tm_map(  docs , stripWhitespace)
  docs <-tm_map(  docs , content_transformer(tolower))
  docs <-tm_map(  docs , removeWords, stopwords("english"))
  docs <-tm_map(  docs , removeWords, character(0))
  docs <-tm_map(  docs , removePunctuation)
  docs <-tm_map(  docs , removePunctuation)
  # remove modal verb
  docs <-tm_map(  docs , removeWords, rm.words)
  
  #make TDM
  tdm.all<-TermDocumentMatrix(docs)
  
  tdm.tidy=tidy(tdm.all)
  #summary 
  tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))
  #plot wordCloud 
  wordcloud(tdm.overall$term, tdm.overall$`sum(count)`,
            scale=c(5,0.5),
            max.words=max,
            min.freq=1,
            random.order=FALSE,
            rot.per=0.3,
            use.r.layout=T,
            random.color=FALSE,
            colors=color)
}