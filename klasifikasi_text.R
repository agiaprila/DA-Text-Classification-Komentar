library(readr) #import data 
library(dplyr) #manipulasi bentuk data

#Text mining packages
library(tm) 
library(SnowballC)
t1 <- read_csv("data/Womens Clothing E-Commerce Reviews.csv")

#memilih kolom
t1 <- t1 %>%
  select(`Recommended IND`, `Review Text`)

#mengubah nama kolom
colnames(t1) <- c("recommended", 'review')

#merubah tipe data
str(t1)
t1$recommended <- as.factor(t1$recommended)

#menghitung perbandingan
t1 %>%
  group_by(recommended) %>%
  summarise(n())

glimpse(t1)  

#Step 1 - Create the Text Corpus
corpus = Corpus(VectorSource(t1$review))
corpus[[1]][1]
t1$recommended[1]

#Step 2 - Conversion to Lowercase
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus[[1]][1]  

#Step 3 - Removing Punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]][1]

#Step 4 - Removing Stopwords
corpus = tm_map(corpus, removeWords, c("cloth", stopwords("english")))
corpus[[1]][1]  

#Step 5 - Stemming
corpus = tm_map(corpus, stemDocument)
corpus[[1]][1]  


#wordcloud
dokkudtm <- TermDocumentMatrix(corpus) 
matrix_text <- as.matrix(dokkudtm)

matrix_text <- sort(rowSums(matrix_text),decreasing=TRUE)
matrix_text

data_text <- data.frame(word = names(matrix_text),freq=matrix_text) 
head(data_text, 15)

library(wordcloud)
wordcloud(words = data_text$word, freq = data_text$freq, min.freq = 1,           
          max.words=50, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = data_text$word, freq = data_text$freq, min.freq = 1,           
          max.words=50, random.order=FALSE, rot.per=0.35,            
          colors=c("chartreuse", "cornflowerblue", "darkorange"))


#modelling
#Create Document Term Matrix
frequencies = DocumentTermMatrix(corpus)

sparse = removeSparseTerms(frequencies, 0.995)

#final data
tSparse = as.data.frame(as.matrix(sparse))
colnames(tSparse) = make.names(colnames(tSparse))
tSparse$recommended_id = t1$recommended


#frekuensi
prop.table(table(tSparse$recommended_id)) #73.6% is the baseline accuracy


#split
library(caTools)
library(caret)
set.seed(100)
split = sample.split(tSparse$recommended_id, SplitRatio = 0.7)
trainSparse = subset(tSparse, split==TRUE)
testSparse = subset(tSparse, split==FALSE)


#model
library(C50)
set.seed(100)
trainSparse$recommended_id = as.factor(trainSparse$recommended_id)
testSparse$recommended_id = as.factor(testSparse$recommended_id )

#Lines 5 to 7
c50_model = C5.0(recommended_id ~ ., data=trainSparse)
predictc50 = predict(c50_model, newdata=testSparse)
table(testSparse$recommended_id, predictc50)

confusionMatrix(testSparse$recommended_id, predictc50)

predict(c50_model, newdata=testSparse)
