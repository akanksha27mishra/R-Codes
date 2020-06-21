install.packages("tm",dependecies = TRUE)
install.packages("wordcloud", dependencies=TRUE)
zomato.data<-read.csv("zomato.csv")
str(zomato.data)
colnames(zomato.data)
zomato.data<-zomato.data[,-c(1,2,8)]
apply(zomato.data,2,function(x)sum(is.na(x)))
str(zomato.data)
head(zomato.data)


library(dplyr)
library(ggplot2)

#Restaurants in Bangalore with Most Outlets
top_rest_outlets<-zomato.data %>% select(name) %>% group_by(name) %>% count() %>% arrange(desc(n))
top_rest_outlets

top_rest_outlets<-top_rest_outlets[1:10,]

#bar chart of this data
top_rest_outlets %>% ggplot(aes(x=reorder(name,n),y=n))+geom_col()+coord_flip()

#Online orders
Rest_online_order<-zomato.data %>% select(name,online_order) %>% group_by(online_order) %>%count()
Rest_online_order

table(zomato.data$online_order)

#PieChart
ggplot(Rest_online_order,aes(x="",y=n,fill=online_order))+
  geom_bar(width = 1,stat="identity",color="white")+
  coord_polar("y",start=0)+
  geom_text(aes(label=round(Rest_online_order$n/sum(Rest_online_order$n)*100,2)),
  position=position_stack(vjust=0.5),size=5)+
  theme_void()


#Booking Table
Rest_table_booking<-zomato.data %>% select(name,book_table) %>% group_by(book_table) %>%count()
Rest_table_booking

color<-c("Red","Green")

ggplot(Rest_table_booking,aes(x="",y=n,fill=book_table))+
  geom_bar(width = 1,stat="identity",color="white")+
  coord_polar("y",start=0)+
  geom_text(aes(label=round(Rest_table_booking$n/sum(Rest_table_booking$n)*100,2)),
            position=position_stack(vjust=0.5),size=5)+
  scale_fill_manual(values = color)
  theme_void()

#gsub
my_text<-"Bangalore          100 Rest         taurants"
gsub("\\s+"," ",my_text)
rating<-zomato.data %>% select(rate) %>% group_by(rate) %>% count()
rating

rating<-rating[-c(1,2,65),]
tail(rating)
rating %>%ggplot(aes(x=reorder(rate,n),y=n))+geom_col(fill="blue")+theme_minimal()+coord_flip()


#Approx cost for 2 people
str(zomato.data)
ACOT<-as.numeric(gsub(",","", as.character(zomato.data$approx_cost.for.two.people.)))
ACOT
summary(ACOT)
boxplot(ACOT)

###############
zomato.data$cuisines
zomato.data$cuisines<-gsub("\\s+","",zomato.data$cuisines)
zomato.data[zomato.data$rest_type=="Quick Bites","cuisines"]
quick.cuisine<-zomato.data%>%select(rest_type,cuisines)%>%filter(rest_type=="Quick Bites")
quick.cuisine$cuisines

library(tm)
library(wordcloud)

text.cuisine<-quick.cuisine$cuisines
text.cuisine<-paste(text.cuisine,collapse = " ")
text.cuisine
myCorpus<-Corpus(VectorSource(text.cuisine))
inspect(myCorpus[1])

myCorpus<-tm_map(myCorpus,tolower)
myCorpus<-tm_map(myCorpus,removePunctuation)
myCorpus<-tm_map(myCorpus,removeNumbers)
myCorpus<-tm_map(myCorpus,removeWords,stopwords("english"))
myCorpus<-tm_map(myCorpus,stripWhitespace)
myCorpus<-tm_map(myCorpus,PlainTextDocument)
mytdm<-TermDocumentMatrix(myCorpus)
findFreqTerms(mytdm,lowfreq = 10)

termFrequency<-rowSums(as.matrix(mytdm))
termFrequency<-subset(termFrequency,termFrequency>=50)
barplot(termFrequency)

m<-as.matrix(mytdm)
myrowSum<-rowSums(m)
wordFreq<-sort(myrowSum,decreasing = TRUE)
wordcloud(words = names(wordFreq),freq = wordFreq,min.freq = 100,max.words = 50,random.order = FALSE,colors = rainbow(20))


#########review############
zomato.reviews<-zomato.data$reviews_list
length(zomato.reviews)

review.Corpus<-Corpus(VectorSource(zomato.reviews))
review.Corpus<-tm_map(review.Corpus,tolower)
review.Corpus<-tm_map(review.Corpus,removePunctuation)
review.Corpus<-tm_map(review.Corpus,removeNumbers)
review.Corpus<-tm_map(review.Corpus,removeWords,stopwords("english"))
review.Corpus<-tm_map(review.Corpus,stripWhitespace)
review.Corpus<-tm_map(review.Corpus,PlainTextDocument)
mytdm<-TermDocumentMatrix(review.Corpus)

##################
review.Corpus<-VCorpus(VectorSource(zomato.reviews))
review.Corpus<-tm_map(review.Corpus,tolower)
review.Corpus<-tm_map(review.Corpus,removePunctuation)
review.Corpus<-tm_map(review.Corpus,removeNumbers)
review.Corpus<-tm_map(review.Corpus,removeWords,stopwords("english"))
review.Corpus<-tm_map(review.Corpus,stripWhitespace)
review.Corpus<-tm_map(review.Corpus,PlainTextDocument)
mytdm<-TermDocumentMatrix(review.Corpus)

checkRevMatrix<-as.data.frame(as.matrix(tdm.Review[,1:10]))
write.csv(checkRevMatrix,"checkRevMatrix.csv")

inspect(review.Corpus[1])