#create data
connectivity <- data.frame(response_variable = response_variable, connectivity_metric = connectivity_metric)

response_variable <- c(CP$response_variable, DC$response_variable, ES$response_variable, LY$response_variable, MB$response_variable, PG$response_variable, RP$response_variable)

connectivity_metric <- c(CP$connectivity_metric, DC$connectivity_metric, ES$connectivity_metric, LY$connectivity_metric, MB$connectivity_metric, PG$connectivity_metric, PG$connectivity_metric)

connectivity_2 <- data.frame(response_variable = response_variable, connectivity_metric = connectivity_metric)

connectivity_all <- rbind(connectivity, connectivity_2)

write.csv(connectivity_all, "data_clean/word_cloud_data.csv")

library(tm)
library(SnowballC)
library(wordcloud)

datCorpus <- Corpus(VectorSource(connectivity_all$response_variable))

datCorpus <- Corpus(VectorSource(connectivity_all$connectivity_metric))

#datCorpus <- tm_map(datCorpus, stemDocument)
datCorpus <- tm_map(datCorpus, removePunctuation)
#datCorpus <- tm_map(datCorpus, removeNumbers)
datCorpus <- tm_map(datCorpus, tolower)

datCorpus <- tm_map(datCorpus, removeWords, c('and', 'unclear', 'connected', 'connectivity', 'connection', stopwords('english')))

pal <- brewer.pal(10, "Paired")

tiff(filename = "word_cloud_response_variable.tiff", width = 10, height = 8, units = "in", res = 300)

wordcloud(datCorpus, max.words = 1000, random.order = FALSE, col = pal, scale = c(1.75,0.25))

dev.off()

