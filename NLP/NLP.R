# natural language processing - Bag of Words

# here are our models that generate confusion matrixes
source("~/code/Machine Learning A-Z (Codes and Datasets)/Own Code/MachineLearning/NLP/models.R")

# this is the setup of the bag of words from the input data 
setupNLP <- function() {
  setwd("~/code/Machine Learning A-Z (Codes and Datasets)/Part 7 - Natural Language Processing/Section 36 - Natural Language Processing/R")
  
  # importing the dataset
  dataset_original <- read.delim("Restaurant_Reviews.tsv", quote = '', stringsAsFactors = FALSE)
  
  # clean text to remove irrelevant words
  library(tm)
  library(SnowballC)
  
  corpus <- VCorpus(VectorSource(dataset_original$Review))
  corpus <- tm_map(x = corpus, content_transformer(tolower)) # make sure everything is in lower case
  corpus <- tm_map(x = corpus, removeNumbers)                # remove numbers from the corpus
  corpus <- tm_map(x = corpus, removePunctuation)            # remove punctuation
  corpus <- tm_map(x = corpus, removeWords, stopwords())     # remove unhelpful words
  corpus <- tm_map(x = corpus, stemDocument)                 # replace words with their root
  corpus <- tm_map(x = corpus, stripWhitespace)              # remove extra spaces
  
  # creating the Bag of Words model
  dtm <- DocumentTermMatrix(corpus)                          # create the sparse matrix
  dtm <- removeSparseTerms(dtm, 0.999)                       # keep 99.9% of the most frequent words 
  
  # transform the data format so that it works for Random Tree Classification
  dataset <- as.data.frame(as.matrix(dtm))
  dataset$Liked <- dataset_original$Liked
  
  # split the dataset in training and test set
  library(caTools)
  set.seed(123)
  split <- sample.split(dataset$Liked, SplitRatio = 0.8)
  training_set <- subset(dataset, split == TRUE)
  test_set <- subset(dataset, split == FALSE)
  return(list(training_set = training_set, test_set = test_set))
}

processed_data <- setupNLP()
training_set <- processed_data$training_set
test_set <- processed_data$test_set

# metrics to judge the quality of a model
metrics <- lapply(cms, FUN = function(cm) {
  metrics <- list()
  metrics$accuracy <- (cm[1] + cm[4]) / (sum(cm))
  metrics$precision <- cm[4] / (cm[4]+cm[3])
  metrics$recall <- cm[4] / (cm[4] + cm[2])
  metrics$f1_score <- 2 * metrics$precision * metrics$recall / (metrics$precision + metrics$recall)
  metrics
})

library(reshape2)
analysis <- plyr::ldply(metrics, data.frame)
analysis <- melt(analysis,id.vars = ".id", measure.vars = c("accuracy","precision","recall","f1_score"))

library(ggplot2)
gg <- ggplot(analysis) + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1)) +
  scale_fill_brewer(palette = "Spectral") +
  geom_col(aes(x = .id, y = value, fill = variable), position = "dodge") 

plotly::ggplotly(gg)
