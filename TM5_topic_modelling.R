# 5 Topic Modelling

library(tidytext)
library(tidyverse)
library(forcats)
library(knitr)
library(kableExtra)
library(topicmodels)

# Get data
# get data and tidy
data <- read_csv("F:/Praxis/online_data.csv")
data = select(data, 1,11:20)
data =
  data %>% rename(
    ID = `Respondent ID`,
    Q1 = 2,
    Q2 = 3,
    Q3 = 4,
    Q4 = 5,
    Q5 = 6,
    Q6 = 7,
    Q7 = 8,
    Q8 = 9,
    Q9 = 10,
    Q10 = 11
  )
# drop 1st row
data = 
  data %>% slice(-1)
data = 
  data %>% 
  pivot_longer(2:11, 
               names_to = 'question',
               values_to = 'word')
# drop na's and order Q's
data %>% drop_na()

data$question <- 
  factor(data$question,
         levels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"))

# tokenize
data("stop_words")

tokens = 
  data %>% 
  unnest_tokens('word', word) %>% 
  anti_join(stop_words, by = c("word" = "word")) %>% 
  drop_na() %>%
  count(word, question) %>% 
  arrange(desc(n))

# Turn tidy data into a DTM
dtm = 
  cast_dtm(tokens,question, word, n)

# make matrix for viewing
non.sparse.matrix <- as.matrix(dtm)
View(non.sparse.matrix)

# Topic Models
# k determines topics
lda <- LDA(dtm, k = 8, control = list(seed = 1234))
lda

# topics and word probabilities
# beta = probability of being associated with topic x
topics <- tidy(lda, matrix = "beta")
topics

topic1 = 
  topics %>% 
  filter(topic==1) %>% 
  arrange(beta)

topic2 = 
  topics %>% 
  filter(topic==2) %>% 
  arrange(beta)

# Find most common words per topic
top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)

## increasing the words per topic reveals greater nuance
## With shorter lists the topics are dominated by the top overall words

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()+
  ggtitle("Most Common Words in a Topic")




# find proportions of answer in a topic
documents <- tidy(lda, matrix = "gamma")
documents$gamma = 
  documents$gamma %>% 
  round(3) %>% 
  group_by(documents)
