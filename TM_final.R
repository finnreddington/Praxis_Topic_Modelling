# TM 7 topics final

# 5 Topic Modelling

library(tidytext)
library(tidyverse)
library(forcats)
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

# Topic Models
# k determines topics
lda <- LDA(dtm, k = 7, control = list(seed = 1234))

# topics and word probabilities
# beta = probability of being associated with topic x
topics <- tidy(lda, matrix = "beta")

# Find most common words per topic
top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms$topic =
  recode_factor(top_terms$topic, 
                `1` = "T1 - Information and Trust",
                `2` = "T2 - Global Impacts",
                `3` = "T3 - Bristol Transport",
                `4` = "T4 - Local Media",
                `5` = "T5 - Emotional Response",
                `6` = "T6 - Transport",
                `7` = "T7 - Bristol and CC")

top_terms %>%
#  filter(topic == "T1 - Information and Trust" 
 #        | topic == "T2 - Global Impacts" 
  #       | topic == "T3 - Bristol Transport" 
   #      | topic == "T4 - Local Media") %>% 
   filter(topic ==  "T5 - Emotional Response"|
             topic == "T6 - Transport"|
             topic == "T7 - Bristol and CC") %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# find proportions of answer in a topic
#documents <- tidy(lda, matrix = "gamma")
#documents$gamma = 
#  documents$gamma %>% 
#  round(3) %>% 
#  group_by(documents)
