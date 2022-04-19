# comparing the use of Documents

# 6 Finding Quotes
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

# make one DTM for ID and one for Q

# Q
# making a document term matrix: the input object for topic models
dtm1 = 
  data %>% 
  unnest_tokens('word', word) %>% 
  anti_join(stop_words, by = c("word" = "word")) %>% 
  drop_na() %>%
  count(word, question) %>% 
  arrange(desc(n)) %>% 
  cast_dtm(question, word, n)

matrix_q = as.matrix(dtm1)

# ID 
dtm2 = 
  data %>% 
  unnest_tokens('word', word) %>% 
  anti_join(stop_words, by = c("word" = "word")) %>% 
  drop_na() %>%
  count(word, ID) %>% 
  arrange(desc(n)) %>% 
  cast_dtm(ID, word, n)

matrix_ID = as.matrix(dtm2)

# these DTMs then form the inout for the topic models

# 1. for question
lda1 <- LDA(dtm1, k = 4, control = list(seed = 1234))
topics1 <- tidy(lda1, matrix = "beta")

top_terms1 <- topics1 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms1$topic =
  recode_factor(top_terms1$topic, 
                `1` = "T1",
                `2` = "T2",
                `3` = "T3",
                `4` = "T4")

top_terms1 %>%
  filter(topic == "T1" 
         | topic == "T2" 
         | topic == "T3" 
         | topic == "T4") %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# 2. for ID
lda2 <- LDA(dtm2, k = 4, control = list(seed = 1234))
topics2 <- tidy(lda2, matrix = "beta")

top_terms2 <- topics2 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms2$topic =
  recode_factor(top_terms2$topic, 
                `1` = "T1",
                `2` = "T2",
                `3` = "T3",
                `4` = "T4")

top_terms2 %>%
  filter(topic == "T1" 
         | topic == "T2" 
         | topic == "T3" 
         | topic == "T4") %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## this produce topics that are DIFFERENT!
## the function LDA() takes the DTM as an input
# so essentially we are asking:
# how do word co-appear in questions? Or
# how do words co-appear in individual responses?
# it's like the unit of analysis and leads to very different results

# the simplest way round would be to pick questions associated with each
# topic and do qualitative analysis on them
# suspect this will be rather obvious tho

# find proportions of answer in a topic
documents = tidy(lda1, matrix = "gamma")
documents$gamma = 
  documents$gamma %>% 
  round(3) 

documents = 
  documents %>% 
  group_by(topic)

documents %>% 
  filter(topic=='2') %>% 
  arrange(desc(gamma)) %>%
  slice_head(n=10)
