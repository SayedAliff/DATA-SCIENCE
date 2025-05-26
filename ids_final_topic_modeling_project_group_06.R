library(tidytext)
library(dplyr)
library(topicmodels)
library(ggplot2)

all_news <- read.csv("F:\\1DS final\\final project\\ids_final_project_group_06_news_clean.csv", header = TRUE, sep = ",")


dtm <- DocumentTermMatrix(all_news)


word_freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
head(word_freq, 20)


num_topics <- 5
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))




topics <- tidy(lda_model, matrix = "beta")


top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)


print(top_terms)



top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms in Each Topic", x = "Terms", y = "Beta")


document_topics <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  top_n(1, gamma)


all_news$topic <- document_topics$topic
