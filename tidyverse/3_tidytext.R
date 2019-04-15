library(janeaustenr)
library(dplyr)
library(tidytext)
library(ggplot2)

original_books <- janeaustenr::austen_books() %>%
  dplyr::group_by(book) %>%
  dplyr::mutate(line = dplyr::row_number()) %>%
  dplyr::ungroup()

tidy_books <- original_books %>%
  tidytext::unnest_tokens(word, text)

tidy_books <- tidy_books %>%
  dplyr::anti_join(tidytext::get_stopwords())

tidy_books %>%
  dplyr::count(word, sort = TRUE)

# sentiment analysis
janeaustensentiment <- tidy_books %>%
  dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
  dplyr::count(book, index = line %/% 80, sentiment) %>%
  tidyr::spread(sentiment, n, fill = 0) %>%
  dplyr::mutate(sentiment = positive - negative)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# term matrices
library(tm)
data("AssociatedPress", package = "topicmodels")
AssociatedPress

comparison <- tidytext::tidy(AssociatedPress) %>%
  dplyr::count(word = term) %>%
  dplyr::rename(AP = n) %>%
  dplyr::inner_join(dplyr::count(tidy_books, word)) %>%
  dplyr::rename(Austen = n) %>%
  dplyr::mutate(AP = AP / sum(AP),
                Austen = Austen / sum(Austen))

library(scales)
comparison %>%
  dplyr::mutate(intensity = abs(AP - Austen)/pmax(AP, Austen)) %>%
  ggplot(aes(AP, Austen)) +
    geom_text(aes(label = word, alpha = 1000^(intensity)), check_overlap = TRUE,
              vjust = 1, hjust = 1) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    geom_abline(color = "red") +
    guides(alpha = FALSE)
