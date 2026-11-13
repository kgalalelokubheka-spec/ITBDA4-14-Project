# Question 3.1 - Web Scraping and Creation of R Data Structure

if (!requireNamespace("rvest", quietly = TRUE)) {
  install.packages("rvest")
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}

if (!requireNamespace("tm", quietly = TRUE)) {
  install.packages("tm")
}

if (!requireNamespace("tidytext", quietly = TRUE)) {
  install.packages("tidytext")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!requireNamespace("wordcloud", quietly = TRUE)) {
  install.packages("wordcloud")
}

if (!requireNamespace("syuzhet", quietly = TRUE)) {
  install.packages("syuzhet")
}

library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(syuzhet)

url <- "https://en.wikipedia.org/wiki/Climate_change"

page <- read_html(url)

title <- page %>%
  html_element("h1") %>%
  html_text2()

web_text <- page %>%
  html_elements("p") %>%
  html_text2()

web_text <- web_text[
  nchar(str_squish(web_text)) > 50
]

web_data <- data.frame(
  url = url,
  title = title,
  content = paste(web_text, collapse = " "),
  stringsAsFactors = FALSE
)

print(web_data)

cat("Website title:", web_data$title, "\n")
cat("Number of characters:", nchar(web_data$content), "\n")


# Question 3.2 - Textual Data Preprocessing

corpus <- VCorpus(
  VectorSource(web_data$content)
)

corpus <- tm_map(
  corpus,
  content_transformer(tolower)
)

corpus <- tm_map(
  corpus,
  removePunctuation
)

corpus <- tm_map(
  corpus,
  removeNumbers
)

corpus <- tm_map(
  corpus,
  removeWords,
  stopwords("en")
)

corpus <- tm_map(
  corpus,
  stripWhitespace
)

clean_text <- sapply(
  corpus,
  as.character
)

clean_text <- str_squish(clean_text)

clean_data <- data.frame(
  original_content = web_data$content,
  cleaned_content = clean_text,
  stringsAsFactors = FALSE
)

print(clean_data)



# Question 3.3 - Text Summarisation Using Two Techniques

# Technique 1 - Frequency-Based Text Summarisation

word_data <- tibble(
  word = unlist(
    strsplit(
      tolower(clean_text),
      "\\s+"
    )
  )
)

word_data <- word_data %>%
  filter(
    word != "",
    nchar(word) > 2
  ) %>%
  count(
    word,
    sort = TRUE
  )

top_words <- word_data %>%
  slice_head(n = 20)

print(top_words)


# Visualisation 1 - Top 20 Most Frequent Words

ggplot(
  top_words,
  aes(
    x = reorder(word, n),
    y = n
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequent Words",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()


# Visualisation 2 - Word Cloud

wordcloud(
  words = word_data$word,
  freq = word_data$n,
  min.freq = 5,
  max.words = 50,
  random.order = FALSE
)


# Technique 2 - Extractive Text Summarisation

sentences <- unlist(
  strsplit(
    web_data$content,
    "(?<=[.!?])\\s+",
    perl = TRUE
  )
)

sentences <- sentences[
  nchar(str_squish(sentences)) > 20
]

sentence_scores <- data.frame(
  sentence = sentences,
  score = sapply(
    sentences,
    function(sentence) {
      
      sentence_words <- unlist(
        strsplit(
          tolower(sentence),
          "\\s+"
        )
      )
      
      sentence_words <- gsub(
        "[^a-z]",
        "",
        sentence_words
      )
      
      sentence_words <- sentence_words[
        sentence_words %in% word_data$word
      ]
      
      if (length(sentence_words) == 0) {
        return(0)
      }
      
      sum(
        word_data$n[
          match(
            sentence_words,
            word_data$word
          )
        ]
      )
    }
  ),
  stringsAsFactors = FALSE
)

sentence_scores <- sentence_scores %>%
  arrange(
    desc(score)
  )

summary_sentences <- sentence_scores %>%
  slice_head(n = 5)

cat("\nExtractive Text Summary:\n\n")

for (i in seq_len(nrow(summary_sentences))) {
  
  cat(
    i,
    ". ",
    str_squish(
      summary_sentences$sentence[i]
    ),
    "\n\n"
  )
}


# Visualisation 3 - Most Important Sentences

top_sentences <- sentence_scores %>%
  slice_head(n = 10)

top_sentences$sentence <- factor(
  top_sentences$sentence,
  levels = rev(
    top_sentences$sentence
  )
)

ggplot(
  top_sentences,
  aes(
    x = sentence,
    y = score
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Most Important Sentences",
    x = "Sentence",
    y = "Importance Score"
  ) +
  theme_minimal()


# Sentiment Analysis

sentences <- get_sentences(web_data$content)

sentiment_values <- get_sentiment(
  sentences,
  method = "syuzhet"
)

sentiment_data <- data.frame(
  sentence = seq_along(sentences),
  sentiment = sentiment_values
)

print(sentiment_data)


# Visualisation 4 - Sentiment Scores

ggplot(
  sentiment_data,
  aes(
    x = sentence,
    y = sentiment
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Sentiment Scores Across the Web Content",
    x = "Sentence Number",
    y = "Sentiment Score"
  ) +
  theme_minimal()