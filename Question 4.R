# Question 4.1 - Web Scraping, Regex and Data Frame Creation

if (!requireNamespace("rvest", quietly = TRUE)) {
  install.packages("rvest")
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}

library(rvest)
library(dplyr)
library(stringr)
library(readr)

url <- "https://news.ycombinator.com/"

page <- read_html(url)

story_nodes <- html_elements(
  page,
  "tr.athing"
)

raw_data <- lapply(
  story_nodes,
  function(story) {
    
    title_node <- html_element(
      story,
      "span.titleline a"
    )
    
    title <- html_text2(title_node)
    
    story_url <- html_attr(
      title_node,
      "href"
    )
    
    details <- html_element(
      story,
      xpath = "following-sibling::tr[1]"
    )
    
    details_text <- html_text2(
      details
    )
    
    points_match <- str_match(
      details_text,
      "([0-9,]+) points?"
    )
    
    comments_match <- str_match(
      details_text,
      "([0-9,]+) comments?"
    )
    
    author_match <- str_match(
      details_text,
      "by\\s+([^\\s]+)"
    )
    
    points <- ifelse(
      is.na(points_match[, 2]),
      NA,
      as.numeric(
        gsub(
          ",",
          "",
          points_match[, 2]
        )
      )
    )
    
    comments <- ifelse(
      is.na(comments_match[, 2]),
      NA,
      as.numeric(
        gsub(
          ",",
          "",
          comments_match[, 2]
        )
      )
    )
    
    author <- author_match[, 2]
    
    data.frame(
      title = title,
      url = story_url,
      author = author,
      points = points,
      comments = comments,
      stringsAsFactors = FALSE
    )
  }
)

raw_data <- bind_rows(
  raw_data
)

raw_data <- raw_data %>%
  mutate(
    word_count = str_count(
      title,
      "\\S+"
    ),
    title_length = nchar(title)
  )

raw_data <- raw_data %>%
  slice_head(n = 30)

print(raw_data)

cat(
  "Number of records:",
  nrow(raw_data),
  "\n"
)

cat(
  "Number of columns:",
  ncol(raw_data),
  "\n"
)


# Question 4.1 - Regex Analysis

raw_data <- raw_data %>%
  mutate(
    extracted_points = str_extract(
      paste0(
        points,
        " points"
      ),
      "[0-9]+"
    ),
    extracted_numbers = str_extract(
      title,
      "[0-9]+"
    )
  )

print(raw_data)


# Question 4.2 - Data Cleaning, Denoising and Missing-Value Imputation

cleaned_data <- raw_data %>%
  mutate(
    title = str_squish(title),
    author = str_squish(author),
    url = str_squish(url)
  )

cleaned_data <- cleaned_data %>%
  mutate(
    title = str_replace_all(
      title,
      "[^[:alnum:][:space:]'?!.,:-]",
      ""
    )
  )

cleaned_data <- cleaned_data %>%
  mutate(
    points = ifelse(
      is.na(points),
      0,
      points
    ),
    comments = ifelse(
      is.na(comments),
      0,
      comments
    )
  )

cleaned_data <- cleaned_data %>%
  filter(
    !is.na(title),
    title != ""
  )

cleaned_data <- cleaned_data %>%
  distinct(
    title,
    .keep_all = TRUE
  )

cleaned_data <- cleaned_data %>%
  mutate(
    word_count = str_count(
      title,
      "\\S+"
    ),
    title_length = nchar(title)
  )

print(cleaned_data)

cat(
  "Number of cleaned records:",
  nrow(cleaned_data),
  "\n"
)

cat(
  "Missing values:\n"
)

print(
  colSums(
    is.na(cleaned_data)
  )
)


# Export Cleaned Dataset

write_csv(
  cleaned_data,
  "cleaned_hacker_news_data.csv"
)

cat(
  "Cleaned dataset saved as cleaned_hacker_news_data.csv\n"
)



install.packages(c("DBI", "RPostgres"))
library(DBI)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "etl_project",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "l3l0kub$22"
)

cleaned_data <- read.csv(
  "/Users/lelo.kubs/cleaned_hacker_news_data.csv",
  stringsAsFactors = FALSE
)

dbWriteTable(
  con,
  "hacker_news",
  cleaned_data,
  overwrite = TRUE
)
  
dbGetQuery(
  con,
  "SELECT COUNT(*) AS total_records FROM hacker_news;"
)

top_posts <- dbGetQuery(
  con,
  "
  SELECT title, points, comments
  FROM hacker_news
  ORDER BY points DESC
  LIMIT 10;
  "
)

print(top_posts)

engagement <- dbGetQuery(
  con,
  "
  SELECT
      ROUND(AVG(points), 2) AS average_points,
      ROUND(AVG(comments), 2) AS average_comments,
      MAX(points) AS highest_points,
      MAX(comments) AS highest_comments
  FROM hacker_news;
  "
)

print(engagement)
