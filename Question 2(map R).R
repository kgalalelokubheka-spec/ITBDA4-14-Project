source("mapper.R")

test_text <- c(
  "South Africa is a beautiful country.",
  "AFRICA has many countries.",
  "The weather is warm today."
)

mapper_results <- mapper(
  text = test_text,
  filename = "test.txt",
  keyword = "africa"
)

print(mapper_results)