
# Question 1.2: Mapper

mapper <- function(text, filename, keyword) {
  
  results <- data.frame(
    document = character(),
    row = integer(),
    column = integer(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(text)) {
    
    positions <- gregexpr(
      pattern = keyword,
      text = text[i],
      ignore.case = TRUE,
      fixed = FALSE
    )[[1]]
    
    if (positions[1] == -1) {
      next
    }
    
    for (position in positions) {
      
      results <- rbind(
        results,
        data.frame(
          document = filename,
          row = i,
          column = position
        )
      )
    }
  }
  
  return(results)
}


# Question 1.3: Reducer

reducer <- function(mapped_results) {
  
  if (nrow(mapped_results) == 0) {
    
    return(data.frame(
      document = character(),
      row = integer(),
      columns = character()
    ))
  }
  
  results <- aggregate(
    column ~ document + row,
    data = mapped_results,
    FUN = function(x) {
      paste(sort(x), collapse = ",")
    }
  )
  
  names(results)[3] <- "columns"
  
  return(results)
}


# Question 1.4: Main Program and Configuration

# Load the Mapper and Reducer functions
source("mapper.R")
source("reducer.R")

# Enter the folder containing the extracted news articles
input_folder <- readline(
  "Enter the full path to your news articles folder: "
)

if (!dir.exists(input_folder)) {
  stop("The folder does not exist. Please check the path.")
}

# Enter the keyword dynamically
keyword <- readline(
  "Enter the keyword you want to search for: "
)

if (nchar(trimws(keyword)) == 0) {
  stop("Please enter a valid keyword.")
}

# Find all files in the folder and its subfolders
files <- list.files(
  input_folder,
  recursive = TRUE,
  full.names = TRUE
)

# Exclude directories
files <- files[!file.info(files)$isdir]

if (length(files) == 0) {
  stop("No files were found in the selected folder.")
}

# Run the Mapper on each document
mapped_results <- list()

for (file in files) {
  
  document_text <- readLines(
    file,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  mapped_results[[length(mapped_results) + 1]] <- mapper(
    text = document_text,
    filename = basename(file),
    keyword = keyword
  )
}

# Combine all Mapper results
all_results <- do.call(
  rbind,
  mapped_results
)

# Run the Reducer
final_results <- reducer(all_results)

# Display the results
print(final_results)

# Save the output to a CSV file
write.csv(
  final_results,
  "keyword_search_results.csv",
  row.names = FALSE
)

cat("\nSearch completed successfully.\n")
cat("Results saved to keyword_search_results.csv\n")
