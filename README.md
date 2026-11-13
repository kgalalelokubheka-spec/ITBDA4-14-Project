# ITBDA4-14 Project 2

## Project Overview

This project focuses on applying different data analytics and data processing techniques using R and PostgreSQL. The project covers MapReduce, topic modelling, web scraping, text analysis, and ETL.

The main aim was to work with different types of data, clean and transform the data, and then use suitable analytical methods to obtain useful information from it.

## Question 1 – MapReduce

The first question demonstrates the MapReduce concept using R. Seven text documents were processed to search for the keyword "africa". The mapper identified where the keyword appeared in each document, while the reducer grouped the results according to the document and row.

## Question 2 – Topic Modelling

The second question uses news documents to identify common topics within the collection. Latent Dirichlet Allocation (LDA) was applied after the text had been cleaned and preprocessed. The most important words for each topic were then identified and visualised.

## Question 3 – WebText Analysis

For Question 3, a Wikipedia article about climate change was used as the web source. The article was scraped using R and the extracted text was cleaned before analysis. Different text summarisation methods were applied, together with visualisations and sentiment analysis, to understand the main information contained in the article.

## Question 4 – ETL using R and PostgreSQL

The final question demonstrates an ETL process using data collected from Hacker News. The scraped information included article titles and numerical engagement measures such as points and comments.

The raw data was cleaned and transformed in R before being exported as a CSV file. The cleaned dataset was then loaded into PostgreSQL, where SQL queries were used to investigate the data and identify patterns in user engagement.

## Technologies Used

- R
- RStudio
- PostgreSQL
- SQL
- R packages for text mining and web scraping
- GitHub

## Author

Kgalalelo Kubheka
