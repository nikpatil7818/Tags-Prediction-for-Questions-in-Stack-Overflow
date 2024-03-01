library(httr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(h2o)
library(keras)
library(stringr)
library(tm)
library(h2o)
library(word2vec)
library(doc2vec)
library(tokenizers)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(tm)
library(skimr)
library(purrr)
library(text2vec)

# Read data from CSV file
df <- read.csv("stackexchange_data.csv")
#df <- df %>% sample_n(70000)
print(nrow(df))

summary(df)
skim(df)

# Check whether there are any title or tags missing
count_missing_rows <- sum(is.na(df$title) | is.na(df$tags))
print(count_missing_rows)

#Drop Duplicates
df <- distinct(df)
print(nrow(df))

#Add spaces instead of pipes
df$tags <- str_replace_all(df$tags, "\\|", " ")

# Concatenate tags to questions using a specific separator
df$concatenated <- paste(df$title, df$tags, sep = ' ')



# Read data from CSV file
df <- read.csv("stackexchange_data.csv")
#df <- df %>% sample_n(70000)
print(nrow(df))


# Check whether there are any title or tags missing
count_missing_rows <- sum(is.na(df$title) | is.na(df$tags))
print(count_missing_rows)

#Drop Duplicates
df <- distinct(df)
print(nrow(df))

#Add spaces instead of pipes
df$tags <- str_replace_all(df$tags, "\\|", " ")

# Concatenate tags to questions using a specific separator
df$concatenated <- paste(df$title, df$tags, sep = ' ')

# -----------------------------------------------------------------------


############################## Questions asked over Years ################################

df_tags_binary_monthly_count <- df %>%
  mutate(creation_date = as.POSIXct(creation_date, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(year = format(creation_date, "%Y"))

# Create a bar chart
ggplot(df_tags_binary_monthly_count, aes(x = year)) +
  geom_bar(stat = "count", fill = "skyblue") +
  labs(title = "Yearly Question Count", x = "Year", y = "Question Count") +
  theme_minimal()


################ Percentage marked as answered vs. questions ##############

# Assuming 'last_activity_date' and 'creation_date' are in Unix timestamp format
df$last_activity_date <- as.POSIXct(df$last_activity_date, origin = "1970-01-01", tz = "UTC")
df$creation_date <- as.POSIXct(df$creation_date, origin = "1970-01-01", tz = "UTC")

# Extract the year from the date columns
df$year <- format(df$creation_date, "%Y")

# Convert 'is_answered' to logical (if not already)
df$is_answered <- as.logical(df$is_answered)

# Calculate the percentage of questions marked as answered for each year
answered_percentage <- df %>%
  group_by(year) %>%
  summarise(percentage_answered = mean(is_answered, na.rm = TRUE) * 100)

# Define custom color palette
custom_palette <- c("#1f78b4", "#33a02c")  # Blue and green

# Create a beautiful plot
ggplot(answered_percentage, aes(x = year, y = percentage_answered, group = 1)) +
  geom_line(color = custom_palette[1], size = 1.5) +
  geom_point(color = custom_palette[1], size = 3) +
  labs(title = "Percentage of Questions Marked as Answered Over the Years",
       x = "Year",
       y = "Percentage Answered") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = custom_palette)


######################### Common words/tags in titles ###############################

# Combine all titles into a single character vector
all_titles <- paste(df$title, collapse = " ")

# Convert to lowercase to ensure case-insensitive counting
all_titles <- tolower(all_titles)

# Tokenize the text into words
title_words <- unlist(strsplit(all_titles, "\\s+"))

# Filter out common English stop words if needed
title_words <- title_words[!title_words %in% stopwords("en")]

# Create a table of word frequencies
word_freq <- table(title_words)

# Convert the table to a data frame
word_freq_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
colnames(word_freq_df) <- c("word", "freq")

# Sort by frequency in descending order
word_freq_df <- word_freq_df[order(-word_freq_df$freq), ]

# Limit the number of words in the word cloud
num_words_to_plot <- 150
word_freq_df <- head(word_freq_df, num_words_to_plot)

# Plotting the word cloud
wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, scale = c(3, 0.5), min.freq = 1, colors = brewer.pal(8, "Dark2"))


########################## Top 5 Tags by Occurrence Frequency ##############################

# Create a table of tag frequencies
tag_frequencies <- df %>%
  separate_rows(tags, sep = "\\|") %>%
  count(tags, sort = TRUE)

# Select the top 5 tags
top_tags <- head(tag_frequencies, 5)

# Plotting the top 5 tags by occurrence frequency
ggplot(top_tags, aes(x = reorder(tags, -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 5 Tags by Occurrence Frequency", x = "Tags", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


####################### Top 5 Tags by Total View Count #################


# Assuming df is your data frame
# Convert view_count to numeric (in case it's not already)
df$view_count <- as.numeric(df$view_count)

# Create a table of tag frequencies
tag_view_counts <- df %>%
  group_by(tags) %>%
  summarise(total_views = sum(view_count)) %>%
  arrange(desc(total_views)) %>%
  head(5)

# Create a bar plot of the top 5 tags by view count
ggplot(tag_view_counts, aes(x = reorder(tags, -total_views), y = total_views)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 5 Tags by View Count", x = "Tags", y = "Total View Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#######################  Tag Growth/Shrinking Over Years ################################

# Extract relevant columns and convert creation_date to POSIXct
df_tags_time <- df %>%
  select(tags, creation_date) %>%
  mutate(creation_date = as.POSIXct(creation_date, origin = "1970-01-01", tz = "UTC"))

# Extract year from creation_date
df_tags_time <- df_tags_time %>%
  mutate(year = format(creation_date, "%Y"))

# Create a table of tag frequencies over time
tag_counts_over_time <- df_tags_time %>%
  separate_rows(tags, sep = "\\|") %>%
  count(year, tags)

# Filter for the top 5 tags
top_tags <- tag_counts_over_time %>%
  group_by(tags) %>%
  summarise(total_count = sum(n)) %>%
  arrange(desc(total_count)) %>%
  head(5)

manual_tags <- c("pandas", "tensorflow", "pyspark", "flask")

# Filter the main data frame for the specified tags and a specific range of years (e.g., 2020 to 2022)
df_manual_tags_filtered <- df %>%
  filter(str_detect(tags, paste(manual_tags, collapse = "|"))) %>%
  filter(creation_date >= as.POSIXct("2020-01-01", tz = "UTC") & creation_date <= as.POSIXct("2022-12-31", tz = "UTC"))

# Plotting tag growth/shrinking over time using a bar chart
ggplot(tag_counts_over_time %>% filter(tags %in% manual_tags), aes(x = year, y = n, fill = tags)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d() +  # You can choose a different color scale if needed
  labs(title = "Tag Growth/Shrinking Over Time", x = "Year", y = "Tag Count", fill = "Tag") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# -------------------------------------------------------------------------



# Extract and split tags from each row
all_tags <- unlist(strsplit(as.character(df$tags), " "))

# Get unique tags
unique_tags <- unique(all_tags)

# Print the result
print(unique_tags)


#Cleaning Punctuations
clean_text <- function(text) {
  if (is.character(text)) {
    # Pre-Processing of the text
    text <- str_replace_all(text, "can't", "cannot")
    text <- str_replace_all(text, "won't", "will not")
    text <- str_replace_all(text, "want's", "wants")
    text <- str_replace_all(text, "when'd", "when did")
    text <- str_replace_all(text, "can'tif", "cannot if")
    text <- str_replace_all(text, "y'know", "you know")
    text <- str_replace_all(text, "y'all", "you all")
    text <- str_replace_all(text, "y'think", "you think")
    text <- str_replace_all(text, "d'you", "do you")
    text <- gsub("\'s", " is", text)
    text <- gsub("\'d", " had", text)
    text <- gsub("n't", " not", text)
    text <- gsub("\'ve", " have", text)
    text <- gsub("\'ll", " will", text)
    text <- gsub("\'m", " am", text)
    text <- gsub("\'re", " are", text)
    text <- gsub("can’t", "cannot", text)
    text <- gsub("won’t", "will not", text)
    text <- gsub("want’s", "wants", text)
    text <- gsub("when’d", "when did", text)
    text <- gsub("can’tif", "cannot if", text)
    text <- gsub("y’know", "you know", text)
    text <- gsub("y’all", "you all", text)
    text <- gsub("y’think", "you think", text)
    text <- gsub("d’you", "do you", text)
    text <- gsub("\\’s", " is", text)
    text <- gsub("\\’d", " had", text)
    text <- gsub("n’t", " not", text)
    text <- gsub("\\’ve", " have", text)
    text <- gsub("\\’ll", " will", text)
    text <- gsub("\\’m", " am", text)
    text <- gsub("\\’re", " are", text)
    text <- gsub("\\’ve", " have", text)
    text <- gsub(":", " ", text)
    text <- gsub("\\?", "", text)
  }
  return(text)
}

# Example usage:

df$concatenated <- sapply(df$concatenated, clean_text)


# Assuming 'df' is your data frame with 'questions' and 'tags' columns
corpus <- Corpus(VectorSource(df$concatenated))

# Preprocess the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
#corpus <- tm_map(corpus, stemDocument)


# Extract text from the corpus
corpus_text <- sapply(corpus, function(x) as.character(x))

# Create a data frame
doc_df <- data.frame(
  doc_id = seq_along(corpus_text),
  text = corpus_text,
  stringsAsFactors = FALSE
)


preprocess_and_tokenize <- function(input_text) {
  # Apply clean_text to each element of the text vector
  #cleaned_text <- sapply(input_text, clean_text)
  # Create a corpus
  corpus <- Corpus(VectorSource(input_text))
  # Preprocess the corpus
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  #Extract the preprocessed text from the corpus
  #preprocessed_text <- sapply(corpus, function(x) as.character(unlist(strsplit(as.character(x), " "))))
  corpus_text <- sapply(corpus, paste, collapse = " ")
  
  # Tokenize using tokenizers
  tokens <- unlist(tokenize_words(corpus_text))
  return(tokens)
}

## Low-dimensional model using DM, low number of iterations, for speed and display purposes
model <- paragraph2vec(x = doc_df, type = "PV-DM", dim = 15, iter = 10, lr = 0.05, threads = 1)
str(model)

embedding <- as.matrix(model)

# Example usage:
preprocess_and_tokenize <- function(input_text) {
  # Apply clean_text to each element of the text vector
  cleaned_text <- sapply(input_text, clean_text)
  # Create a corpus
  corpus <- Corpus(VectorSource(cleaned_text))
  # Preprocess the corpus
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  # Get the cleaned text from the corpus
  cleaned_text <- sapply(corpus, function(x) as.character(unlist(strsplit(as.character(x), " "))))
  # Tokenize using tokenizers
  tokens <- unlist(tokenize_words(cleaned_text))
  return(tokens)
}

#Save your model
write.paragraph2vec(model, "mymodel.bin")

#Use saved model
model <- read.paragraph2vec("mymodel.bin")

nn <- predict(model, newdata = preprocess_and_tokenize("How to run python in conda environment ?"), type = "nearest", which = "word2word", top_n = 5)

# Use Reduce to merge data frames horizontally
combined_df <- Reduce(function(x, y) merge(x, y, by = c("term1", "term2", "similarity", "rank"), all = TRUE), nn)

# Print the combined data frame
print(combined_df)

# Filter rows based on the condition
filtered_rows <- combined_df[combined_df$similarity > 0.85,c("term2","similarity")]


# Sort the tags based on similarity
sorted_tags <- filtered_rows[order(filtered_rows$similarity, decreasing = TRUE), "term2"]
print(sorted_tags)
# Get the top 5 unique tags
top_tags_out <- head(unique(sorted_tags), 20)

# Print the result
print(intersect(top_tags_out,unique_tags))


######################################################


# Assuming 'embedding' is your word embedding matrix
library(umap)

embedding_word2vec <- data.frame(embedding_word2vec)

# Assuming umap_data has more than 1000 rows
subset_size <- 200
subset_indices <- sample(1:nrow(embedding_word2vec), size = min(subset_size, nrow(embedding_word2vec)))

# Subset the data
subset_umap_data <- embedding_word2vec[subset_indices, ]

# Plot the word embeddings for the subset
plot(subset_umap_data$X1, subset_umap_data$X2, pch = 16, col = "blue", 
     main = "Word Embeddings (UMAP)", xlab = "UMAP1", ylab = "UMAP2",xlim = c(-1,1),ylim = c(-1,1))

# Annotate the points with word labels
text(subset_umap_data$X1, subset_umap_data$X2, labels = rownames(subset_umap_data), 
     pos = 3, col = "red", cex = 0.8)


############################################################



#Cleaning Punctuations
clean_text <- function(text) {
  if (is.character(text)) {
    # Pre-Processing of the text
    text <- str_replace_all(text, "can't", "cannot")
    text <- str_replace_all(text, "won't", "will not")
    text <- str_replace_all(text, "want's", "wants")
    text <- str_replace_all(text, "when'd", "when did")
    text <- str_replace_all(text, "can'tif", "cannot if")
    text <- str_replace_all(text, "y'know", "you know")
    text <- str_replace_all(text, "y'all", "you all")
    text <- str_replace_all(text, "y'think", "you think")
    text <- str_replace_all(text, "d'you", "do you")
    text <- gsub("\'s", " is", text)
    text <- gsub("\'d", " had", text)
    text <- gsub("n't", " not", text)
    text <- gsub("\'ve", " have", text)
    text <- gsub("\'ll", " will", text)
    text <- gsub("\'m", " am", text)
    text <- gsub("\'re", " are", text)
    text <- gsub("can’t", "cannot", text)
    text <- gsub("won’t", "will not", text)
    text <- gsub("want’s", "wants", text)
    text <- gsub("when’d", "when did", text)
    text <- gsub("can’tif", "cannot if", text)
    text <- gsub("y’know", "you know", text)
    text <- gsub("y’all", "you all", text)
    text <- gsub("y’think", "you think", text)
    text <- gsub("d’you", "do you", text)
    text <- gsub("\\’s", " is", text)
    text <- gsub("\\’d", " had", text)
    text <- gsub("n’t", " not", text)
    text <- gsub("\\’ve", " have", text)
    text <- gsub("\\’ll", " will", text)
    text <- gsub("\\’m", " am", text)
    text <- gsub("\\’re", " are", text)
    text <- gsub("\\’ve", " have", text)
    text <- gsub(":", " ", text)
    text <- gsub("\\?", "", text)
  }
  return(text)
}

# Example usage:

df$concatenated <- sapply(df$concatenated, clean_text)


# Assuming 'df' is your data frame with 'questions' and 'tags' columns
corpus <- Corpus(VectorSource(df$concatenated))

# Preprocess the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
#corpus <- tm_map(corpus, stemDocument)


# Extract text from the corpus
corpus_text <- sapply(corpus, function(x) as.character(x))

# Create a data frame
doc_df <- data.frame(
  doc_id = seq_along(corpus_text),
  text = corpus_text,
  stringsAsFactors = FALSE
)


data <- doc_df$text

stopwords <- read.csv(url("https://raw.githubusercontent.com/masdevid/ID-Stopwords/master/id.stopwords.02.01.2016.txt"), header = FALSE)

# making tokenizer
tokenizer <- text_tokenizer(num_words =  18000) # maximum number of word to keep (based on frequency)

# tokenize data
tokenizer %>% fit_text_tokenizer(data)

#tokenizer = unlist(tokenizer$word_index)

skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    
    list(x, y)
  }
  
}

# determine model tuning inputs
embedding_size <- 256  # dimension of embedding vector
skip_window <- 5       # number of skip-gram
num_sampled <- 1       # number of negative sample for each word

# making architecture
input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)

embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1,
  output_dim = embedding_size,
  input_length = 1, 
  name = "embedding"
)

target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten() # to return the dimension of the input

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()

dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)

output <- layer_dense(dot_product, units = 1, activation = "sigmoid")

model <- keras_model(list(input_target, input_context), output)
model %>% compile(loss = "binary_crossentropy", optimizer = "adam")

summary(model)


model %>%
  fit_generator(
    skipgrams_generator(data, tokenizer, skip_window, negative_samples),
    steps_per_epoch = 100, epochs = 30
  )

# Save model
save_model_tf(model, "stackexchangekeras.h5")


# Load model
model = keras::load_model_tf('stackexchangekeras.h5')


#obtaining word vector
embedding_matrix <- get_weights(model)[[1]]

words <- dplyr::data_frame(
  word = names(tokenizer$word_index), 
  id = as.integer(unlist(tokenizer$word_index))
)

words <- words %>%
  dplyr::filter(id <= tokenizer$num_words) %>%
  dplyr::arrange(id)

row.names(embedding_matrix) <- c("UNK", words$word)

dim(embedding_matrix)

# Save embedding_matrix
saveRDS(embedding_matrix, "embedding_matrix.rds")

find_similar_words <- function(word, embedding_matrix, n = 7) {
  similarities <- embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}

preprocess_and_tokenize <- function(input_text) {
  
  # Apply clean_text to each element of the text vector
  cleaned_text <- sapply(input_text, clean_text)
  
  # Create a corpus
  corpus <- Corpus(VectorSource(cleaned_text))
  
  # Preprocess the corpus
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  
  # Get the cleaned text from the corpus
  cleaned_text <- sapply(corpus, function(x) as.character(unlist(strsplit(as.character(x), " "))))
  
  # Tokenize using tokenizers
  tokens <- unlist(tokenize_words(cleaned_text))
  
  return(tokens)
}


find_similar_words(preprocess_and_tokenize("Cors not found in my flask application"), embedding_matrix)