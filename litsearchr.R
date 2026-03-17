install.packages("synthesisr")
library(litsearchr)
library(synthesisr)
library(dplyr)

# 2. Import your database results (Handled by synthesisr)
# read_refs replaces import_results. It reads all supported files in your directory.
my_files <- list.files("D:/College/Sys results", full.names = TRUE)
search_data <- read_refs(filename = my_files)

# 3. Deduplicate the results (Handled by synthesisr)
# The old title_upper/abstract_upper arguments are deprecated. 
# We now specify the column to match by and the matching method (fuzzy string matching).
dedup_data <- deduplicate(
  data = search_data, 
  match_by = "title", 
  method = "string_osa"
) 

# 4. Extract terms using fakerake (Handled by litsearchr)
rake_terms <- extract_terms(
  text = paste(dedup_data$title, dedup_data$abstract, sep = " "),
  method = "fakerake", 
  min_freq = 2, 
  ngrams = TRUE,        # Added from vignette
  min_n = 2,
  language = "English"  # Added from vignette
)

# Extract database-tagged keywords
tagged_terms <- extract_terms(
  keywords = dedup_data$keywords, 
  method = "tagged"
)

# 5. Combine into a Dictionary
# DEPRECATED: my_dictionary <- create_dictionary(rake_terms, tagged_terms)
# NEW VIGNETTE METHOD: Combine them into a simple vector of unique terms
all_keywords <- unique(append(tagged_terms, rake_terms))

# 6. Create Document-Feature Matrix and Keyword Co-occurrence Network
docs <- paste(dedup_data$title, dedup_data$abstract, sep = " ")

# Use the new 'all_keywords' vector as your features
dfm <- create_dfm(elements = docs, features = all_keywords) 

# Added min_occ = 2 from the vignette to improve network accuracy
network <- create_network(search_dfm = dfm, min_studies = 2, min_occ = 2) 

# 7. Find the cutoff for keyword importance 
# The vignette explicitly shifts away from method="knot" to method="cumulative"
cutoffs <- find_cutoff(
  network, 
  method = "cumulative", 
  percent = 0.80, 
  imp_method = "strength"
) 

suggested_keywords <- get_keywords(reduce_graph(network, cutoff_strength = cutoffs[1])) 

# 8. MANUALLY REVIEW KEYWORDS
approved_keywords <- c("pancreatic cancer", "immune checkpoint inhibitor") 

# 9. Expand terms via shared stems
# Note: get_similar no longer needs a 'dictionary' object, just your list of keywords.
similar_terms <- get_similar(all_keywords, terms = approved_keywords)

# 10. Group Concepts and Write Final Search
concept_1_disease <- c("pancreatic cancer", "pancreatic ductal adenocarcinoma", "pdac")
concept_2_ici <- c("immune checkpoint inhibitor", "pembrolizumab", "nivolumab")

my_concepts <- list(concept_1_disease, concept_2_ici)

final_search_string <- write_search(
  groupdata = my_concepts,
  languages = "English", 
  exactphrase = TRUE, 
  stemming = TRUE 
)

cat(final_search_string)