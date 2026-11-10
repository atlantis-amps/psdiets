# Separate a column with text followed by numbers
# Example: "Shrimp 33 148 167 86 50 28 19 2 533"

library(tidyr)
library(dplyr)
library(stringr)

# ============================================================================
# APPROACH 1: Simple separate_wider_delim() - for single-word species names
# ============================================================================
# Use when the species name is a single word (e.g., "Shrimp", "Fish")

separate_single_word <- function(df, col_name, num_numeric_cols = 8) {
  df %>%
    separate_wider_delim(
      {{col_name}},
      delim = " ",
      names = c("species", paste0("col_", 1:num_numeric_cols)),
      cols_remove = TRUE
    )
}

# Example usage:
# df <- tibble(item_data = c("Shrimp 33 148 167 86 50 28 19 2"))
# df_separated <- separate_single_word(df, item_data, num_numeric_cols = 8)


# ============================================================================
# APPROACH 2: Regex extraction - for multi-word species names
# ============================================================================
# Use when the species name can have multiple words (e.g., "Tiger Shrimp")

separate_multiword <- function(df, col_name, num_numeric_cols = 8) {
  df %>%
    mutate(
      # Extract everything before the first sequence of numbers
      species = str_extract({{col_name}}, "^[^0-9]+") %>% str_trim(),
      # Remove the species name and split remaining numbers
      numbers = str_remove({{col_name}}, "^[^0-9]+") %>% str_trim()
    ) %>%
    separate_wider_delim(
      numbers,
      delim = " ",
      names = paste0("value_", 1:num_numeric_cols),
      cols_remove = TRUE
    )
}

# Example usage:
# df <- tibble(item_data = c("Tiger Shrimp 33 148 167 86 50 28 19 2"))
# df_separated <- separate_multiword(df, item_data, num_numeric_cols = 8)


# ============================================================================
# APPROACH 3: separate_wider_regex() - most flexible
# ============================================================================
# Use for precise control over pattern matching

separate_with_regex <- function(df, col_name, num_numeric_cols = 8) {
  # Build the patterns dynamically
  patterns <- c(species = "[a-zA-Z ]+")
  for (i in 1:num_numeric_cols) {
    patterns[paste0("value_", i)] <- "\\s+\\d+"
  }

  df %>%
    separate_wider_regex(
      {{col_name}},
      patterns = patterns,
      cols_remove = TRUE
    )
}

# Example usage:
# df <- tibble(item_data = c("Tiger Shrimp 33 148 167 86 50 28 19 2"))
# df_separated <- separate_with_regex(df, item_data, num_numeric_cols = 8)
