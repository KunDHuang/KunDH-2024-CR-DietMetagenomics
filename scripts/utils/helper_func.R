# Define the function to convert repeated strings to numbers
convert_strings_to_numbers <- function(df, column_name) {
  # Extract the column as a factor and assign numeric values to its levels
  df[[column_name]] <- as.numeric(factor(df[[column_name]]))
  
  # Return the modified dataframe
  return(df)
}

# Define the function to convert all string columns to numbers
convert_all_strings_to_numbers <- function(df) {
  # Loop through each column in the dataframe
  for (col_name in colnames(df)) {
    # Check if the column is of type character or factor
    if (is.character(df[[col_name]]) || is.factor(df[[col_name]])) {
      # Convert the column to factor and then to numeric
      df[[col_name]] <- as.numeric(factor(df[[col_name]]))
    }
  }
  # Return the modified dataframe
  return(df)
}