#'@title Quick Look at a Company's Financials
#'@description This function will allow a use to look at several ratios of the company
#'@export
#'@param company_name Write in company name in quotes
#'@returns The stock ticker for the company if it is a public stock on NASDAQ or NYSE
#'@import tidyedgar
#'@import dplyr
#'@import readxl
#'@examples
#'

library(dplyr)
# import the cik and ticker data set
library(readxl)
cikticker <- read_excel("~/Desktop/cikticker.xlsx",
                        col_types = c("text", "numeric"))

# For the package we will have to have cikticker dataframe apart of package

filter_company_data <- function(tik) {
  # Define the dataframe inside the function
  df <- yearly_data(years = 2015:2023)

  # Merge df with cikticker by the cik number
  merged_df <- merge(df, cikticker, by = "data.cik", all = FALSE)

  # Add the ratios to the dataframe using mutate
  merged_df <- merged_df %>%
    mutate(
      gross_profit_margin = (GrossProfit / revenue) * 100,
      return_on_sales = (net_income / GrossProfit) * 100,
      gross_profit_to_net_income_ratio = (GrossProfit / net_income) * 100,
      Ticker = toupper(Ticker)
    )

  # Check if df has been populated correctly
  if (nrow(merged_df) == 0) {
    stop("No data found for the specified years.")
  }

  # Filter data by Ticker symbol
  result <- merged_df %>% filter(Ticker == tik)

  # If no rows are returned after filtering
  if (nrow(result) == 0) {
    stop("No data found for the specified Ticker.")
  }

  # Order the data by the year column from smallest to largest
  result <- result %>% arrange(year)

  # Return the result where year is in ascending order
  return(result)
}


company_data <- filter_company_data(tik = "AAPL")



# visualization that allows user to see how a given ratio changes over time for a specified company
library(ggplot2)

# Function to generate a plot based on the saved output dataframe
plot_financial_ratio <- function(data, ratio_name) {
  # Check if the specified ratio exists in the dataframe
  if (!ratio_name %in% colnames(data)) {
    stop("Invalid ratio name provided.")
  }

  # Create the plot
  ggplot(data, aes(x = as.factor(year), y = .data[[ratio_name]])) +
    geom_line() +
    geom_point() +
    labs(
      title = paste("Financial Ratio -", ratio_name, "for", unique(data$Ticker)),
      x = "Year",
      y = ratio_name
    ) +
    theme_minimal()
}

# Example usage:
# Assuming 'object' is the result of filter_company_data function
# plot for 'gross_profit_margin'
plot_financial_ratio(company_data, "gross_profit_margin")
