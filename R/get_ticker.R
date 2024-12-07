#'@title Finding a Company's Ticker
#'@description Every company that has public stock has a ticker. This function will allow a user to plug in the company name and be able to find the ticker of it.
#'@export
#'@param company_name Write in company name in quotes
#'@returns The stock ticker for the company if it is a public stock on NASDAQ or NYSE
#'@import tidyquant
#'@import quantmod
#'@import dplyr
#'@import ggplot2
#'@import plotly
#'@import shiny
#'@examples
#'get_ticker("Par Technology")


get_ticker <- function(company_name) {
  # Retrieve stock listings for NYSE and NASDAQ
  nyse_stocks <- tq_exchange("NYSE")
  nasdaq_stocks <- tq_exchange("NASDAQ")

  # Combine NYSE and NASDAQ stocks
  all_stocks <- bind_rows(nyse_stocks, nasdaq_stocks)

  # Search for the company name in the combined list
  matching_stocks <- all_stocks %>%
    filter(grepl(company_name, company, ignore.case = TRUE))

  # Return results
  if (nrow(matching_stocks) == 0) {
    cat("Cannot find the ticker for:", company_name, "\n")
  } else {
    # Extract the symbol, convert to lowercase, and output the result
    symbol <- matching_stocks$symbol[1]
    cat((symbol))
  }
}
