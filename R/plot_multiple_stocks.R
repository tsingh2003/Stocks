#'@title Plot Multiple Stocks on One Plot
#'@description Allows user to see multiple stocks on one plot
#'@export
#'@param tickers The stock tickers that will be plotted
#'@param from_date The starting date of the plot, has to be done as MM/DD/YYYY
#'@param to_date The ending date of the plot, has to be done as MM/DD/YYYY
#'@returns One plot will be generated with stocks prices of selected stocks over a certain amount of time specified. One table will be generated giving the percent increase or descrease of the stock over the specified amount of time.
#'@import tidyquant
#'@import ggplot2
#'@import plotly
#'@import lubridate
#'@import dplyr
#'@import knitr
#'@import scales
#'@examples
#'plot_multiple_stocks(c("PAR", "MSFT", "BA", "AAPL"), from_date = "01/05/2023", to_date = "01/01/2024")



# Function to get the last valid market day before a given date
get_last_valid_trading_day <- function(to_date) {
  while (weekdays(to_date) %in% c("Saturday", "Sunday") || !is_trade_day(to_date)) {
    to_date <- to_date - 1
  }
  return(to_date)
}

# Check if a given date is a valid trading day
is_trade_day <- function(date) {
  # Assuming the market is closed on weekends and specific holidays
  # You could extend this list with more specific holiday logic.
  holidays <- as.Date(c("2023-01-01", "2023-12-25")) # Add more holidays if needed
  !weekdays(date) %in% c("Saturday", "Sunday") && !(date %in% holidays)
}

plot_multiple_stocks <- function(tickers, from_date = NULL, to_date = NULL) {

  # If from_date or to_date is provided as numeric, convert it to Date format
  if (!is.null(from_date)) {
    # Handle date format, either passed as a string or numeric value
    if (is.numeric(from_date)) {
      from_date <- as.Date(as.character(from_date), format="%m/%d/%Y")
    } else {
      from_date <- mdy(from_date)  # Convert from MM/DD/YYYY to Date
    }
  } else {
    from_date <- Sys.Date() - 365  # Default to one year ago if not specified
  }

  if (!is.null(to_date)) {
    if (is.numeric(to_date)) {
      to_date <- as.Date(as.character(to_date), format="%m/%d/%Y")
    } else {
      to_date <- mdy(to_date)  # Convert from MM/DD/YYYY to Date
    }
  } else {
    to_date <- Sys.Date()  # Default to today if not specified
  }

  # Adjust the to_date to ensure it's the last valid trading day
  to_date <- get_last_valid_trading_day(to_date)

  # Fetch stock data with error handling and suppress warnings
  stock_data <- purrr::map_dfr(tickers, function(ticker) {
    suppressWarnings(
      tryCatch({
        data <- tq_get(ticker, from = from_date, to = to_date, get = "stock.prices")
        if (nrow(data) > 0) {
          mutate(data, symbol = ticker)
        } else {
          stop("No data returned.")
        }
      }, error = function(e) {
        message(paste0("Cannot gather \"", ticker, "\" data because Yahoo Finance is down. Please try again later."))
        NULL
      })
    )
  })

  # If no data was fetched, stop the function
  if (nrow(stock_data) == 0) {
    stop("No valid stock data available. Please check the ticker symbols or try again later.")
  }

  # Calculate the percentage change for each stock
  percentage_changes <- stock_data %>%
    group_by(symbol) %>%
    summarize(
      start_date = first(date),
      end_date = last(date),
      start_price = first(adjusted),
      end_price = last(adjusted),
      pct_change = (last(adjusted) - first(adjusted)) / first(adjusted) * 100
    ) %>%
    mutate(
      pct_change = scales::percent(pct_change / 100, accuracy = 0.01)  # Format pct_change with percentage sign
    ) %>%
    arrange(desc(pct_change))  # Optional: arrange by pct_change for better readability

  # Print percentage changes with custom formatting
  cat("\nPercentage Change for each stock:\n")
  print(kable(percentage_changes %>%
                select(symbol, start_date, end_date, pct_change),
              format = "pipe",
              digits = 2))

  # Plot the stock data
  p <- ggplot(stock_data, aes(x = date, y = adjusted, color = symbol)) +
    geom_line() +
    labs(title = "Stock Price Comparison", y = "Adjusted Close", x = "Date") +
    theme_minimal()

  # Make the plot interactive with plotly
  ggplotly(p)
}

