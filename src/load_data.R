library("quantmod")
library("dplyr")
library("readr")

SPY.divs <- getDividends("SPY", auto.assign=FALSE)
SPY.prices <- getSymbols("SPY", auto.assign=FALSE)

SPY.div.df <- data.frame(
    date = index(SPY.divs),
    dividend = coredata(SPY.divs)[, "SPY.div"]
)
SPY.price.df <- data.frame(
    date = index(SPY.prices),
    close = coredata(SPY.prices)[, "SPY.Close"],
    volume = coredata(SPY.prices)[, "SPY.Volume"]
)

SPY.value.df <- left_join(SPY.price.df, SPY.div.df, by=("date")) %>% 
  mutate(dividend = ifelse(is.na(dividend), 0, dividend),
         # Estimate value after reinvesting dividends
         dividend_shares = dividend / close,
         shares = 1 + cumsum(dividend_shares),
         value = shares * close) %>% 
  select(date, volume, value)

write_csv(SPY.value.df, "data/spy.csv")
