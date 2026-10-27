library(tidyquant)
library(tidyverse)
nifty_url="https://archives.nseindia.com/content/indices/ind_nifty50list.csv"
nifty50data=read_csv(nifty_url)
nifty_tickers=nifty50data %>%
    mutate(symbol=paste0(Symbol,".NS")) %>%
    select(symbol, company='Company Name',sector='Industry')
niftyprices=tq_get(nifty_tickers$symbol,
                   get = "stock.prices",
                   from = "2023-01-01",
                   to = Sys.Date())

final_dataset = niftyprices %>% left_join(nifty_tickers, by = "symbol")
engineered_data = final_dataset %>%
  group_by(symbol) %>%
  arrange(date) %>%
  fill(adjusted, .direction = "down") %>%
  mutate(

    SMA_50 = SMA(adjusted, n = 50),
    SMA_200 = SMA(adjusted, n = 200),
    

    Daily_Return = (adjusted / lag(adjusted)) - 1,
 
    Rolling_Vol = runSD(Daily_Return, n = 20) * sqrt(252), 
    Dip_Opportunity = ifelse(adjusted < (SMA_50 * 0.95), 1, 0)
  ) %>%
  drop_na() %>%
 ungroup()
head(engineered_data)
library(forecast)
  forecast_stock=function(ticker_symbol,dataset)
  {
    single_stock= dataset%>%
      filter(symbol==ticker_symbol)%>%
      select(date,adjusted)
    p=as.numeric(single_stock$adjusted)
    d=ymd(single_stock$date)
    aq=xts(p,order.by=d)
    
    tx=auto.arima(aq)
     future= forecast(tx,h=10)
     forecast_table=as.data.frame(future)%>%
       mutate(
         symbol=ticker_symbol,
         Forecast_day=1:10
       )
     return(forecast_table)
  }
all_tickers=unique(engineered_data$symbol)
all_forecast_list=list()
for(ticker in all_tickers)
{
  current_forecast=forecast_stock(ticker,engineered_data)
  all_forecast_list[[ticker]]=current_forecast
}
final_master_forecast=bind_rows(all_forecast_list)
write_csv(engineered_data, "Nifty50_Engineered_History.csv")
write_csv(final_master_forecast,"Nifty50_ARIMA_Forecasts.csv")
