library(tidyverse)
library(forecast)
library(lubridate)
library(Metrics)

rossmanns <- read.csv("train.csv", stringsAsFactors = F)

#need imputation approach for missing values (0s)- stores being refurbished (180 total)
#Might be good for the transformations section of project


theme_set(
  theme_minimal()
)

rossmanns %>%
  group_by(Date) %>% 
  summarise(NumStores = n()) %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(Date, NumStores)) +
  geom_line(size = 1.3, colour = "dodgerblue2") +
  scale_x_date(date_breaks = "4 months") +
  labs(title = "Missing data in Rossmanns store data- Period from 2014-07-01 to 2014-12-31 absent from set",
       subtitle = "Zero values stem from store closures for refurbishing businesses")

#all stores visualization- missing data can be seen
rossmanns %>%
  select(Date, Sales, Store) %>%
  mutate(Date = as.Date(Date),
         Store = as.character(Store)) %>%
  ggplot(aes(Date, Sales, colour = Store)) +
  geom_line(show.legend = F, alpha = .2) +
  ggtitle("All store sales for Rossmann data- missing data seen around 2015")

#one store visualization
rossmanns %>%
  filter(Store == 400) %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(Date, Sales)) +
  geom_line(size = 1.3, colour = "dodgerblue2")

#collection of 20 stores without missing values
clean_stores <- rossmanns %>%
  filter(Store %in% c(1:11, 14:19, 21, 23, 24)) %>%
  mutate(Date = as.Date(Date))

write_csv(clean_stores, "Rossmann_stores_no_missing.csv")

clean_stores %>%
  mutate(Store = as.character(Store)) %>%
  ggplot(aes(Date, Sales, colour = Store)) +
  geom_line(size = 1.3, show.legend = F) +
  facet_wrap(facets = "Store") +
  labs(title = "20 Rossmann stores sales over time period from 2013-01-01 to 2015-07-31",
       subtitle = "Stores with only complete data are included",
       x = NULL)

#random sample of 20 stores
set.seed(51)
store_sample <- sample(x = seq(min(rossmanns$Store), 
                               max(rossmanns$Store), 1), 
                       size = 20, 
                       replace = F)

twenty_stores <- rossmanns %>%
  filter(Store %in% c(store_sample)) %>%
  mutate(Date = as.Date(Date))

write_csv(twenty_stores, "Rossmann_stores_with_missing.csv")

twenty_stores %>%
  mutate(Store = as.character(Store)) %>%
  ggplot(aes(Date, Sales, colour = Store)) +
  geom_line(size = 1.3, show.legend = F) +
  facet_wrap(facets = "Store") +
  labs(title = "20 Rossmann stores sales over time period from 2013-01-01 to 2015-07-31",
       subtitle = "Set includes stores with missing data as seen by gap in Store 20",
       x = NULL)

twenty_stores %>%
  filter(Store == 20) %>%
  mutate(Store = as.character(Store)) %>%
  ggplot(aes(Date, Sales, colour = Store)) +
  geom_line(size = 1.3, colour = "dodgerblue2") +
  scale_x_date(date_breaks = "4 months") +
  labs(title = "Sales for Rossmann store 20 over time period from 2013-01-01 to 2015-07-31",
       subtitle = "Missing data included from 2014-07-01 to 2014-12-31",
       x = NULL)

twenty_stores %>%
  filter(Store != 20) %>%
  count(Store)

store_20 <- twenty_stores %>%
  filter(Store == 20)

missing_dates <- seq.Date(from = as.Date("2014-07-01"),
                          to =  as.Date("2014-12-31"), 
                          by = "day") %>%
  as.data.frame() %>%
  rename(Date = ".")

impute_values <- twenty_stores %>%
  filter(Date %in% c(missing_dates$Date)) %>%
  group_by(Date) %>%
  summarise(Sales = median(Sales),
            Customers = median(Customers),
            Open = 0,
            Promo = 0)

impute_values <- twenty_stores %>%
  filter(Date %in% c(missing_dates$Date) & duplicated(as.character(Date)) == F) %>%
  select(DayOfWeek, StateHoliday, SchoolHoliday) %>%
  bind_cols(impute_values) %>%
  mutate(Store = 20) %>%
  select(Store, DayOfWeek, Date, Sales, Customers, 
         Open, Promo, StateHoliday, SchoolHoliday)

twenty_stores <- bind_rows(twenty_stores, impute_values) %>%
  arrange(desc(Date))

write_csv(twenty_stores, "Rossman_stores_with_imputation.csv")

#check to see if number is 942- verified same number of records as other stores
twenty_stores %>%
  filter(Store == 20) %>%
  count(Store)

twenty_stores %>%
  filter(Store == 20) %>%
  mutate(Store = as.character(Store)) %>%
  ggplot(aes(Date, Sales, colour = Store)) +
  geom_line(size = 1.3, colour = "dodgerblue2") +
  scale_x_date(date_breaks = "4 months") +
  geom_vline(xintercept = as.Date("2014-07-01"), size = 1.7, colour = "darkorange") +
  geom_vline(xintercept = as.Date("2014-12-31"), size = 1.7, colour = "darkorange") +
  labs(title = "Sales for Rossmann store 20 over time period from 2013-01-01 to 2015-07-31",
       subtitle = "Missing data imputed for 2014-07-01 to 2014-12-31 (between orange lines)",
       x = NULL)

#3 forecasting methods: SARIMA, ARFIMA, GAM

#multiply by day open marker to correct for negative values
twenty_stores <- twenty_stores %>%
  mutate(DayOfWeek = as.factor(DayOfWeek),
         Month = as.factor(month(Date)))

store189_sales <- twenty_stores %>%
  filter(Store == 189) %>%
  select(Sales)

#Plot shows long memory autocorrelation- arfima may be useful  
ggAcf(x = twenty_stores$Sales) +
  labs(title = "ACF plot for all Rossman store sales",
       subtitle = "Long memory autocorrelation and seasonality are evident; Fractional and SARIMA model may be appropriate")

ggAcf(x = store189_sales$Sales) +
  labs(title = "ACF plot for Rossman store 189",
       subtitle = "Seasonality are evident; SARIMA may be appropriate")

rossman_arfima <- function(store){
  store_train <- twenty_stores %>%
    filter(Date < "2015-07-01" & Store == store) %>%
    select(Sales) %>%
    ts(data = ., frequency = 365)
  
  store_test <- twenty_stores %>%
    filter(Date >= "2015-07-01" & Store == store) %>%
    select(Sales) %>%
    ts(data = ., frequency = 365)
  
  store_open_days <- twenty_stores %>%
    filter(Date >= "2015-07-01" & Store == store) %>%
    select(Open)
  
  store_arfima <- arfima(y = store_train)
  
  arfima_forecast <- data.frame(
    store = store,
    actual = store_test %>% as.numeric(),
    forecast = forecast(store_arfima, h = length(store_test))$mean %>% as.numeric()
  ) %>%
    mutate(forecast = forecast * store_open_days$Open) %>%
    filter(forecast != 0)
  
  arfima_accuracy <- data.frame(
    store = store,
    smape = smape(actual = arfima_forecast$actual, 
                  predicted = arfima_forecast$forecast) *100
  )
  
  forecst_components <- list(
    arfima = store_arfima,
    forecast = arfima_forecast,
    smape_review = arfima_accuracy
  )
  
  return(forecst_components)
}

store_forecasts <- map(unique(twenty_stores$Store), 
                       function(x) rossman_arfima(store = x))

arfima_smape <- map_df(seq(1, length(store_forecasts), 1), 
                       function(x) store_forecasts[[x]]$smape_review)

avg_smape <- arfima_smape %>%
  summarise(arfima_avg_smape = mean(smape)) %>%
  as.numeric() %>%
  round()

map_df(seq(1, length(store_forecasts), 1), 
       function(x) store_forecasts[[x]]$forecast) %>%
  mutate(event = rep(1:27, times = 20)) %>%
  gather(key = "variable", value = "value", -store, -event) %>%
  ggplot(aes(event, value, colour = variable)) +
  geom_line(size = 1.3) +
  facet_wrap(facets = "store", scales = "free") +
  scale_x_continuous(breaks = seq(0, 27, 3)) +
  scale_colour_manual(values = c("dodgerblue2", "darkorange")) +
  labs(title = "ARFIMA forecasts for all Rossman stores",
       subtitle = paste("Method shows varied fit by store: Avg. smape =" , avg_smape),
       y = "Sales")
