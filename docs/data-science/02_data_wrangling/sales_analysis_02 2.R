# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bikeshops_tbl <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

bikes_tbl      <- read_excel(path = "data-science/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

# 3.0 Examining Data ----
bikeshops_tbl
glimpse(bikeshops_tbl)

# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% 
  separate(col = location, into = c("city", "state"), sep = ",") %>%
  mutate(revenue = price * quantity) %>%
  select(-...1, -(gender:url)) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  select(order.id, contains("order"), contains("model"), contains("city"), contains("state"), 
         price, quantity, revenue, everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

bike_orderlines_wrangled_tbl %>% glimpse()

# 6.0 Business Insights ----
# 6.1 Sales by Location ----

# Step 1 - Manipulate

sales_by_location_tbl <- bike_orderlines_wrangled_tbl %>%
  select(state, revenue) %>%
  group_by(state) %>%
  summarise(sales = sum(revenue)) %>%
  arrange(desc(sales)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",'", 
                                     prefix = "",
                                     suffix = " €"))

sales_by_location_tbl %>% glimpse()

# Step 2 - Visualize
sales_by_location_tbl %>% 
  ggplot(aes(x = state, y = sales)) +
  geom_col(fill = "#00abff") +
  geom_label(aes(label = sales_text)) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
  
theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 6.2 Sales by Location and Year ----

# Step 1 - Manipulate
library(lubridate)

sales_by_location_year_tbl <- bike_orderlines_wrangled_tbl %>%
  select(state, order_date, revenue) %>%
  mutate(year = year(order_date)) %>%
  group_by(state, year) %>%
  summarise(sales = sum(revenue)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",'", 
                                     prefix = "",
                                     suffix = " €"))

sales_by_location_year_tbl %>% glimpse()

# Step 2 - Visualize
sales_by_location_year_tbl %>% 
  ggplot(aes(x = year, y = sales, fill = state)) +
  geom_col() +
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  facet_wrap(~ state) +
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +

  labs(
    title    = "Revenue by year and state",
    subtitle = "North Rhine-Westphalia state has the highest revenue.", 
    x = "",
    fill = "States" 
  )

# 7.0 Writing Files ----
# 7.1 Excel ----
install.packages("writexl")
library(writexl)
bike_orderlines_wrangled_tbl %>%
  write_xlsx("data-science/00_data/01_bike_sales/02_wrangled_data/bike_location.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("data-science/00_data/01_bike_sales/02_wrangled_data/bike_location.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("data-science/00_data/01_bike_sales/02_wrangled_data/bike_location.rds")

