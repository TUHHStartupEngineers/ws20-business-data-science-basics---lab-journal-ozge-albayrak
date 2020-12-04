# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----

bikes_tbl      <- read_excel(path = "data-science/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

bikeshops_tbl <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
# 3.0 Examining Data ----
orderlines_tbl
glimpse(orderlines_tbl)

# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% 
  separate(col = category, into = c("category.1", "category.2", "category.3"), sep = "-") %>%
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  select(order.id, contains("order"), contains("model"), contains("category"), 
         price, quantity, total.price, everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

bike_orderlines_wrangled_tbl %>% glimpse()

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate
library(lubridate)

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price) %>% 
  mutate(year = year(order_date)) %>%
  group_by(year) %>% 
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",'", 
                                     prefix = "",
                                     suffix = " €"))

sales_by_year_tbl %>% glimpse() 

# Step 2 - Visualize
sales_by_year_tbl %>% 
  ggplot(aes(x = year, y = sales)) +
  geom_col(fill = "#00abff") +
  geom_label(aes(label = sales_text)) +
  geom_smooth(method = "lm", se = FALSE) +
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price, category_2) %>%
  mutate(year = year(order_date)) %>%
  group_by(year, category_2) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",'", 
                                     prefix = "",
                                     suffix = " €"))
  
sales_by_year_cat_2_tbl %>% glimpse()

# Step 2 - Visualize
sales_by_year_cat_2_tbl %>%
  ggplot(aes(x = year, y = sales, fill = category_2)) +
  geom_col() +
  
  facet_wrap(~ category_2) +
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  
  labs(
    title    = "Revenue by year and category 2",
    subtitle = "Each product category has an Upward Trend",
    fill = "Main Category" 
  )

# 7.0 Writing Files ----

# 7.1 Excel ----
install.packages("writexl")
library(writexl)
bike_orderlines_wrangled_tbl %>%
  write_xlsx("data-science/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")


# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("data-science/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>%
  write_rds("data-science/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

