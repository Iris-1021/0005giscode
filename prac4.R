library(sf)
library(dplyr)
library(countrycode)
library(jsonlite)

#collect data
world <- st_read("/Users/fengxinyi/Desktop/USS/0005/GIS/WK4/Homework/World_Countries_(Generalized).geojson")
gender_data <- fromJSON("/Users/fengxinyi/Desktop/USS/0005/GIS/WK4/Homework/hdr-data.json", flatten = TRUE)

gender_2010 <- gender_data %>% filter(year == 2010) %>% select(country, index)
gender_2019 <- gender_data %>% filter(year == 2019) %>% select(country, index)

gender_2010 <- gender_2010 %>% distinct(country, .keep_all = TRUE)
gender_2019 <- gender_2019 %>% distinct(country, .keep_all = TRUE)

# 根据国家名称将两个年份的数据合并
gender_diff <- left_join(gender_2019, gender_2010, by = "country", suffix = c("_2019", "_2010"))

colnames(gender_diff)

str(gender_diff$index_2019)
str(gender_diff$index_2010)

#除去na值
gender_diff <- gender_diff %>%
  filter(index_2019 != "Gender Inequality Index" & index_2010 != "Gender Inequality Index") %>%
  mutate(index_2019 = as.numeric(index_2019),
         index_2010 = as.numeric(index_2010))

#计算不平等差异
gender_diff <- gender_diff %>% 
  mutate(inequality_diff = index_2019 - index_2010)

gender_diff$country_name <- as.character(gender_diff$country_name)

gender_diff$iso_a3 <- countrycode(gender_diff$country_name, "country.name", "iso3c")

colnames(world)

world <- world %>%
  mutate(ISO_A3 = countrycode(COUNTRY, "country.name", "iso3c"))
world_data <- left_join(world, gender_diff, by = c("ISO_A3" = "iso_a3"))

library(dplyr)

world_data <- world_data %>%
  select(-duplicate_field_name)


st_write(world_data, "World_Gender_Inequality_Difference.geojson", driver = "GeoJSON")




