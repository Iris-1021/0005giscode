library(sf)
library(dplyr)
library(countrycode)
library(jsonlite)
library(tidyr)

#collect data
world <- st_read("/Users/fengxinyi/Desktop/USS/0005/0005giscode/Week 4/World_Countries_(Generalized).geojson")
gender_data <- fromJSON("/Users/fengxinyi/Desktop/USS/0005/0005giscode//Week 4/hdr-data.json", flatten = TRUE)


# 筛选出2010年和2019年的数据
gender_inequality_filtered <- gender_data %>%
  filter(year %in% c(2010, 2019), indexCode == "GII") %>%  # 假设GII代表Gender Inequality Index
  select(countryIsoCode, year, value)

# 转为宽格式以便计算差值
gender_inequality_wide <- gender_inequality_filtered %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "GII_")

# 检查哪些国家和年份存在重复项
duplicates <- gender_inequality_filtered %>%
  group_by(countryIsoCode, year) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# 使用mean函数来处理重复值
gender_inequality_wide <- gender_inequality_filtered %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "GII_", values_fn = mean)

# 计算2010年到2019年的不平等差值
gender_inequality_wide <- gender_inequality_wide %>%
  mutate(GII_diff = GII_2019 - GII_2010)

# 转换ISO代码以便匹配
gender_inequality_wide <- gender_inequality_wide %>%
  mutate(ISO3C = countrycode(countryIsoCode, origin = "iso3c", destination = "iso3c"))

world_data<- world %>%
  mutate(ISO3C = countrycode(ISO, origin = "iso2c", destination = "iso3c"))


# 移除无法匹配的非国家代码
gender_inequality_wide <- gender_inequality_wide %>%
  filter(!countryIsoCode %in% c("ZZA.VHHD", "ZZB.HHD", "ZZC.MHD", "ZZD.LHD", "ZZE.AS", 
                                "ZZF.EAP", "ZZG.ECA", "ZZH.LAC", "ZZI.SA", "ZZJ.SSA", 
                                "ZZK.WORLD")) %>%
  mutate(ISO3C = countrycode(countryIsoCode, origin = "iso3c", destination = "iso3c"))



# 将性别不平等数据与地理数据合并
world_data_with_gii <- world_data %>%
  left_join(gender_inequality_wide, by = "ISO3C")


# 使用不同的文件名写入
st_write(world_data_with_gii, "World_Gender_Inequality_2010_2019_prac4.geojson")

library(sf)

# 用 0 替换 GII_diff 中的 NA 值
world_data_with_gii <- world_data_with_gii %>%
  mutate(GII_diff = replace_na(GII_diff, 0))


library(ggplot2)

ggplot(world_data_with_gii) +
  geom_sf(aes(fill = GII_diff)) +
  scale_fill_gradient2(low = "green", mid = "white", high = "blue", midpoint = 0, 
                       name = "Gender Inequality Difference") +
  labs(
    title = "Gender Inequality Index Difference (2010 - 2019)",
    subtitle = "Positive values indicate increased inequality, negative values indicate improvement"
  ) +
  theme_minimal()


