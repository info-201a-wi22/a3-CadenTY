library(tidyverse)
library(tibble)
data_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
jail_jurisdiction <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

avg_tjp <- mean(data_trends$total_jail_pop, na.rm = TRUE)
avg_bjp <- mean(data_trends$black_jail_pop, na.rm = TRUE)  
#These lines of code find the mean of the total jail pop and black jail pop columns while also dropping all NA values
highest_jpt_county <- data_trends %>%
  select(black_jail_pop, county_name) %>%
  drop_na() %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(county_name)

#These lines find the highest value within the jail pre-trial column and figure out which county has that value
bjpr1 <- data_trends %>%
  select(black_jail_pop_rate, year, county_name) %>%
  drop_na() %>%
  filter(year == "2000") %>%
  filter(county_name == "Los Angeles County") %>%
  pull(black_jail_pop_rate)

bjpr2 <- data_trends %>%
  select(black_jail_pop_rate, year, county_name) %>%
  drop_na() %>%
  filter(year == "2005") %>%
  filter(county_name == "Los Angeles County") %>%
  pull(black_jail_pop_rate)
 # These lines of code find the value of black jail population rate in a specific year and location
bjpr_diff <- bjpr2 - bjpr1
#this line finds the difference between the two variables above
wjpr1 <- data_trends %>%
  select(white_jail_pop_rate, year, county_name) %>%
  drop_na() %>%
  filter(year == "2000") %>%
  filter(county_name == "Los Angeles County") %>%
  pull(white_jail_pop_rate)

wjpr2 <- data_trends %>%
  select(white_jail_pop_rate, year, county_name) %>%
  drop_na() %>%
  filter(year == "2005") %>%
  filter(county_name == "Los Angeles County") %>%
  pull(white_jail_pop_rate)

wjpr_diff <- wjpr2 - wjpr1


#Code for Over time chart
library(tidyverse)

data_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#tot_chart <- data_trends %>%
  #select(year, black_jail_pop, white_jail_pop, latinx_jail_pop, total_jail_pop) %>%
  #drop_na() %>%
  #group_by(year) %>%
  #summarise(black_jail_pop = mean(black_jail_pop), white_jail_pop = mean(white_jail_pop), latinx_jail_pop = mean(latinx_jail_pop), total_jail_pop = mean(total_jail_pop))


#new_tot_chart <- tot_chart

data_trends %>%
  ggplot()+
  geom_line(mapping = aes(x = year, y = total_jail_pop, color = "blue"))+
  geom_point(mapping = aes(x = year, y = total_jail_pop, color = "blue"))+
  geom_line(mapping = aes(x = year, y = black_jail_pop, color = "red"))+
  geom_point(mapping = aes(x = year, y = black_jail_pop, color = "red"))+
  geom_line(mapping = aes(x = year, y = white_jail_pop, color = "green"))+
  geom_point(mapping = aes(x = year, y = white_jail_pop, color = "green"))+
  geom_line(mapping = aes(x = year, y = latinx_jail_pop, color = "yellow"))+  
  geom_point(mapping = aes(x = year, y = latinx_jail_pop, color = "yellow")) +
  labs(x = "Year", y = "TJP (blue), BJP (green), WJP (Red), LJP (Yellow)", title = "Different Race Jail Population In Proportion to Total Jail Population") 

#This code first isolates the data set to show specific columns I want to use
#then it creates multiple lines on a line chart in order to show the values of each 
#column/variable in relation to the total population value

#Code for Comparison chart
library(tidyverse)
data_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
jail_jurisdiction <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

view(data_trends)
vc_chart <- data_trends %>%
  select(county_name, black_jail_pop_rate, white_jail_pop_rate) %>%
  drop_na() %>%
  filter(county_name == "Los Angeles County")

vc_chart %>%
  ggplot(aes(black_jail_pop_rate,white_jail_pop_rate)) +
  geom_point() +
  geom_smooth(method = lm,
              se = F)+
  labs(x = "Population rate of Black Individuals",
       y = "Population rate of White Individuals",
       title = "Population Rate Comparison") +
  theme_minimal()

#this code filters the data set to first isolate specific columns
#then it creats a geom_point or scatterplot to show the difference between
#the two variables, also has a trend line to show the general direction.

#map code
library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)
data_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
jail_jurisdiction <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

counties_modified <- data_trends %>%
  filter(year == max(year))

county_map <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_map %>%
  left_join(counties_modified, by = "fips")
  

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

new_column <- map_data
new_column$blackpopulationdiff <- new_column$black_pop_15to64 / new_column$black_jail_pop

view(new_column)
  
incarceration_map <- ggplot(new_column) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = blackpopulationdiff),
    color = "gray", size = 0.3
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(new_column$blackpopulationdiff)), na.value = "white", low = "yellow", high = "red") +
  blank_theme + 
  labs(title = "Difference Between Black Individuals in Jail and Ages 15 to 64")



