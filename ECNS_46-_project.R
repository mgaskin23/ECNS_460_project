
#load necessary packages
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(ggplot2)
library(readr)
#load in the GDP data 
GDP_data <- read.csv("C:/Users/megan/OneDrive/Desktop/Classes FALL 2024/ECNS 460/GDP_data.csv")
######CLEANING AND MERGING DATA###########
#format the GDP data
GDP_data <- GDP_data[, -c(3, 4)] 
GDP_data <- GDP_data[-c(1,2,3),]
colnames(GDP_data)<- GDP_data[1,]
GDP_data <- GDP_data[-c(1),]

#subset the GDP data to only include the years 2014 to 2023
after_2014 <- GDP_data[,as.character(2014:2023)]

#add back in the country name column 
after_2014<- cbind(`country`=GDP_data$`Country Name`, after_2014)

#convert the GDP data to long format  
GDP_long <- after_2014|>
  pivot_longer(
    cols=-`country`,
    names_to = "year"
    
  )|>
  rename(GDP_change=value)|>
  na.omit()

#function to clean data
clean_string = function(x) {
  x |>
    str_to_lower() |>                      
    str_replace_all("[[:punct:]]", "") |>  
    str_replace_all("\\b(the|a|and|of|in|for)\\b", "") |>  
    str_trim() |>                          
    str_squish()                      
}

#cleaning GDP data
cleaned_GDP_data = GDP_long |>
  mutate(across(where(is.character), clean_string))|>
  distinct()
#making year an integer like in the energy data so they can be merged
cleaned_GDP_data$year <- as.integer(cleaned_GDP_data$year)


#read in energy data
energy_data <- read.csv("C:/Users/megan/OneDrive/Desktop/Classes FALL 2024/ECNS 460/renewable_energy_data.csv")
energy_data_new <- energy_data|>
  select(-id_number)

#merge the two datasets by country and year
merged_data <- inner_join(cleaned_GDP_data,energy_data_new, by= c("country", "year"))

# Add a primary key called id_number
merged_data_id <- merged_data |>
  mutate(id_number = row_number()) |>
  relocate(id_number, .before = country)

#creates regional data with average percent change in renewable energy capacity and GDP by region and year
#and total renewable energy capacity by region and average renewable capacity by region in order to better visualize the data
  region_data <- merged_data_id %>%
    group_by(region, year) %>%
    summarize(total_renewable_capacity = sum(total_re_capacity, na.rm = TRUE),
              avg_renewable_capacity = mean(total_re_capacity, na.rm=T),
              avg_percent_change_renewable = mean(re_percent_change, na.rm = TRUE),
              avg_percent_change_gdp = mean(GDP_change, na.rm = TRUE)) %>%
    ungroup()
#####VISUALIZING THE DATA#############  

  
#plot of total renewable energy capacity by region over time
ggplot(region_data, aes(x = year, y = total_renewable_capacity, color = region, group = region)) +
    geom_line(size = 1.2) +
    geom_point(size = 1.5) +  
    labs(
      title = "Total Renewable Energy Capacity by Region Over Time",
      x = "Year",
      y = "Total Renewable Energy Capacity",
      color = "Region"
    ) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))+
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )


#plot of average percent change in renewable energy capacity and GDP by region
ggplot(region_data, aes(x = year)) +
  geom_line(aes(y = avg_percent_change_renewable, color = region), size = 1.2) +
  geom_line(aes(y = avg_percent_change_gdp, color = region), linetype = "dashed", size = 1) +
  labs(
    title = "Average Percent Change in Renewable Capacity and GDP by Region",
    x = "Year",
    y = "Average Percent Change in Renewable Capacity",
    color = "Region"
  ) +
  scale_y_continuous(
    name = "Average Percent Change in Renewable Capacity",
    sec.axis = sec_axis(~ ., name = "Average Percent Change in GDP")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))

#plot of Average Percent Change in GDP BY region over time  
ggplot(region_data, aes(x = year, y = avg_percent_change_gdp, color = region, group = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +  
  labs(
    title = "Average Percent change in GDP by Region Over Time",
    x = "Year",
    y = "Average Percent change in GDP",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#plot of the average renewable energy capacity by region over time
ggplot(region_data, aes(x = year, y = avg_renewable_capacity, color = region, group = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +  
  labs(
    title = "Average Renewable energy capacity by region over time",
    x = "Year",
    y = "Average renewable energy capacity",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#plot of the average percent change in renewable energy capacity by region over time
ggplot(region_data, aes(x = year, y = avg_percent_change_renewable, color = region, group = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +  
  labs(
    title = "Average percent change in Renewable energy capacity by region over time",
    x = "Year",
    y = "Percent change in renewable energy capacity",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#plot of each region's percent change in renewable energy capacity and GDP on separate plots
ggplot(region_data, aes(x = year)) +
  geom_line(aes(y = avg_percent_change_renewable, color = "Percent change in Renewable Capacity", linetype = "Percent change in Renewable Capacity"), size = 1.2) +
  geom_line(aes(y = avg_percent_change_gdp, color = "GDP Change", linetype = "GDP Change"), size = 1) +
  labs(
    title = "Average Percent Change in Renewable Capacity and GDP by Region",
    x = "Year",
    y = "Average Percent Change in Renewable Capacity",
    color = "Variable",
    linetype = "Variable"
  ) +
  scale_y_continuous(
    name = "Average Percent Change in Renewable Capacity",
    sec.axis = sec_axis(~ ., name = "Average Percent Change in GDP")
  ) +
  scale_color_manual(values = c("Percent change in Renewable Capacity" = "blue", "GDP Change" = "red")) +
  scale_linetype_manual(values = c("Percent change in Renewable Capacity" = "solid", "GDP Change" = "dashed")) +
  facet_wrap(~ region) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
