#Thomas' stuff
library(googlesheets4)
library(tidyverse)
library(urbnmapr)

#Read google sheets data into R
cwdpositive <- read_sheet('1bazVSrrfMWM6scjJzAvDXBw-EeX2BpK1BI1FRdx5GCk') %>% 
  rename(permitarea=`Permit\nArea`, sampleacquisition=`Sample\nAcquisition`) %>% 
  mutate(permitarea=factor(permitarea), Year = as.integer(Year))


distributionofcwdinunitedstates <- read_sheet('https://docs.google.com/spreadsheets/d/1AJFJeUI25nDAGxogKNd7oA70r1roLoU92hv9408OR6Q/edit#gid=0')

minnesotayearlysamplingdata <- read_sheet  ('https://docs.google.com/spreadsheets/d/1ABj6j_GxRa1dpEzdP5ipZHaDyJgaZrYWtITForcFfW4/edit#gid=0')                                            

#Aaron's data stuff
usa_cwd <- read_sheet("https://docs.google.com/spreadsheets/d/18nwJNkR9APotdyceGLIBKdA8ktHZqGjZSj1FsKNT7KY/edit#gid=0")

#Data for captive cases by county for national map
captivecountydata <- read_sheet('https://docs.google.com/spreadsheets/d/1Bqnrw1BbulGervJO2zODaUC6wX49SRcCu2zdQ57sNXw/edit#gid=0')

#Data for wild cases by county for national map
wildcountydata <- read_sheet('https://docs.google.com/spreadsheets/d/1kZsk7IcZDokeosX1HKxCdFl-Dp7uQa4y5KmcNyPrIoI/edit#gid=0')


#Figure 1
#MN Yearly Cases by Permit Area
cwdpositive %>%
  count(Year, permitarea) %>% 
  ggplot() + 
  geom_col(mapping = aes(x = Year, y = n, fill = permitarea))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Permit Area", fill = "Permit Area")

#Figure 2
#Total samples collected, positive cases collected, positivity rate
minnesotayearlysamplingdata %>% 
  separate(Year, into = c("start year", "Year"), sep = "-") %>% 
  group_by(Year) %>% 
  summarise(Positive = sum(Positive), Samples = sum(Samples)) %>% 
  mutate(`Positivity Rate`=Positive/Samples) %>% 
  pivot_longer(cols = c(Samples, Positive, `Positivity Rate`), 
               names_to = "Variable", values_to = "Value") %>% 
  mutate(Variable=as_factor(Variable)) %>% 
  ggplot(aes(x=Year, y=Value, fill = Variable))+
  geom_col()+
  facet_wrap(~Variable, scales = "free_y")+
  guides(fill ="none")+
  labs(x=NULL, y=NULL, title = "Total Samples, Positive Cases, and Positivity Rate")

#Figure 3
#This works Dots are small
ggplot(data = minnesotayearlysamplingdata) + 
  geom_point(mapping = aes(x = Year, y = Positive, color = `Zones Tested`))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y="Positive Cases", title = "MN Yearly Cases by Sample Collection Zones", fill = "Permit Area", size = NULL)

#Figure 4
#This works MN Yearly Cases by Age
ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = Age))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Age")

#Figure 5
#This works MN Yearly Cases by Sex
ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = Sex))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Sex")

#Figure 6
#Bar Chart sample acquisition
ggplot(data = cwdpositive) + 
  geom_bar(mapping = aes(x = Year, fill = sampleacquisition))+
  labs(y="Positive Cases", title = "MN Yearly Cases by Sample Acquisition Method", fill = "Sample Acquisition", size = NULL)

#Figure 7
# Bar chart of total_collected_samples vs year
ggplot(data = usa_cwd,
       mapping = aes(x = year)) +
  geom_col(mapping = aes(y = total_collected_samples), fill = "#a6192e")+
  labs(title = "Total Collected Samples per Year", y = "Total Collected Samples", x = "Year") 

#Figure 8
# Bar chart showing state vs total_positive_samples vs states
usa_cwd %>%
  group_by(state) %>% 
  summarise(total_positive_samples = sum(total_positive_samples)) %>% 
  arrange(desc(total_positive_samples)) %>% 
  mutate(state = as_factor(state)) %>% 
  ggplot(mapping = aes(x = state)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e") +
  theme_gray(base_size =18 )+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Total Positive Samples", x = NULL)

#Figure 9
# Bar chart of total_positive_samples vs year
ggplot(data = usa_cwd,
       mapping = aes(x = year)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e") +
  labs(y = "Total Positive Samples", title = "Total Positive Samples Per Year", x = "Year")

#Figure 10
# Bar chart showing species vs total_positive_samples
ggplot(data = usa_cwd,
       mapping = aes(x = species)) +
  geom_col(mapping = aes(y = total_positive_samples), fill = "#a6192e")+
  labs(y = "Total Positive Samples", title = "Total Positive Samples Per Year Per Species", x = "Year")

#Figure 11
# Bar chart showing species vs total_collected_samples
ggplot(data = usa_cwd,
       mapping = aes(x = species)) +
  geom_col(mapping = aes(y = total_collected_samples), fill = "#a6192e")+
  labs(title = "Total Collected Samples per Year", y = "Total Collected Samples", x = "Year")

#Figure 12
#Map of States infected Free Ranging
states %>% 
  left_join(distributionofcwdinunitedstates, by = c("state_name"="State")) %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = `Free-ranging cervids`)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Infections?",
       title="Wild Cervid Infections")+
  theme_grey(base_size = 18)

#Figure 13
#Map of captive cervids by state
states %>% 
  left_join(distributionofcwdinunitedstates, by = c("state_name"="State")) %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = `Captive cervids`)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Infections?",
       title="Captive Cervid Infections")+
  theme_grey(base_size = 18)

#Figure 14
#Wild County Data, works, just need correct size.
countydata <- counties %>%
  left_join(wildcountydata %>%
              mutate(Wild=1, County=paste0(County," County")), 
            by=c("state_name"="State", "county_name"="County"))
ggplot(countydata, mapping = aes(long, lat, group = group, fill = Wild)) +
  geom_polygon(aes(color = Wild)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill="none")+
  labs(fill = "Wild Infections", color = "Wild Infections",
       title="Wild Cervid Infections by County")

#Figure 15
#Counties Captive, works
captive <- counties %>%
  left_join(captivecountydata %>%
              mutate( County=paste0(County," County")), 
            by=c("state_name"="State", "county_name"="County"))
ggplot(captive, mapping = aes(long, lat, group = group, fill = Count)) +
  geom_polygon(aes(color=Count)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Captive Infections", color="Captive Infections",
       title="Captive Cervid Infections by County")

#Mean
statesmean <- usa_cwd %>%
  group_by(state) %>% 
  summarise(total_positive_samples = sum(total_positive_samples)) %>%
  summarise(state_mean =mean(total_positive_samples)) %>% 
  pull(state_mean)


