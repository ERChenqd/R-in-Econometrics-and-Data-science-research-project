rm(list=ls())

library(readxl)
library (dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(psych)

###1.Data pre-processing
#1.1. Read, combine and format data
#presidential election data
pres_elec <- read_excel("C:/Users/24333/Downloads/1976-2020-US election votes.xlsx", sheet = '1976-2020-president')
pres_elec <- pres_elec %>%
  filter(!party_simplified %in% c("OTHER", "LIBERTARIAN", "DEMOCRAT")) 
pres_elec <- pres_elec %>%
  mutate(voteshare = candidatevotes / totalvotes)

pres_elec <- pres_elec %>%
  group_by(state) %>%  # Group by state to compare within the same state
  mutate(
    prev_voteshare = lag(voteshare, 1), 
    voteshare_change = voteshare - prev_voteshare  
  ) %>%
  ungroup()  

View(pres_elec)
pres_elec <- pres_elec |>
  select(-state_fips, -state_cen,-state_ic,-office,-candidate,-writein,-version,-notes,-party_detailed )

#median household income data
median_hhincome <- read_excel("C:/Users/24333/Downloads/income_by_state.xlsx")

#merge into one dataset
merged_data <- pres_elec %>%
  left_join(median_hhincome, by = c("year", "state"))

#introduce percentage change in median household income
merged_data <- merged_data %>%
  group_by(state) %>%  
  mutate(
    prev_income = lag(`Median Household Income`, 1),  
    income_change_pct = ((`Median Household Income` - prev_income) / prev_income) * 100
  ) %>%
  ungroup()  # Remove grouping

merged_data <- merged_data %>%
  rename(Medianincome = `Median Household Income`)

#introduce absolute values
merged_data$abs_voteshare_change <- abs(merged_data$voteshare_change)
merged_data$abs_income_change_pct <- abs(merged_data$income_change_pct)

View (merged_data)

#1.2. Handle the NA values
#Replace the NA values with the value in t+1 and averages
fill_na_with_next_or_zero <- function(x) {
  next_val <- dplyr::lead(x)
  is_na <- is.na(x)
  x_mean <- mean(x, na.rm = TRUE)
  x[is_na] <- ifelse(is.na(next_val[is_na]), x_mean, next_val[is_na])
  return(x)
}
# Columns to be processed
cols_to_fix <- c(
  'prev_voteshare','voteshare_change',
  'Medianincome','prev_income','income_change_pct',
  'abs_income_change_pct','abs_voteshare_change')

# Loop over column names and modify in-place
for (col in cols_to_fix) {
  merged_data[[col]] <- fill_na_with_next_or_zero(merged_data[[col]])
}

#1.3. Handle the duplicates in data
df=distinct(merged_data)
# Identify duplicate state-year entries in election data
duplicates=df %>%
  group_by(year, state) %>%
  filter(n() > 1) %>%
  ungroup()
df=df[-252, ]

#Introduce Race data
Ethnic <- read_excel("C:/Users/24333/Downloads/race_data.xlsx",sheet='Sheet1')
colnames(Ethnic)=c("Label","population1981","population1982","population1983","population1984","population1985","population1986","population1987","population1988","population1989",
                   "population1990","population1991","population1992","population1993","population1994","population1995","population1996","population1997","population1998","population1999",
                   "population2000","population2001","population2002","population2003","population2004","population2005","population2006","population2007","population2008",
                   "population2009","population2010","population2011","population2012","population2013","population2014","population2015","population2016","population2017",
                   "population2018","population2019","population2020")
EthbyYear=cbind(Ethnic$Label,Ethnic$population1981,Ethnic$population1984,Ethnic$population1988,Ethnic$population1992,Ethnic$population1996,
                Ethnic$population2000,Ethnic$population2004,Ethnic$population2008,Ethnic$population2012,Ethnic$population2016,Ethnic$population2020)
EthbyYear=EthbyYear[-nrow(EthbyYear), ]
EthbyYear=as.data.frame(EthbyYear)
EthbyYear <- EthbyYear %>%
  mutate(across(V2:V12, as.integer)) %>%
  mutate(Average = rowMeans(select(., V2:V12))) %>%
  relocate(Average, .after = V1)

colnames(EthbyYear)=c('Label','population1976','population1980','population1984','population1988','population1992','population1996',
                      'population2000','population2004','population2008','population2012','population2016','population2020')

all_states=unique(pres_elec$state)
Ethresult <- EthbyYear %>%
  # Clean and prepare state labels
  mutate(Label = toupper(Label)) %>%
  # Identify state rows using complete list
  mutate(State = ifelse(Label %in% all_states, Label, NA_character_)) %>%
  fill(State, .direction = "down") %>%
  filter(Label != State) %>%
  # Pivot to long format
  pivot_longer(
    cols = starts_with("population"),
    names_to = "Year",
    values_to = "Population",
    names_prefix = "population",
    values_drop_na = TRUE
  ) %>%
  # Group and calculate
  group_by(State, Year) %>%
  mutate(
    Total_Population = sum(as.numeric(Population)), 
    White_Population = ifelse(Label == "WHITE", Population, NA_real_)
    ) %>%
      fill(White_Population, .direction = "downup") %>%
      summarise(
        Total_Population = first(Total_Population),
        White_Population = first(White_Population),
        White_Share = round((White_Population / Total_Population) * 100, 1)
      ) %>%
      arrange(State, Year)
rm(Ethnic)
colnames(Ethresult)=c('state','year','total_population','white_population','white_share')
df$white_share=Ethresult$white_share

#Read in Education attainment:schooling enrollment
Education=read_excel("C:/Users/24333/Downloads/final_totalenrollment.xlsx")
colnames(Education)=c("state","2023","2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007",
                      "2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988",
                      "1987","1986")

Education=Education %>%
  select("state","1988", "1992","1996","2000","2004","2008","2012","2016","2020")
Education=Education %>% slice(-(52:58))

Education=Education %>%
  mutate(across("1988":"2020", as.integer)) %>%
  mutate('1984'=`1988`) %>%
  relocate('1984', .after = state) %>%
  mutate('1980'=`1988`) %>%
  relocate('1980', .after = state) %>%
  mutate('1976'=`1988`) %>%
  relocate('1976', .after = state)

Education=Education %>%
  pivot_longer(
    cols = -state,
    names_to = "year",
    values_to = "enrollment"
  ) %>%
  mutate(year = as.integer(year)) %>%
  arrange(state, year)

Education <- Education %>%
  group_by(state) %>%  
  mutate(
    prev_enroll = lag(`enrollment`, 1),  
    enrollchange_pct = ((`enrollment` - prev_enroll) / prev_enroll) * 100
  ) %>%
  ungroup()  # Remove grouping
Education[is.na(Education)]=0

df$enrollchange_pct=Education$enrollchange_pct
df$lnenroll=log(Education$enrollment)

##Final data set for information
View(df)

###2. Statistic summary
attach(df)
summary(year)
unique(state)
summary(df)
unique(party_simplified)
describe(voteshare)
describe(Medianincome)
describe(white_share)
describe(Education$enrollment)
##Visualization: line graph of the vote shares in time series
label_data <- df %>%
  group_by(state_po) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(voteshare))

ggplot(df, aes(x = year, y = voteshare, group = state_po, color = state_po)) +
  geom_line(size = 1) +
  geom_text(
    data = label_data,
    aes(label = state_po),
    hjust = 0,
    nudge_x = 0.5,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Republican Vote Share by State (1976–2020)",
    x = "Year",
    y = "Vote Share"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16)
  ) +
  xlim(1976, 2025)

##Visualization: Distribution of states by average vote shares
state_avg_share <- df %>%
  group_by(state) %>%
  summarise(avg_voteshare = mean(voteshare, na.rm = TRUE))

state_avg_share <- state_avg_share %>%
  mutate(vote_group = cut(
    avg_voteshare,
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0–25%", "25–50%", "50–75%", "75–100%"),
    include.lowest = TRUE
  ))

group_counts <- state_avg_share %>%
  count(vote_group)

ggplot(group_counts, aes(x = vote_group, y = n, fill = vote_group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of States by Avg. Republican Vote Share (1976–2020)",
    x = "Average Vote Share Range",
    y = "Number of States"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

##Visualization: Median income
df$logincome=log(df$Medianincome)
label_data <- df %>%
  group_by(state_po) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(logincome))

ggplot(df, aes(x = year, y = logincome, group = state_po, color = state_po)) +
  geom_line(size = 1) +
  geom_text(
    data = label_data,
    aes(label = state_po),
    hjust = 0,
    nudge_x = 0.5,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Median Household income (log) by State (1976–2020)",
    x = "Year",
    y = "log income"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16)
  ) +
  xlim(1976, 2025)

##Visualization: Distribution of states by average median incomes
state_avg_income <- df %>%
  group_by(state_po) %>%
  summarise(avg_income = mean(Medianincome, na.rm = TRUE))

state_avg_income <- state_avg_income %>%
  mutate(income_group = cut(
    avg_income,
    breaks = c(30000,35000,40000,45000,50000,55000),
    labels = c("30k-35K", "35K-40K","40K-45K","45K-50K","50K-55K"),
    include.lowest = TRUE
  ))

group_counts <- state_avg_income %>%
  count(income_group)

ggplot(group_counts, aes(x = income_group, y = n, fill = income_group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of States by Average Median Income (1976–2020)",
    x = "Average income Range",
    y = "Number of States"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")+
scale_x_discrete(drop = FALSE) +  # Show all income groups
  expand_limits(y = max(group_counts$n) + 5)  # Adjust y-axis upper limit

#Relationship Plot
ggplot(df, aes(x = income_change_pct, y = voteshare_change)) +
  geom_point() +
  geom_vline(xintercept = mean(df$income_change_pct, na.rm = TRUE),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(df$voteshare_change, na.rm = TRUE),
             linetype = "dashed", color = "blue") +
  labs(
    x = "Income Change (%)",
    y = "Voteshare Change (%)",
    title = "Scatterplot of Income vs. Voteshare Change"
  ) +
  theme_minimal()

##Visualization: Race compositions, percentage of white citizens
ggplot(df,
       aes(x=year,
           y=white_share,
           colour=factor(state_po))
           ) +
  geom_point() +
  labs(x ="Year", y = "Percentage of white citizens in each state") +
  scale_colour_discrete(name="state_po") +
  geom_smooth(method="lm", se =FALSE)

ggplot(df, aes(x = year, y = white_share)) +
  geom_point() +
  geom_hline(yintercept = mean(df$white_share, na.rm = TRUE),
             linetype = "dashed", color = "blue") +
  labs(
    x = "Year",
    y = "Percentage of white citizens in each state (%)",
    title = "Scatterplot of white citizen shares% in the U.S. vs. Years"
  ) +
  theme_minimal()

ggplot(df, aes(x = white_share, y = voteshare)) +
  geom_point() +
  geom_hline(yintercept = mean(df$voteshare, na.rm = TRUE),
             linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(df$white_share, na.rm = TRUE),
             linetype = "dashed", color = "blue") +
  labs(
    x = "Percentage of white citizens(%)",
    y = "Percentage of votes for the Republican",
    title = "Scatterplot of white citizen shares. vs. Vote for Republican"
  ) +
  theme_minimal()
cor(df$voteshare,df$white_share)

##Visualization: enrollments
#
ggplot(df, aes(x = year, y = lnenroll)) +
  geom_point() +
  geom_hline(yintercept = mean(df$lnenroll, na.rm = TRUE),
             linetype = "dashed", color = "blue") +
  labs(
    x = "Year",
    y = "log(enrollment)",
    title = "Scatterplot: Public school enrollments in the U.S. vs. Years"
  ) +
  theme_minimal()

#
label_data <- df %>%
  group_by(state_po) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(lnenroll))

ggplot(df, aes(x = year, y = lnenroll, group = state_po, color = state_po)) +
  geom_line(size = 1) +
  geom_text(
    data = label_data,
    aes(label = state_po),
    hjust = 0,
    nudge_x = 0.5,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Public school enrollment (log) by State (1976–2020)",
    x = "Year",
    y = "log enrollment"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlim(1976, 2025)

#
ggplot(df, aes(x = year, y = enrollchange_pct)) +
  geom_point() +
  geom_hline(yintercept = mean(df$enrollchange_pct, na.rm = TRUE),
             linetype = "dashed", color = "blue") +
  labs(
    x = "Year",
    y = "%change in enrollment",
    title = "Time series: percentage change in public school enrollments"
  ) +
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12)
  ) 

#
ggplot(df, aes(x = enrollchange_pct, y = voteshare_change)) +
  geom_point() +
  geom_vline(xintercept = mean(df$enrollchange_pct, na.rm = TRUE),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(df$voteshare_change, na.rm = TRUE),
             linetype = "dashed", color = "blue") +
  labs(
    x = "Enrollment Change (%)",
    y = "Voteshare Change (%)",
    title = "Public Education Enrollment vs.Voteshare Change"
  ) +
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12)
  ) 
cor(voteshare_change,Education$enrollchange_pct)