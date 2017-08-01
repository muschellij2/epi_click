rm(list=ls())
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
fname = "yemen_dat.csv"
df = read_csv(fname)
df = df %>% select(-date_variable, adm2pcod, longitude, latitude)

df = df %>% 
  mutate(year = year(date_value))
min_year = min(df$year)
df = df %>% 
  mutate(diff_year = year - min_year,
         add_weeks = diff_year * 52,
         new_epiweek = add_weeks + epiweek) %>% 
  select(-year, -diff_year)
df = df %>% rename(group_name = adm1name)

df %>% 
  filter(date_value == ymd("2017-01-01") |
           date_value == ymd("2017-01-02") |
           date_value == ymd("2017-01-08") |
           date_value == ymd("2016-09-29") |
           date_value == ymd("2016-12-31") ) %>% 
  select(date_value, epiweek, add_weeks, new_epiweek) %>% 
  unique


week_df = df %>% 
  group_by(group_name, date_value) %>% 
  summarize(vweek = var(epiweek),
            epiweek = unique(epiweek),
            uweek = unique(lubridate::week(date_value)),
            dow = unique(wday(date_value, label = TRUE))) %>% 
  ungroup
stopifnot(all(week_df$vweek == 0))


week_df = df %>% 
  group_by(group_name, new_epiweek) %>% 
  summarize(y = sum(incidence),
            date_value = first(date_value)) %>% 
  ungroup
date_week_df = week_df  %>% 
  mutate(group = group_name,
         x = date_value) 
date_week_df = date_week_df %>% select(x, y, group, group_name)

week_df = week_df %>% 
  mutate(group = group_name,
         x = new_epiweek)
week_df = week_df %>% select(x, y, group, group_name)

df = df %>% 
  group_by(group_name, date_value) %>% 
  summarize(y = sum(incidence)) %>% 
  ungroup
df = df %>% 
  mutate(group = group_name,
         x = date_value)
df = df %>% select(x, y, group, group_name)

regroup = function(x) {
  factor(as.numeric(factor(x)))
}
# remove these when using real data
df = df %>% 
  mutate(group = regroup(group))
week_df = week_df %>% 
  mutate(group = regroup(group))
date_week_df = date_week_df %>%
  mutate(group = regroup(group))

g = df %>% ggplot(aes(x = x, 
                      y = y,
                  colour = group)) + 
  geom_line()  +
  ylab("Number of Cases") + xlab("Date")
pdf("incidence_plots_over_time.pdf", height = 5, width = 10)
print(g)
print(g + guides(color = FALSE))
print({g %+% week_df})
print({( g + guides(color = FALSE)) %+% week_df})
dev.off()


# saveRDS(df, file = "plot_data.rds")
saveRDS(week_df, file = "plot_data.rds")
saveRDS(date_week_df, file = "plot_data_date.rds")
