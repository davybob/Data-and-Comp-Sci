#---------- TASK 1 ----------
library(rio) #import
#Question 1
dublinBikes <- import("dublin-bikes.txt", sep='\t',
                       setclass='tibble')
colnames(dublinBikes)[1:5] <- c("time",
                                "precipitation",
                                "temperature",
                                "wind speed",
                                "cloud amount")
str(dublinBikes)

#Question 2
library(lubridate)
str(dublinBikes)
is.timepoint(dublinBikes$time)

# Fill missing values with 0?
dublinBikes[!complete.cases(dublinBikes),]
dublinBikes[is.na(dublinBikes)] <- 0

#Question 3
library(forcats)
dublinBikes$`cloud amount` <- factor(x = dublinBikes$`cloud amount`, 
       levels = sort(unique(dublinBikes$`cloud amount`)),
       labels = c('0 oktas', '1 okta', '2 oktas',
                  '3 oktas', '4 oktas', '5 oktas',
                  '6 oktas', '7 oktas', '8 oktas'),
       ordered = TRUE)
is.factor(dublinBikes$`cloud amount`)
is.ordered(dublinBikes$`cloud amount`)
levels(dublinBikes$`cloud amount`)

#Question 4
dublinBikes$date <- date(dublinBikes$time)
dublinBikes$hour <- hour(dublinBikes$time)

all(table(dublinBikes$date == 24))
length(dublinBikes$date)/24 == 365

#Question 5
dublinBikes$day = wday(dublinBikes$time, label=TRUE)
dublinBikes$month = month(dublinBikes$time,label = TRUE)
is.ordered(dublinBikes$day)
is.ordered(dublinBikes$month)

#Question 6
library(dplyr)
dublinBikes <- dublinBikes |> relocate(date, hour, day, month)
dublinBikes$time = NULL
str(dublinBikes)


#---------- TASK 2 ----------
#Question 1
rainByMonth <- by(dublinBikes, dublinBikes$month, (\(x) sum(x$precipitation)))
Max <- which.max(rainByMonth)
Min <- which.min(rainByMonth)
rainByMonth[Max]
rainByMonth[Min]

#Question 2
library(ggplot2)
library(tidyr)
dublinBikes |> 
  select(date, temperature) |>
  group_by(date) |> 
  summarize(maxT= max(temperature), minT = min(temperature)) |>
  pivot_longer(cols=maxT:minT,
               names_to="measurement",
               values_to="value") |>
  ggplot(aes(x=date, y=value))+
    geom_line(aes(color = measurement))


#Question 3
library(forcats)
dublinBikes |> 
  mutate(day = fct_collapse(day, weekday = c("Mon","Tue", "Wed", "Thu", "Fri"),
                            weekend = c("Sat","Sun")),
         precipitation) |>
  group_by(day) |>
  summarize(`Avg rain` = mean(precipitation, na.rm=TRUE), samples = length(day))
# -- DO A HYPOTHESIS TEST - USING ANOVA

#Question 4
# Month September 2022
# Griffith Avenue (Lane Side)
Mode <- function(x) {
  t <- table(x)
  names(t)[which.max(t)]
}

table(dublinBikes$`cloud amount`)
Mode(dublinBikes$`cloud amount`)

dublinBikes |>
  filter(year(date)==2022 & month=="Oct")|>
  select(date, month, bikes = `Clontarf - James Larkin Rd`, `cloud amount`, day) |>
  group_by(date) |>
  summarize(cloud = Mode(`cloud amount`), `daily traffic` = sum(bikes)) |>
  ggplot(aes(x=date, y=`daily traffic`, color=cloud))+
    geom_point()+
    geom_line(aes(group = cloud))+
    scale_color_manual(values = c("0 okta" = "green",
                                  "1 oktas" = "blue",
                                  "2 oktas" = "orange",
                                  "3 oktas" = "purple",
                                  "4 oktas" = "red",
                                  "5 oktas" = "pink",
                                  "6 oktas" = "yellow",
                                  "7 oktas" = "black",
                                  "8 oktas" = "magenta"))
  
#----------- TASK 3 -------------------

# Number of bikes per wind category during rush hour times, mon-fri
# TABLES
dublinBikes |> 
  filter((hour > 6 & hour < 10) | (hour > 15 & hour < 20) | !(date %in% c('Sat','Sun'))) |>
  select(`wind speed`, starts_with('G'), starts_with('Richmond'), starts_with('Clontarf')) |>
  mutate('wind speed' = cut(`wind speed`, 
                            breaks = c(-1,1,3,6,10,16,21,27,33,40,47,55),
                            labels = c("Calm","Light Air",
                                       "Light Breeze","Gentle Breeze",
                                       "Moderate Breeze","Fresh Breeze",
                                       "Strong Breeze", "Near Gale", 
                                       "Gale", "Strong Gale", "Storm")),
          'Griffith Avenue' = `Griffith Avenue (Lane Side)`+`Griffith Avenue (Clare Rd Side)`,
          'Richmond Street' = `Richmond Street Cyclists 2`+`Richmond Street Cyclists 1`,
          'Clontarf' = `Clontarf - James Larkin Rd`+`Clontarf - Pebble Beach Carpark`)|>
  group_by(`wind speed`) |>
  summarise('Griffith Avenue' = mean(`Griffith Avenue`),
            'Richmond Street' = mean(`Richmond Street`),
            'Grove Road Totam' = mean(`Grove Road Totem`),
            'Clontarf' = mean(Clontarf)) |>
  pivot_longer(`Griffith Avenue`:`Clontarf`, names_to='location',
               values_to = 'bikes') |>
  pivot_wider(names_from=`wind speed`, values_from = `bikes`)

#PLOT
dublinBikes |>
  filter(month %in% c('Jun','Jul','Aug') ) |>
  group_by(hour) |>
  mutate('Griffith Avenue' = `Griffith Avenue (Lane Side)`+`Griffith Avenue (Clare Rd Side)`,
         'Richmond Street' = `Richmond Street Cyclists 2`+`Richmond Street Cyclists 1`,
         'Clontarf' = `Clontarf - James Larkin Rd`+`Clontarf - Pebble Beach Carpark`)|>
  summarise('Griffith Avenue' = mean(`Griffith Avenue`),
            'Richmond Street' = mean(`Richmond Street`),
            'Grove Road Totam' = mean(`Grove Road Totem`),
            'Clontarf' = mean(Clontarf)) |>
  pivot_longer(`Griffith Avenue`:`Clontarf`, names_to='location',
               values_to = 'bikes') |>
  ggplot(aes(x=hour,y=bikes, fill=location))+
    geom_col()+
  scale_x_continuous(breaks = seq(1,24,by=2))