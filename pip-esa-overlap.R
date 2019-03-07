library(readxl)
library(tidyr)
library(dplyr)

pip_esa_overlap <- read_excel("table_2019-03-07_13-12-17.xlsx", 
                              range = "B11:Z21")

pip_esa_overlap <- pip_esa_overlap %>%
  rename("pip" ="..1", "incap" ="..2") %>%
  fill("pip") %>%
  filter(!is.na(`Feb-13`))

names(pip_esa_overlap) <- snakecase::to_snake_case(names(pip_esa_overlap))

pip_esa_overlap <- pip_esa_overlap %>%
  gather(key = "key", value = "value", -pip, -incap) 

pip_esa_overlap <- pip_esa_overlap %>%
  filter(pip != "Not PIP/DLA/AA", incap != "Not INCAP")

pip_esa_overlap

pip_esa_overlap2 <- pip_esa_overlap %>% spread(key = pip, value = value)

names(pip_esa_overlap2) <- snakecase::to_snake_case(names(pip_esa_overlap2))

pip_esa_overlap2 <- pip_esa_overlap2  %>% filter(incap != "Total") %>%
  mutate(perc = pip_dla_aa/total) %>%
  select(-incap, -total, -pip_dla_aa) %>% mutate(type = "PIP Getting INCAP")

## Reordering pip_esa_overlap2$key
pip_esa_overlap3 <- pip_esa_overlap %>% spread(key = incap, value = value)


names(pip_esa_overlap3) <- snakecase::to_snake_case(names(pip_esa_overlap3))

pip_esa_overlap3 <- pip_esa_overlap3  %>% filter(pip != "Total") %>%
  mutate(perc = incap/total) %>%
  select(-incap, -total, -pip) %>% mutate(type = "INCAP Getting PIP")

over_combined <- bind_rows(pip_esa_overlap3,pip_esa_overlap2)

over_combined$key <- factor(over_combined$key, 
                               levels=c("feb_13", "may_13", "aug_13", "nov_13",
                                        "feb_14", "may_14", "aug_14", "nov_14",
                                        "feb_15", "may_15", "aug_15", "nov_15",
                                        "feb_16", "may_16", "aug_16", "nov_16",
                                        "feb_17", "may_17", "aug_17", "nov_17",
                                        "feb_18", "may_18", "aug_18"))



library(ggplot2)


p1 <- ggplot(over_combined,
             aes(y = perc, x = key, colour = type, group = type)) + 
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom", legend.title = element_blank()) +
  labs(x = "Quarter", y = "Percent of claimants",
       title = "Percentage of one group of benefit recipients receiving the other type",
       subtitle = "Working age claimants only",
       caption = "(c) Evan Odell | 2019 | CC-BY-SA") 


p1






porg <- ggplot(pip_esa_overlap2, aes(y = pip_perc, x = key, group = 1)) + 
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Quarter", y = "Percent of PIP/DLA/AA receiving INCAP",
       title = "Percentage of PIP/DLA/AA recipients also getting ESA/IB",
       subtitle = "Working age claimants only",
       caption = "(c) Evan Odell | 2019 | CC-BY-SA") 


porg
