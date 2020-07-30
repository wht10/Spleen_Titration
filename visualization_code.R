library(tidyverse)
dat <- read.csv("BM_Spleen_Stain_Index_Table.csv")
str(dat)

#adding column with numeric value of each titration, avg_SI, and se_SI
dat <- dat %>% mutate(Titration_value = 1/Titration_.1.XXX., Organ = as.factor(Organ)) %>% 
  group_by(Organ, Flour, Titration_value) %>% 
  mutate(avg_SI = mean(Stain_Index), 
         se_SI = sd(Stain_Index)/sqrt(3)) %>% 
  ungroup()

#adding mean_SI (for hline in graphs)
dat <- dat %>% 
  group_by(Organ, Flour) %>% 
  mutate(tot_avg_SI = mean(Stain_Index))

# mean SI per titration per fluor
dat %>% summarise(mean = mean(Stain_Index))

# number of fluors (14)
length(unique(dat$Flour))  

# Spleen graphs
dat %>% filter(Organ == "Spleen") %>% 
  group_by(Flour) %>% 
  ggplot(aes(x = Titration_value, y = Stain_Index)) + 
  geom_point(size = 1) +
  geom_line(aes(x = Titration_value, y = avg_SI), col = "red") +
  geom_hline(aes(yintercept = tot_avg_SI), linetype = "dashed") +
  xlab("Titration") +
  # scale_x_reverse() +
  scale_x_continuous(breaks = c(1/100, 1/200, 1/400, 1/800, 1/1600), 
                     labels = c("1:100", "1:200", "1:400", "1:800", "1:1600"), 
                     trans = "reverse") +
  ylab("Stain Index") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90), 
        axis.title = element_text(size = 14)) +
  facet_wrap(.~Flour, 
             scales = "free", 
             ncol = 4, 
             labeller = labeller(Flour = c(AF700 = "Alexa Fluor 700", 
                                           APC = "APC", 
                                           APCcy7 = "APC-Cyanine7", 
                                           BB700 = "BB700", 
                                           BV421 = "Brilliant Violet 421", 
                                           BV605 = "Brilliant Violet 605", 
                                           BV711 = "Brilliant Violet 711", 
                                           BV786 = "Brilliant Violet 786", 
                                           PE = "PE", 
                                           PeD594 = "PE-Dazzle 594", 
                                           PEcy5 = "PE-Cyanine 5", 
                                           PEcy7 = "PE-Cyanine 7", 
                                           SB645 = "Super Bright 645", 
                                           LD_ZA = "Zombie Aqua FVD")) )

dat %>% filter(Organ == "BM") %>% 
  group_by(Flour) %>% 
  ggplot(aes(x = Titration_value, y = Stain_Index)) + 
  geom_point(size = 1) +
  geom_line(aes(x = Titration_value, y = avg_SI), col = "blue") +
  geom_hline(aes(yintercept = tot_avg_SI), linetype = "dashed") +
  xlab("Titration") +
  # scale_x_reverse() +
  scale_x_continuous(breaks = c(1/100, 1/200, 1/400, 1/800, 1/1600), 
                     labels = c("1:100", "1:200", "1:400", "1:800", "1:1600"), 
                     trans = "reverse") +
  ylab("Stain Index") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90), 
        axis.title = element_text(size = 14)) +
  facet_wrap(.~Flour, 
             scales = "free", ncol = 4, 
             labeller = labeller(Flour = c(AF700 = "Alexa Fluor 700", 
                                           APC = "APC", 
                                           APCcy7 = "APC-Cyanine7", 
                                           BB700 = "BB700", 
                                           BV421 = "Brilliant Violet 421", 
                                           BV605 = "Brilliant Violet 605", 
                                           BV711 = "Brilliant Violet 711", 
                                           BV786 = "Brilliant Violet 786", 
                                           PE = "PE", 
                                           PeD594 = "PE-Dazzle 594", 
                                           PEcy5 = "PE-Cyanine 5", 
                                           PEcy7 = "PE-Cyanine 7", 
                                           SB645 = "Super Bright 645", 
                                           LD_ZA = "Zombie Aqua FVD")) )


# making a table
install.packages("kableExtra")
library(kableExtra)

tab_dat <- dat %>% select(Organ, Flour, Titration_.1.XXX., avg_SI, tot_avg_SI)
head(tab_dat)

#formatting titrations
tab_dat$Titration_.1.XXX. <- paste0("1:", dat$Titration_.1.XXX.)
head(tab_dat)

#only unique rows
tab_dat <- distinct(tab_dat)
tab_dat <- tab_dat %>% group_by(Organ, Flour) %>% 
  mutate(weight = ifelse(avg_SI == sort(avg_SI, decreasing = T)[1], "+++", 
                         ifelse(avg_SI == sort(avg_SI, decreasing = T)[2], "++", 
                                ifelse(avg_SI == sort(avg_SI, decreasing = T)[3], "+", "-"))), 
         deviation = avg_SI/tot_avg_SI) %>% 
  ungroup()
head(tab_dat)
# added the sum_dev column
tab_dat <- tab_dat %>% group_by(Flour, Titration_.1.XXX.) %>% mutate(sum_dev = sum(deviation))

#new table with added weights
new_tab_dat <- tab_dat %>% group_by(Organ, Flour) %>% 
  mutate(new_weight = ifelse(sum_dev == sort(sum_dev, decreasing = T)[1], "+++", 
                         ifelse(sum_dev == sort(sum_dev, decreasing = T)[2], "++", 
                                ifelse(sum_dev == sort(sum_dev, decreasing = T)[3], "+", "-")))) %>% 
  ungroup()

#new top picks
new_top_picks_tab <- new_tab_dat %>% group_by(Organ, Flour) %>% 
  summarize(top_pick = Titration_.1.XXX.[which(new_weight == "+++")], 
            sum_foldChange_better = sum_dev[which(new_weight == "+++")]) %>% 
  pivot_wider(names_from = Organ, values_from = top_pick)

#Making a tidy table of top_picks
top_picks_tab <- tab_dat %>% group_by(Organ, Flour) %>% 
  summarize(top_pick = Titration_.1.XXX.[which(weight == "+++")]) %>% 
  pivot_wider(names_from = Organ, values_from = top_pick)

top_picks_tab %>% 
  kable() %>% 
  kable_styling()
head(top_picks_tab)

#Making table pretty
kable(tab_dat, format = "html")
tab_dat %>% 
  kable(col.names = c("Organ", "Fluorochrome", "Titration", "AVG Stain Index", "Weight"),
        align = "c", 
        digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F,
                font_size = 14,
                fixed_thead = F) %>% 
  row_spec(row = which(tab_dat$weight == "+++"), 
           bold = T, 
           italic = F, 
           underline = T) %>% 
  row_spec(row = which(tab_dat$Titration_.1.XXX. == "1:100" & tab_dat$Organ == "Spleen"), 
           bold = F, 
           color = "white", 
           background = "#FF6666" ) %>% 
  row_spec(row = which(tab_dat$Titration_.1.XXX. == "1:100" & tab_dat$Organ == "BM"), 
           bold = F, 
           color = "white", 
           background = "#3399FF" )
 


