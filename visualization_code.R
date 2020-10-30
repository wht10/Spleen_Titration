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
             labeller = labeller(Flour = c(AF700 = "CD8 AF700", 
                                           APC = "B220 APC", 
                                           APCcy7 = "CD11b APC-Cy7", 
                                           BB700 = "Ly6G BB700", 
                                           BV421 = "CD11c BV421", 
                                           BV605 = "CD4 BV605", 
                                           BV711 = "F4/80 BV711", 
                                           BV786 = "CD45.2 BV786", 
                                           PE = "CD3 PE", 
                                           PeD594 = "Ly6C PE-TR", 
                                           PEcy5 = "NK1.1 PE-Cy5", 
                                           PEcy7 = "CD43 PE-Cy7", 
                                           SB645 = "MHC II SB645", 
                                           LD_ZA = "Zombie Aqua FVD")) )

#BM alone graph
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
             scales = "free", 
             ncol = 4, 
             labeller = labeller(Flour = c(AF700 = "CD8 AF700", 
                                           APC = "B220 APC", 
                                           APCcy7 = "CD11b APC-Cy7", 
                                           BB700 = "Ly6G BB700", 
                                           BV421 = "CD11c BV421", 
                                           BV605 = "CD4 BV605", 
                                           BV711 = "F4/80 BV711", 
                                           BV786 = "CD45.2 BV786", 
                                           PE = "CD3 PE", 
                                           PeD594 = "Ly6C PE-TR", 
                                           PEcy5 = "NK1.1 PE-Cy5", 
                                           PEcy7 = "CD43 PE-Cy7", 
                                           SB645 = "MHC II SB645", 
                                           LD_ZA = "Zombie Aqua FVD")) )
#Together
dat %>% #didnt filter by organ this time
  group_by(Flour) %>% 
  ggplot(aes(x = Titration_value, y = Stain_Index)) + 
  geom_point(aes(color = Organ), size = 1) + #also made the points colored by organ
  geom_line(aes(x = Titration_value, y = avg_SI, color = Organ)) + #added color by organ instead
  geom_hline(aes(yintercept = tot_avg_SI), linetype = "dashed") +
  xlab("Titration") +
  # scale_x_reverse() +
  scale_x_continuous(breaks = c(1/100, 1/200, 1/400, 1/800, 1/1600), 
                     labels = c("1:100", "1:200", "1:400", "1:800", "1:1600"), 
                     trans = "reverse") +
  ylab("Stain Index") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 90), 
        axis.title = element_text(size = 14)) +
  facet_wrap(.~Flour, 
             scales = "free", 
             ncol = 4, 
             labeller = labeller(Flour = c(AF700 = "CD8 AF700", 
                                           APC = "B220 APC", 
                                           APCcy7 = "CD11b APC-Cy7", 
                                           BB700 = "Ly6G BB700", 
                                           BV421 = "CD11c BV421", 
                                           BV605 = "CD4 BV605", 
                                           BV711 = "F4/80 BV711", 
                                           BV786 = "CD45.2 BV786", 
                                           PE = "CD3 PE", 
                                           PeD594 = "Ly6C PE-TR", 
                                           PEcy5 = "NK1.1 PE-Cy5", 
                                           PEcy7 = "CD43 PE-Cy7", 
                                           SB645 = "MHC II SB645", 
                                           LD_ZA = "Zombie Aqua FVD")) )


# making a table
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
         FoldChange = avg_SI/tot_avg_SI) %>% 
  ungroup()
head(tab_dat)

#making table for spleen 
install.packages("magick")
install.packages("webshot")
webshot::install_phantomjs()

tab_dat %>% select(Organ,Flour,Titration_.1.XXX.,avg_SI,weight,FoldChange) %>% filter(Organ == "Spleen") %>% 
  kable(.,"html",col.names = c("Organ", "Fluorochrome", "Titration", "Average Stain Index", "Weight","Fold Change"),
        align = "c", 
        digits = 2) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                                      full_width = F,
                                      font_size = 14,
                                      fixed_thead = F) %>% save_kable("Spleen_Titration.pdf")
#making table for BM  
tab_dat %>% select(Organ,Flour,Titration_.1.XXX.,avg_SI,weight,FoldChange) %>% filter(Organ == "BM") %>% 
  kable(.,"html",col.names = c("Organ", "Fluorochrome", "Titration", "Average Stain Index", "Weight","Fold Change"),
        align = "c", 
        digits = 2) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                                      full_width = F,
                                      font_size = 14,
                                      fixed_thead = F) %>% save_kable("BM_Titration.pdf")
  
# added the sum_FoldChange column
tab_dat <- tab_dat %>% group_by(Flour, Titration_.1.XXX.) %>% mutate(sum_FoldChange = sum(FoldChange))

#new table with added weights
new_tab_dat <- tab_dat %>% group_by(Organ, Flour) %>% 
  mutate(new_weight = ifelse(sum_FoldChange == sort(sum_FoldChange, decreasing = T)[1], "+++", 
                         ifelse(sum_FoldChange == sort(sum_FoldChange, decreasing = T)[2], "++", 
                                ifelse(sum_FoldChange == sort(sum_FoldChange, decreasing = T)[3], "+", "-")))) %>% 
  ungroup()

#new top picks
new_top_picks_tab <- new_tab_dat %>% group_by(Organ, Flour) %>% 
  summarize(top_pick = Titration_.1.XXX.[which(new_weight == "+++")], 
            sum_foldChange_better = sum_FoldChange[which(new_weight == "+++")]) %>% 
  pivot_wider(names_from = Organ, values_from = top_pick)

new_top_picks_tab %>% 
  kable() %>% 
  kable_styling()


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
 


