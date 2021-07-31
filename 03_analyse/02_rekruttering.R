library(ggplot2)
library(gridExtra)
library(cowplot)

data <- readRDS("05_data/data_weighted_final_resp.rds")

# Laver fordelinger på q3 og q4_*
Q3 <- data %>%
  group_by(Q3) %>%
  tally() %>%
  mutate(share = n/sum(n),
         quest = "I hvilken grad består arbejdet i lokalforeningen i \nat opsøge og rekruttere kandidater op til kommunalvalg?",
         scale = Q3) %>%
  select(!Q3)
  
Q4_1 <- data %>%
  group_by(Q4_1_resp) %>%
  tally() %>%
  mutate(share = n/sum(n),
         quest = "Potentielle kandidater opsøger selvstændigt opstilling for partiet",
         scale = Q4_1_resp) %>%
  select(!Q4_1_resp)

Q4_3 <- data %>%
  group_by(Q4_3_resp) %>%
  tally() %>%
  mutate(share = n/sum(n),
         quest = "Kandidater bliver typisk opfordret til at stille \nop af ledende medlemmer af lokalforeningen",
         scale = Q4_3_resp) %>%
  select(!Q4_3_resp)

Q4_4 <- data %>%
  group_by(Q4_4_resp) %>%
  tally() %>%
  mutate(share = n/sum(n),
         quest = "Kandidater bliver typisk opfordret til at stille op af ledende medlemmer af lokalforeningen",
         scale = Q4_4_resp) %>%
  select(!Q4_4_resp)

#visualiserer q3, q4_1 og _4
p1 <- survey_besva %>%
  ggplot(aes(x = Q3, fill = Q3)) + 
  geom_bar(aes(y = ((..count..)/sum(..count..)) * 100)) +
  labs(subtitle ="I hvilken grad består arbejdet i lokalforeningen i at opsøge og rekruttere \nkandidater op til kommunalvalg?", align = TRUE, x="", y = "Procent %") +  
  theme_minimal() +
  scale_fill_grey() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        text=element_text(family="Times New Roman"),
        legend.position = "") +
  guides(fill = guide_legend(nrow = 1))

p1

ggsave(filename = "06_Analyse/02_results/02_rekruttering/fordeling_Q3.png",
       plot = p1, 
       width = 6, 
       height = 3.5, 
       dpi = 320)

p2 <- survey_besva %>%
  ggplot(aes(x = Q4_1_resp, fill = Q4_1_resp)) + 
  geom_bar(aes(y = ((..count..)/sum(..count..)) * 100)) +
  labs(subtitle ="Potentielle kandidater opsøger selvstændigt opstilling for \npartiet", align = TRUE, x="", y = "Procent %") +  
  theme_minimal() +
  ylim(0, 60) +
  scale_fill_grey() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        text=element_text(family="Times New Roman"),
        legend.position = "") +
  guides(fill = guide_legend(nrow = 1))

p2

ggsave(filename = "06_Analyse/02_results/02_rekruttering/fordeling_Q4_1.png",
       plot = p2, 
       width = 8, 
       height = 3, 
       dpi = 320)

p3 <- survey_besva %>%
  ggplot(aes(x = Q4_4_resp, fill=Q4_4_resp)) + 
  geom_bar(aes(y = ((..count..)/sum(..count..)) * 100)) +
  labs(subtitle ="Kandidater bliver typisk opfordret til at stille op af ledende \nmedlemmer af lokalforeningen", align = TRUE, x="", y = "Procent %") +  
  theme_minimal() +
  ylim(0, 60) +
  scale_fill_grey() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        text=element_text(family="Times New Roman"),
        legend.position = "") +
  guides(fill = guide_legend(nrow = 1))

p3

ggsave(filename = "06_Analyse/02_results/02_rekruttering/fordeling_Q4_4.png",
       plot = p3, 
       width = 8, 
       height = 3, 
       dpi = 320)

plot <- plot_grid(p2, p3, ncol=2)

ggsave(filename = "06_Analyse/02_results/02_rekruttering/fordeling_Q4_1_AND_4.png",
       plot = plot, 
       width = 9, 
       height = 3.5, 
       dpi = 320)

#Fordelinger på, om nogle er svære at rekrutterer

Q5_1 <- survey_besva %>%
  group_by(Q5_1_resp) %>%
  tally() %>%
  mutate(share = n/sum(n),
         quest = "Kvinder",
         scale = Q5_1_resp) %>%
  select(!Q5_1_resp)


Q5_2 <- survey_besva %>%
  group_by(Q5_2_resp) %>%
  tally() %>%
  mutate(share = n/sum(n),
         quest = "Mænd",
         scale = Q5_2_resp) %>%
  select(!Q5_2_resp)


Q5_3 <- survey_besva %>%
  group_by(Q5_3_resp) %>%
  tally() %>%
  mutate(share = n/sum(n),
         quest = "Minoriteter",
         scale = Q5_3_resp) %>%
  select(!Q5_3_resp)

survey_besva <- survey_besva %>%
  mutate(Parti = Q_party, 
         Ind_krit = ifelse(Parti=='B: Radikale Venstre'|Parti=='F: Socialistisk Folkeparti'|Parti=='Ø: Enhedslisten'|Parti=='Å: Alternativet', "Lempelig udlaendingepolitik",
                         ifelse(Parti=='A: Socialdemokratiet'|Parti=='C: Konservative'|Parti=='D: Nye Borgerlige'|Parti=='I: Liberal Alliance'|Parti=='O: Dansk Folkeparti'|Parti=='V: Venstre'|Parti=='K: Kristendemokraterne'|Parti=='Andet, angiv venligst:', "Stram udlaendingepolitik", "Ukodet")))
  

Q5_3 <- survey_besva %>%
  group_by(Ind_krit, Q5_3_resp) %>%
  tally() %>%
  mutate(share = n/sum(n),
         quest = "Minoriteter",
         scale = Q5_3_resp) %>%
  select(!Q5_3_resp)

#Visualiseringer på, om nogle er svære at rekruttere
p1 <- survey_besva %>%
  ggplot(aes(x = Q5_1_resp, fill = Q5_1_resp)) + 
  geom_bar(aes(y = ((..count..)/sum(..count..)) * 100)) +
  labs(subtitle ="I hvilken grad oplever du, at det er svært at rekruttere kvindelige kandidater", align = TRUE, x="", y = "Procent %") +  
  theme_minimal() +
  ylim(0, 40) +
  scale_fill_grey() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        text=element_text(family="Times New Roman"),
        legend.position = "") +
  guides(fill = guide_legend(nrow = 1))

p1

ggsave(filename = "06_Analyse/02_results/02_rekruttering/fordeling_Q5_1.png",
       plot = p1, 
       width = 8, 
       height = 3, 
       dpi = 320)

p1 <- survey_besva %>%
  ggplot(aes(x = Q5_2_resp, fill = Q5_2_resp)) + 
  geom_bar(aes(y = ((..count..)/sum(..count..)) * 100)) +
  labs(subtitle ="I hvilken grad oplever du, at det er svært at rekruttere mandlige kandidater", align = TRUE, x="", y = "Procent %") +  
  theme_minimal() +
  ylim(0, 40) +
  scale_fill_grey() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        text=element_text(family="Times New Roman"),
        legend.position = "") +
  guides(fill = guide_legend(nrow = 1))

p1

ggsave(filename = "06_Analyse/02_results/02_rekruttering/fordeling_Q5_2.png",
       plot = p1, 
       width = 8, 
       height = 3, 
       dpi = 320)

p1 <- survey_besva %>%
  ggplot(aes(x = Q5_3_resp, fill = Q5_3_resp)) + 
  geom_bar(aes(y = ((..count..)/sum(..count..)) * 100)) +
  labs(subtitle ="I hvilken grad oplever du, at det er svært at rekruttere kandidater med indvandrerbaggrund", align = TRUE, x="", y = "Procent %") +  
  theme_minimal() +
  ylim(0, 40) +
  scale_fill_grey() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        text=element_text(family="Times New Roman"),
        legend.position = "") +
  guides(fill = guide_legend(nrow = 1))

p1

ggsave(filename = "06_Analyse/02_results/02_rekruttering/fordeling_Q5_3.png",
       plot = p1, 
       width = 8, 
       height = 3, 
       dpi = 320)






Q3_parti <- data %>%
  group_by(Parti, Q3) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n),
         quest = "I hvilken grad består arbejdet i lokalforeningen i \nat opsøge og rekruttere kandidater op til kommunalvalg?") %>%
  arrange(Parti) %>%
  select(!quest)
  
Q3_parti

write.xlsx(Q3_parti, "q3_fordeling.xlsx")

