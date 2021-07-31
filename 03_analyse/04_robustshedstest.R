##### 1.0 Data ------------------------------------------------------------------------------------------------------------------------------
### 1.1 Read data in
choice_df<-readRDS(file="05_data/conjoint_choice.rds")

# 2. Hovedmodel med Leepers Cregg pakke

# S?tter labels for variable
attr(choice_df$Feat01.Navn, "label") <- "NAVN"
attr(choice_df$Feat01a.Koen, "label") <- "KØN"
attr(choice_df$Feat01b.Etnicitet, "label") <- "ETNICITET"
attr(choice_df$Feat02.Alder, "label") <- "ALDER"
attr(choice_df$Feat03.civilstatus, "label") <- "CIVILSTATUS"
attr(choice_df$Feat04a.beskaeftigelse, "label") <- "BESKÆFTIGELSE"
attr(choice_df$Feat04b.beskaeftigelse_koen, "label") <- "ERHVERV"
attr(choice_df$Feat05.politisk_erfaring, "label") <- "POLITISK ERFARING"
attr(choice_df$Feat06.partimedlemskab, "label") <- "PARTIMEDLEMSKAB"

#Set formula
features.vec <- names(choice_df[str_detect(names(choice_df), "^Feat[:digit:]{2}")])
features.plus <- paste(features.vec, collapse=" + ")
formula.main <- as.formula(paste("preferred_profile ~", features.plus))
formula.main.rating <- as.formula(paste("rating ~", features.plus))

interval1 <- -qnorm((1-0.9)/2) # 90% multiplier
interval2 <- -qnorm((1-0.95)/2) # 95% multiplier

##### 3.0 Rating outcome --------------------------------------------------------------------------------------------------------------------
### 3.1 Fit model
choice_df_rating <- choice_df %>%
  filter(!is.na(rating))

mm <- cj(data=choice_df_rating, 
         formula.main.rating,
         id = ~Id,
         estimate = "mm", 
         h0 = mean(choice_df_rating$rating))

plot <- mm[!mm$feature=="BESKÆFTIGELSE"&!mm$feature=="NAVN",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)))) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
                     #, limits = c(0.30,0.70)
  geom_hline(yintercept = mean(choice_df_rating$rating),
             linetype = "longdash") +
  ylab("Marginal means for støtte til kandidat på skala fra 0-10") +
  xlab("") +
  coord_flip() + 
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "none") +
  ggtitle("") +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.MM_samlet_rating.png",
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

 # Create LaTeX output
output <- kable(x = mm,
                format = "latex",
                digits = 2)

##### 4.0 Task number -----------------------------------------------------------------------------------------------------------------------
### 4.1 Make task number indicator
choice_df <- choice_df %>% 
  mutate(conjointnr = case_when(Task==1 ~ "Opgave nr. 1",
                                Task==2 ~ "Opgave nr. 2",
                                Task==3 ~ "Opgave nr. 3",
                                Task==4 ~ "Opgave nr. 4",
                                Task==5 ~ "Opgave nr. 5",
                                Task==6 ~ "Opgave nr. 6",
                                Task==7 ~ "Opgave nr. 7",
                                Task==8 ~ "Opgave nr. 8"))

choice_df <- choice_df %>% 
  mutate(conjointnr_grup = case_when(Task==1 ~ "Opgave nr. 1-4",
                                Task==2 ~ "Opgave nr. 1-4",
                                Task==3 ~ "Opgave nr. 1-4",
                                Task==4 ~ "Opgave nr. 1-4",
                                Task==5 ~ "Opgave nr. 5-8",
                                Task==6 ~ "Opgave nr. 5-8",
                                Task==7 ~ "Opgave nr. 5-8",
                                Task==8 ~ "Opgave nr. 5-8"))

table(choice_df$conjointnr, choice_df$Feat03.civilstatus)

choice_df_5.8 <- choice_df %>%
  filter(Task %in% c(5:8))

choice_df_1.4 <- choice_df %>%
  filter(Task %in% c(1:4))

choice_df$conjointnr <- factor(choice_df$conjointnr)
choice_df$conjointnr_grup <- factor(choice_df$conjointnr_grup)

choice_df_1.4$conjointnr <- factor(choice_df_1.4$conjointnr)
choice_df_5.8$conjointnr <- factor(choice_df_5.8$conjointnr)

###laver diff in means for opgaver


### 4.2 ANOVA
anova <- cj_anova(data = choice_df,
                  formula = preferred_profile ~ Feat01.Navn + Feat01a.Koen + Feat01b.Etnicitet + 
                    Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
                    Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
                    Feat06.partimedlemskab,
                  id = ~Id,
                  by = ~conjointnr)

xtable(anova)

### 4.2.1 ANOVA 1-4
anova <- cj_anova(data = choice_df_1.4,
                  formula = preferred_profile ~ Feat01.Navn + Feat01a.Koen + Feat01b.Etnicitet + 
                    Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
                    Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
                    Feat06.partimedlemskab,
                  id = ~Id,
                  by = ~conjointnr)

xtable(anova)

### 4.2.2 ANOVA 5-8
anova <- cj_anova(data = choice_df_5.8,
                  formula = preferred_profile ~ Feat01.Navn + Feat01a.Koen + Feat01b.Etnicitet + 
                    Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
                    Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
                    Feat06.partimedlemskab,
                  id = ~Id,
                  by = ~conjointnr)

xtable(anova)

### 4.2.3 Resultater på tværs af opgave nummer grupper
mm <- cj(data = choice_df,
         formula = formula.main,
         id = ~Id,
         by = ~conjointnr_grup,
         estimate = "mm",
         h0 = 0.5)

plot <- mm[!mm$feature=="BESKÆFTIGELSE"&!mm$feature=="NAVN",] %>% 
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = conjointnr_grup)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 1,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.23,0.77))+
  geom_hline(yintercept = 0.5,
             linetype = "longdash") +
  ylab("Marginal means") +
  xlab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "bottom") +
  coord_flip()+
  #scale_color_manual(values = c("#FFDB6D", "#C4961A", "#F4EDCA", 
  #                             "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")) +
  scale_colour_grey() +
  ggtitle("") +
  guides(colour = guide_legend(reverse=FALSE)) +
  scale_shape_discrete(breaks = c("Opgave nr. 1-4",
                                  "Opgave nr. 5-8"))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/conjointnr_grup_mm.png",
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

### 4.3 Fit model
mm <- cj(data = choice_df,
         formula = formula.main,
         id = ~Id,
         by = ~conjointnr,
         estimate = "mm",
         h0 = 0.5)

### 4.4 Visualise results
plot1 <- mm[!mm$feature=="BESKÆFTIGELSE"&!mm$feature=="NAVN",] %>%
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = conjointnr)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 1,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.23,0.77))+
  geom_hline(yintercept = 0.5,
             linetype = "longdash") +
  ylab("Marginal means") +
  xlab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "") +
  coord_flip()+
  #scale_color_manual(values = c("#FFDB6D", "#C4961A", "#F4EDCA", 
   #                             "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")) +
  scale_colour_grey() +
  scale_shape_discrete(breaks = c("Opgave nr. 1",
                                  "Opgave nr. 2",
                                  "Opgave nr. 3",
                                  "Opgave nr. 4",
                                  "Opgave nr. 5",
                                  "Opgave nr. 6",
                                  "Opgave nr. 7",
                                  "Opgave nr. 8")) +
  ggtitle("") +
  guides(colour = guide_legend(reverse=FALSE))
plot1

### amce
amce <- cj(data = choice_df,
         formula = preferred_profile ~  Feat01a.Koen + Feat01b.Etnicitet + 
           Feat02.Alder + Feat03.civilstatus + 
           Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
           Feat06.partimedlemskab,
         id = ~Id,
         by = ~conjointnr,
         estimate = "amce")

### 3.5 Visualise results
plot2 <- amce[!amce$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = conjointnr)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 1,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(-0.2,0.4))+
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("AMCE") +
  xlab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank()) +
  coord_flip()+
  #scale_color_manual(values = c("#FFDB6D", "#C4961A", "#F4EDCA", 
  #                             "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")) +
  scale_colour_grey() +
  scale_shape_discrete(breaks = c("Opgave nr. 1",
                                  "Opgave nr. 2",
                                  "Opgave nr. 3",
                                  "Opgave nr. 4",
                                  "Opgave nr. 5",
                                  "Opgave nr. 6",
                                  "Opgave nr. 7",
                                  "Opgave nr. 8")) +
  ggtitle("") +
  guides(colour = guide_legend(reverse=FALSE))
plot2

plot <- plot_grid(plot1, plot2, nrow = 1, rel_widths =c(3/7, 4/7))

ggsave(filename = "06_Analyse/02_results/03_conjoint/conjointnr_amce_mm.png",
       plot = plot, 
       width = 10, 
       height = 11, 
       dpi = 320)

##### 4.0 Profile placing ------------------------------------------------------------------------------------------------------------------
### 4.1 Make profile placering indicator
choice_df <- choice_df %>% 
  mutate(profilnr = case_when(Concept==1 ~ "Profil til venstre",
                              Concept==2 ~ "Profil til højre"))

### 4.2 ANOVA
anova <- cj_anova(data = choice_df,
                  formula.main,
                  id = ~Id,
                  by = ~profilnr)

xtable(anova)

choice_df$profilnr <- factor(choice_df$profilnr)

### 4.3 Fit model
mm <- cj(data = choice_df,
         preferred_profile ~  Feat01.Navn + Feat01a.Koen + Feat01b.Etnicitet + 
           Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
           Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
           Feat06.partimedlemskab,
         id = ~Id,
         by = ~profilnr,
         estimate = "mm",
         h0 = 0.5)

# Visualise results
plot1 <- mm[!mm$feature=="BESKÆFTIGELSE"&!mm$feature=="NAVN",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = profilnr)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 1.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.23,0.77))+
  geom_hline(yintercept = 0.5,
             linetype = "longdash") +
  ylab("MM") +
  xlab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "") +
  coord_flip()+
  scale_colour_grey() +
#  scale_color_manual(values = c("#52854C", "#4E84C4"))  +
  scale_shape_discrete(breaks = c("Profil til venstre",
                                  "Profil til højre")) +
  ggtitle("")  

plot1

amce <- cj(data = choice_df,
           preferred_profile ~ Feat01a.Koen + Feat01b.Etnicitet + 
             Feat02.Alder + Feat03.civilstatus + 
             Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
             Feat06.partimedlemskab,
         id = ~Id,
         by = ~profilnr,
         estimate = "amce")

# Visualise results
plot2 <- amce[!amce$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = profilnr)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 1.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(-0.2,0.4)) +
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("AMCE") +
  xlab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank()) +
  coord_flip()+
  scale_colour_grey() +
  #  scale_color_manual(values = c("#52854C", "#4E84C4"))  +
  scale_shape_discrete(breaks = c("Profil til venstre",
                                  "Profil til højre")) +
  ggtitle("")  

plot2

plot <- plot_grid(plot1, plot2, nrow = 1, rel_widths =c(3/7, 4/7))

plot 

ggsave(filename = "06_Analyse/02_results/03_conjoint/profilnr_both.png",
       plot = plot, 
       width = 10, 
       height = 8, 
       dpi = 320)

## betydning af navn
plot1 <- mm[mm$feature=="NAVN",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = profilnr)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 1.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.4,0.6))+
  geom_hline(yintercept = 0.5,
             linetype = "longdash") +
  ylab("Marginal means for Pr(støtte til kandidat)") +
  xlab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "bottom") +
  coord_flip()+
  scale_colour_grey() +
  scale_shape_discrete(breaks = c("Profil til venstre",
                                  "Profil til højre")) +
  ggtitle("")  

plot1
ggsave(filename = "06_Analyse/02_results/03_conjoint/profilnr_navn.png",
       plot = plot1, 
       width = 6, 
       height = 3.5, 
       dpi = 320)


amce <- cj(data = choice_df,
           preferred_profile ~ Feat01a.Koen + Feat01b.Etnicitet + 
             Feat02.Alder + Feat03.civilstatus + 
             Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
             Feat06.partimedlemskab,
           id = ~Id,
           by = ~profilnr,
           estimate = "amce")

# Create LaTeX output
output <- mm %>%
  select(statistic,
         outcome,
         level,
         estimate,
         std.error,
         z,
         p,
         BY)

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/profilnr.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/profilnr.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)

######## tjek af betydning af profil nr. på tværs af opgaver.

##### 5 Distributions ----------------------------------------------------------------------------------------------------------------
### 5.1 Make data frame

data <- choice_df

# Køn
Koen <- data %>%
  group_by(Feat01a.Koen) %>%
  tally() %>%
  mutate(andel = (n / nrow(data))*100,
         feature = "Køn",
         level = as.factor(Feat01a.Koen)) 

# Etnicitet
Etnicitet <- data %>%
  group_by(Feat01b.Etnicitet) %>%
  tally() %>%
  mutate(andel = (n / nrow(data))*100,
         feature = "Etnicitet",
         level = as.factor(Feat01b.Etnicitet)) 

# Alder
Alder <- data %>%
  group_by(Feat02.Alder) %>%
  tally() %>%
  mutate(andel = (n / nrow(data))*100,
         feature = "Alder",
         level = as.factor(Feat02.Alder))

# Civilstatus
Civilstatus <- data %>%
  group_by(Feat03.civilstatus) %>%
  tally() %>%
  mutate(andel = (n / nrow(data))*100,
         feature = "Civilstatus",
         level = as.factor(Feat03.civilstatus)) 

# Beskæftigelse
Erhverv <- data %>%
  group_by(Feat04b.beskaeftigelse_koen) %>%
  tally() %>%
  mutate(andel = (n / nrow(data))*100,
         feature = "Erhverv",
         level = as.factor(Feat04b.beskaeftigelse_koen))

# Politisk erfaring
Politisk_erfaring <- data %>%
  group_by(Feat05.politisk_erfaring) %>%
  tally() %>%
  mutate(andel = (n / nrow(data))*100,
         feature = "Politisk erfaring",
         level = as.factor(Feat05.politisk_erfaring)) 

# Partimedlemskab
Partimedlemskab <- data %>%
  group_by(Feat06.partimedlemskab) %>%
  tally() %>%
  mutate(andel = (n / nrow(data))*100,
         feature = "Partimedlemskab",
         level = as.factor(Feat06.partimedlemskab)) 

# Combine into one
dist <- bind_rows(Koen, Etnicitet, Alder, Civilstatus, Erhverv, Politisk_erfaring, Partimedlemskab)

dist <- dist %>%
  mutate(feature = factor(feature,
                          levels = c("Køn",
                                     "Etnicitet",
                                     "Alder",
                                     "Civilstatus",
                                     "Erhverv",
                                     "Politisk erfaring",
                                     "Partimedlemskab")),
         level = factor(level,
                        levels = c("Kvinde",
                                   'Mand',
                                   "Majoritet",
                                   'Minoritet',
                                   "28 år",
                                   '35 år',
                                   '42 år',
                                   '49 år',
                                   '56 år',
                                   '63 år',
                                   "Gift og har to børn",
                                   'Bor med sin kæreste',
                                   'Bor alene',
                                   'Gift og har ingen børn',
                                   "Lavtuddannet kvindefag",
                                   'Højtuddannet kvindefag',
                                   'Lavtuddannet mandefag',
                                   'Højtuddannet mandefag',
                                   "Ingen politisk erfaring",
                                   'Aktivt medlem af lokalforeningen',
                                   'Opstillet ved seneste kommunalvalg ',
                                   'Medlem af kommunalbestyrelsen',
                                   "Nyligt medlem",
                                   'Medlem i 1 år',
                                   'Medlem i 3 år',
                                   'Medlem i 6 år',
                                   "Medlem i 10 år")))

dist$color = "Farve"

### 7.2 Visualise results
# Before priming
plot1 <- dist %>%
  ggplot(., aes(x = andel,
                y = reorder(level, desc(level)),
                label = round(andel,1))) +
  geom_col(fill = "grey25") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 80)) +
  geom_text(position = position_dodge(width = .9),
            hjust = -0.2, 
            size = 3) +
  ylab("") +
  xlab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  ggtitle("Fordelinger") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "none")


plot1

ggsave("06_Analyse/02_results/03_conjoint/fordelinger.png", 
       plot = plot1, 
       width = 8, 
       height = 10, 
       dpi = 320)



### BALANCETEST ----

conjoint <- choice_df %>%
  filter(Koen %in% c("Mand","Kvinde")) %>%
  mutate(resp_koen_num = ifelse(Koen=="Kvinde", 1, 0)) 

# Fit model
mm <- cj(data = conjoint,
         formula = resp_koen_num ~ Feat01.Navn + Feat01a.Koen + Feat01b.Etnicitet + 
           Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
           Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
           Feat06.partimedlemskab,
         id = ~Id,
         estimate = "mm",
         h0 = mean(conjoint$resp_køn_num))

plot1 <- mm[!mm$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)))) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 1.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.2,0.4))+
  geom_hline(yintercept = mean(conjoint$resp_koen_num),
             linetype = "longdash") +
  ylab("Marginal means for Pr(kvinde)") +
  xlab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "") +
  coord_flip()+
  scale_colour_grey() +
  ggtitle("")  

plot1

### balance - blok

conjoint <- choice_df %>%
  filter(!is.na(Blok)) %>%
  mutate(Blok_num = ifelse(Blok=="Rod Blok", 1, 0)) 

# Fit model
mm <- cj(data = conjoint,
         formula = Blok_num ~ Feat01.Navn + Feat01a.Koen + Feat01b.Etnicitet + 
           Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
           Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
           Feat06.partimedlemskab,
         id = ~Id,
         estimate = "mm",
         h0 = mean(conjoint$resp_køn_num))

plot2 <- mm[!mm$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)))) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 1.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.4,0.55))+
  geom_hline(yintercept = mean(conjoint$Blok_num),
             linetype = "longdash") +
  ylab("Marginal means for Pr(Rød blok)") +
  xlab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_blank(),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "") +
  coord_flip()+
  scale_colour_grey() +
  ggtitle("")  

plot2

### balance - ind_krit

table(choice_df$Ind_krit)

conjoint <- choice_df %>%
  filter(!is.na(Ind_krit)) %>%
  mutate(Ind_krit_num = ifelse(Ind_krit=="Lempelig udlaendingepolitik", 1, 0)) 

# Fit model
mm <- cj(data = conjoint,
         formula = Ind_krit_num ~ Feat01.Navn + Feat01a.Koen + Feat01b.Etnicitet + 
           Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
           Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
           Feat06.partimedlemskab,
         id = ~Id,
         estimate = "mm",
         h0 = mean(conjoint$resp_køn_num))

plot3 <- mm[!mm$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)))) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 1.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.2,0.35))+
  geom_hline(yintercept = mean(conjoint$Ind_krit_num),
             linetype = "longdash") +
  ylab("Marginal means for Pr(Lempelig udlændingepolitik)") +
  xlab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_blank(),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "") +
  coord_flip()+
  scale_colour_grey() +
  ggtitle("")  

plot3

plot <- plot_grid(plot1, plot2, plot3, nrow = 1, rel_widths =c(4/9, 2.5/9,2.5/9))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/balancetest.png",
       plot = plot, 
       width = 12, 
       height = 12, 
       dpi = 320)

##### 5.0 Manipulation check --------------------------------------------------------------------------------------------------------------
### 5.1 Make a indicator of whether respondents noticed the information
priming <- priming %>% 
  mutate(resp_manipulationstjek_korrekt = ifelse(resp_prime=="Økonomisk prime" & resp_manipulationstjek=="Konsekvenser for Danmarks økonomi, velfærdssamfund og arbejdsmarked" |
                                                   resp_prime=="Kulturelt prime" & resp_manipulationstjek=="Konsekvenser for dansk kultur, normer og værdier", 
                                                 "Korrekt",
                                                 "Ikke korrekt"),
         resp_eksperiment = ifelse(resp_prime=="Økonomisk prime", "Økonomisk trussel", "Kulturel trussel"))

### Select data
# Economic
øko <- priming %>%
  filter(resp_eksperiment=="Økonomisk trussel")

# Cultural
kul <- priming %>%
  filter(resp_eksperiment=="Kulturel trussel")

### 5.3 Labels
# Economic
attr(øko$køn, "label") <- "Køn"
attr(øko$alder, "label") <- "Alder"
attr(øko$uddannelse, "label") <- "Uddannelse"
attr(øko$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(øko$sprog, "label") <- "Engelskkundskaber"
attr(øko$land, "label") <- "Oprindelsesland"
attr(øko$religion, "label") <- "Religion"

# Cultural
attr(kul$køn, "label") <- "Køn"
attr(kul$alder, "label") <- "Alder"
attr(kul$uddannelse, "label") <- "Uddannelse"
attr(kul$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(kul$sprog, "label") <- "Engelskkundskaber"
attr(kul$land, "label") <- "Oprindelsesland"
attr(kul$religion, "label") <- "Religion"

### 5.4 Fit model
# Economic
anova <- cj_anova(data = øko,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_manipulationstjek_korrekt,
                  id = ~Respondent_Serial)

diff1 <- mm_diffs(data = øko,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_manipulationstjek_korrekt,
                  id = ~Respondent_Serial,
                  alpha = 0.975)

# Cultural
anova <- cj_anova(data = kul,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_manipulationstjek_korrekt,
                  id = ~Respondent_Serial)

diff2 <- mm_diffs(data = kul,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_manipulationstjek_korrekt,
                  id = ~Respondent_Serial,
                  alpha = 0.975)

### 5.5 Make indicator
# Economic
diff1 <- diff1 %>% 
  mutate(resp_eksperiment = "Økonomisk trussel")

# Cultural
diff2 <- diff2 %>% 
  mutate(resp_eksperiment = "Kulturel trussel")

### 5.5 Merge
diff <- rbind(diff1, diff2)

# Visualise results
plot <- diff %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = resp_eksperiment)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Forskel i marginal means for Pr(korrekt svar|støtte til indvandrer)\n og Pr(forkert svar|støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(-0.4, 0.4) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Økonomisk trussel",
                                  "Kulturel trussel"))

# Create LaTeX output
diff <- diff %>%
  select(statistic,
         outcome,
         level,
         estimate,
         std.error,
         z,
         p,
         BY,
         resp_eksperiment)

output <- kable(x = diff,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/manipulationstjek_rigtig.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/manipulationstjek_rigtig.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)


### 5.2 Select data
# Correct answers
korrekt <- priming %>% 
  filter(resp_manipulationstjek_korrekt=="Korrekt")

# Incorrect answers
ikkekorrekt <- priming %>% 
  filter(resp_manipulationstjek_korrekt=="Ikke korrekt")

### 5.3 Labels
# Correct answers
attr(korrekt$køn, "label") <- "Køn"
attr(korrekt$alder, "label") <- "Alder"
attr(korrekt$uddannelse, "label") <- "Uddannelse"
attr(korrekt$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(korrekt$sprog, "label") <- "Engelskkundskaber"
attr(korrekt$land, "label") <- "Oprindelsesland"
attr(korrekt$religion, "label") <- "Religion"

# Incorrect answers
attr(ikkekorrekt$køn, "label") <- "Køn"
attr(ikkekorrekt$alder, "label") <- "Alder"
attr(ikkekorrekt$uddannelse, "label") <- "Uddannelse"
attr(ikkekorrekt$beskæftigelse, "label") <- "Tidligere beskæftigelse"
attr(ikkekorrekt$sprog, "label") <- "Engelskkundskaber"
attr(ikkekorrekt$land, "label") <- "Oprindelsesland"
attr(ikkekorrekt$religion, "label") <- "Religion"

### 5.4 Fit model
# Correct answers
diff1 <- mm_diffs(data = korrekt,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_eksperiment,
                  id = ~Respondent_Serial,
                  alpha = 0.975)

# Incorrect answers
diff2 <- mm_diffs(data = ikkekorrekt,
                  formula = choice ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
                  by = ~resp_eksperiment,
                  id = ~Respondent_Serial,
                  alpha = 0.975)

### 5.5 Make indicator
# Correct
diff1 <- diff1 %>% 
  mutate(manipulation = "Korrekt")

# Incorrect
diff2 <- diff2 %>% 
  mutate(manipulation = "Ikke korrekt")

### 5.5 Merge
diff <- rbind(diff1, diff2)

# Visualise results
plot <- diff %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                shape = manipulation)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Forskel i marginal means for Pr(økonomisk trussel|støtte til indvandrer)\n og Pr(kulturel trussel|støtte til indvandrer)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(-0.4, 0.4) +
  add_hygge() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Korrekt",
                                  "Ikke korrekt"))

# Create LaTeX output
output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/manipulationstjek.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/manipulationstjek.eps", 
       plot = plot, 
       width = 8, 
       height = 12, 
       dpi = 320)

##### 6.0 Balance check ----------------------------------------------------------------------------------------------------------------
### 6.1 Gender
# Create indicator
conjoint <- conjoint %>%
  mutate(resp_køn_num = ifelse(resp_køn=="Kvinde", 1, 0)) 

# Fit model
mm <- cj(data = conjoint,
         formula = resp_køn_num ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = mean(conjoint$resp_køn_num))

### 2.2 Visualise results
plot1 <- mm %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = mean(conjoint$resp_køn_num),
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(kvinde)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(0.4, 0.7) +
  add_hygge()

# Create LaTeX output
output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_køn.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_køn.eps", 
       plot = plot1, 
       width = 8, 
       height = 12, 
       dpi = 320)

### 6.2 Age
# Fit model
mm <- cj(data = conjoint,
         formula = resp_alder_num ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = mean(conjoint$resp_alder_num))

# Visualise results
plot2 <- mm %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = mean(conjoint$resp_alder_num),
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for gennemsnitlig alder") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(40, 55) +
  add_hygge()

# Create LaTeX output
output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_alder.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_alder.eps", 
       plot = plot2, 
       width = 8, 
       height = 12, 
       dpi = 320)

### 6.3 Education
# Create indicator
conjoint <- conjoint %>%
  mutate(resp_uddannelse_num = ifelse(resp_uddannelse=="Kort videregående (f.eks. datamatiker, laborant)" |
                                        resp_uddannelse=="Mellemlang videregående (f.eks. teknikumingeniør, lærer)" |
                                        resp_uddannelse=="Bachelor (f.eks. 1. del af en lang videregående uddannelse)" |
                                        resp_uddannelse=="Lang videregående (f.eks. gymnasielærer, økonom, jurist)", 1, 0)) 

# Fit model
mm <- cj(data = conjoint,
         formula = resp_uddannelse_num ~ køn + alder + uddannelse + beskæftigelse + sprog + land + religion + uddannelse:beskæftigelse,
         id = ~Respondent_Serial,
         estimate = "mm",
         h0 = mean(conjoint$resp_uddannelse_num))

# Visualise results
plot3 <- mm %>%  
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.5,
                 color = "black") +
  geom_vline(xintercept = mean(conjoint$resp_uddannelse_num),
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means for Pr(videregående uddannelse)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(0.35, 0.65) +
  add_hygge()

# Create LaTeX output
output <- kable(x = mm,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_uddannelse.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_uddannelse.eps", 
       plot = plot3, 
       width = 8, 
       height = 12, 
       dpi = 320)

# Combine into one
plot <- grid.arrange(plot1, plot2, plot3, nrow = 1)

ggsave("07. Analyse/05. Robusthedstest/03. results/balancetest.eps", 
       plot = plot, 
       width = 18, 
       height = 12, 
       dpi = 320)

### 6.4 Priming experiment
# Select data
prime <- data %>% 
  filter(priming==1) %>% 
  mutate(resp_eksperiment = ifelse(resp_prime=="Økonomisk prime", "Økonomisk trussel", "Kulturel trussel")) %>%
  select(Respondent_Serial, resp_køn, resp_alder_num, resp_uddannelse, resp_eksperiment) %>%
  distinct()

# Make dataframe
df <- data.frame(resp_eksperiment = c("Kulturel trussel",
                                      "Økonomisk trussel"))

# Gender
# Create indicator
prime <- prime %>%
  mutate(resp_køn_num = ifelse(resp_køn=="Kvinde", 1, 0)) 

# Fit model
model <- lm(formula = resp_køn_num ~ resp_eksperiment, data = prime)

# Predict
gender <- data.frame(resp_eksperiment = c("Kulturel trussel",
                                          "Økonomisk trussel"),
                     predict(object = model,
                             newdata = df,
                             interval = "confidence",
                             type = "response"),
                     feature = "Eksperimentel situation"
)

# Visualise results
plot1 <- gender %>%  
  ggplot(., aes(x = fit, 
                y = resp_eksperiment)) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lwr, 
                     xmax = upr),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = mean(prime$resp_køn_num),
             linetype = "longdash",
             color = "black") +
  xlab("Estimeret Pr(kvinde)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(0.4, 0.7) +
  add_hygge()

# Create LaTeX output
output <- kable(x = gender,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_køn_prime.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_køn_prime.eps", 
       plot = plot1, 
       width = 7, 
       height = 5, 
       dpi = 320)

# Age
# Fit model
model <- lm(formula = resp_alder_num ~ resp_eksperiment, data = prime)

# Predict
age <- data.frame(resp_eksperiment = c("Kulturel trussel",
                                       "Økonomisk trussel"),
                  predict(object = model,
                          newdata = df,
                          interval = "confidence",
                          type = "response"),
                  feature = "Eksperimentel situation"
)

# Visualise results
plot2 <- age %>%  
  ggplot(., aes(x = fit, 
                y = resp_eksperiment)) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lwr, 
                     xmax = upr),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = mean(prime$resp_alder_num),
             linetype = "longdash",
             color = "black") +
  xlab("Estimeret gennemsnitlig alder") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(40, 55) +
  add_hygge()

# Create LaTeX output
output <- kable(x = age,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_alder_prime.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_alder_prime.eps", 
       plot = plot2, 
       width = 7, 
       height = 5, 
       dpi = 320)

# Education
# Create indicator
prime <- prime %>%
  mutate(resp_uddannelse_num = ifelse(resp_uddannelse=="Kort videregående (f.eks. datamatiker, laborant)" |
                                        resp_uddannelse=="Mellemlang videregående (f.eks. teknikumingeniør, lærer)" |
                                        resp_uddannelse=="Bachelor (f.eks. 1. del af en lang videregående uddannelse)" |
                                        resp_uddannelse=="Lang videregående (f.eks. gymnasielærer, økonom, jurist)", 1, 0))  

# Fit model
model <- lm(formula = resp_uddannelse_num ~ resp_eksperiment, data = prime)

# Predict
edu <- data.frame(resp_eksperiment = c("Kulturel trussel",
                                       "Økonomisk trussel"),
                  predict(object = model,
                          newdata = df,
                          interval = "confidence",
                          type = "response"),
                  feature = "Eksperimentel situation"
)

# Visualise results
plot3 <- edu %>%  
  ggplot(., aes(x = fit, 
                y = resp_eksperiment)) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lwr, 
                     xmax = upr),
                 height = 0.2,
                 color = "black") +
  geom_vline(xintercept = mean(prime$resp_uddannelse_num),
             linetype = "longdash",
             color = "black") +
  xlab("Estimeret Pr(videregående uddannelse)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  xlim(0.40, 0.60) +
  add_hygge()

# Create LaTeX output
output <- kable(x = edu,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/balance_uddannelse_prime.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/balance_uddannelse_prime.eps", 
       plot = plot3, 
       width = 7, 
       height = 5, 
       dpi = 320)

# Combine into one
plot <- grid.arrange(plot1, plot2, plot3, nrow = 1)

ggsave("07. Analyse/05. Robusthedstest/03. results/balancetest_prime.eps", 
       plot = plot, 
       width = 15, 
       height = 5, 
       dpi = 320)

##### 7.0 Distributions ----------------------------------------------------------------------------------------------------------------
### 7.1 Make data frame
# Køn
køn <- data %>%
  group_by(priming, køn) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Køn",
         level = as.factor(køn)) %>%
  select(-køn)

# Alder
alder <- data %>%
  group_by(priming, alder) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Alder",
         level = as.factor(alder)) %>%
  select(-alder)

# Uddannelse
uddannelse <- data %>%
  group_by(priming, uddannelse) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Uddannelse",
         level = as.factor(uddannelse)) %>%
  select(-uddannelse)

# Tidligere beskæftigelse
beskæftigelse <- data %>%
  group_by(priming, beskæftigelse) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Tidligere beskæftigelse",
         level = as.factor(beskæftigelse)) %>%
  select(-beskæftigelse)

# Engelskkundskaber
sprog <- data %>%
  group_by(priming, sprog) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Engelskkundskaber",
         level = as.factor(sprog)) %>%
  select(-sprog)

# Oprindelsesland
land <- data %>%
  group_by(priming, land) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Oprindelsesland",
         level = as.factor(land)) %>%
  select(-land)

# Religion
religion <- data %>%
  group_by(priming, religion) %>%
  tally() %>%
  mutate(andel = (n / (nrow(data)/2))*100,
         feature = "Religion",
         level = as.factor(religion)) %>%
  select(-religion)

# Combine into one
dist <- bind_rows(køn, alder, uddannelse, beskæftigelse, sprog, land, religion)

dist <- dist %>%
  mutate(feature = factor(feature,
                          levels = c("Køn",
                                     "Alder",
                                     "Uddannelse",
                                     "Tidligere beskæftigelse",
                                     "Engelskkundskaber",
                                     "Oprindelsesland",
                                     "Religion")),
         level = factor(level,
                        levels = c("Mand",
                                   "Kvinde",
                                   "25 år", 
                                   "31 år",
                                   "37 år",
                                   "43 år",
                                   "55 år",
                                   "Ingen formel uddannelse",
                                   "Grundskole",
                                   "Gymnasial uddannelse",
                                   "Erhvervsfaglig uddannelse",
                                   "Kort videregående uddannelse",
                                   "Lang videregående uddannelse",
                                   "Arbejdsløs",
                                   "Tjener",
                                   "Kok",
                                   "Mekaniker",
                                   "Butiksassistent",
                                   "Salgskonsulent",
                                   "Regnskabsmedarbejder",
                                   "It-medarbejder",
                                   "Analytiker",
                                   "Ingeniør",
                                   "Læge",
                                   "Taler og forstår ikke engelsk",
                                   "Forstår, men taler ikke engelsk",
                                   "Kan føre en samtale på engelsk",
                                   "Taler flydende engelsk",
                                   "Polen",
                                   "Tyskland",
                                   "Somalia",
                                   "Kina",
                                   "Syrien",
                                   "Tyrkiet",
                                   "Irak",
                                   "Ikke troende",
                                   "Muslim",
                                   "Kristen")))

### 7.2 Visualise results
# Before priming
plot1 <- dist %>%
  filter(priming==0) %>%
  ggplot(., aes(x = andel,
                y = reorder(level, desc(level)))) +
  geom_col(fill = "black") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 75)) +
  ylab("") +
  xlab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  ggtitle("Inden primingeksperiment (n = 10.000)") +
  add_hygge()

# After priming
plot2 <- dist %>%
  filter(priming==1) %>%
  ggplot(., aes(x = andel,
                y = reorder(level, desc(level)))) +
  geom_col(fill = "black") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 75)) +
  ylab("") +
  xlab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  ggtitle("Efter primingeksperiment (n = 10.000)") +
  add_hygge()

# Combine into one plot
plot <- grid.arrange(plot1, plot2, nrow = 1)

# Create LaTeX output
output <- kable(x = dist,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/fordeling_conjoint.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/fordeling_conjoint.eps", 
       plot = plot, 
       width = 12, 
       height = 12, 
       dpi = 320)

### 7.3 Primingeksperiment
# Create variable
data <- data %>% 
  mutate(resp_eksperiment = case_when(priming==0 ~ "Kontrolmåling",
                                      priming==1 & resp_prime=="Kulturelt prime" ~ "Kulturel trussel",
                                      priming==1 & resp_prime=="Økonomisk prime" ~ "Økonomisk trussel"),
         resp_eksperiment = factor(resp_eksperiment,
                                   levels = c("Kontrolmåling",
                                              "Økonomisk trussel",
                                              "Kulturel trussel")))

# Make data frame
dist <- data %>%
  group_by(resp_eksperiment) %>%
  tally() %>%
  mutate(andel = (n / sum(n))*100,
         feature = "Primingeksperiment")

# Visualise
plot <- dist %>%
  ggplot(., aes(x = andel,
                y = reorder(resp_eksperiment, desc(resp_eksperiment)))) +
  geom_col(fill = "black") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 100)) +
  ylab("") +
  xlab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  add_hygge()

# Create LaTeX output
output <- kable(x = dist,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/05. Robusthedstest/03. results/fordeling_priming.txt")


ggsave("07. Analyse/05. Robusthedstest/03. results/fordeling_priming.eps", 
       plot = plot, 
       width = 7, 
       height = 5, 
       dpi = 320)




choice_df_5.8 <- choice_df %>%
  filter(Task %in% c(5:8))

choice_df_1.4 <- choice_df %>%
  filter(Task %in% c(1:4))

choice_df$conjointnr <- factor(choice_df$conjointnr)
choice_df$conjointnr_grup <- factor(choice_df$conjointnr_grup)

choice_df_1.4$conjointnr <- factor(choice_df_1.4$conjointnr)
choice_df_5.8$conjointnr <- factor(choice_df_5.8$conjointnr)

###laver diff in means for opgaver
amce <- cj(data = choice_df,
           preferred_profile ~ Feat01a.Koen + Feat01b.Etnicitet + 
             Feat02.Alder + Feat03.civilstatus + 
             Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
             Feat06.partimedlemskab + profilnr,
          # by = ~ Ind_krit,
           id = ~ Id,
           estimate = "amce")

plot <- amce[!amce$feature=="BESKÆFTIGELSE"&!amce$feature=="NAVN",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)))) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("Marginal means for Pr(støtte til kandidat)") +
  xlab("") +
  coord_flip() + 
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 7, colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 1),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks =        element_line(colour = "grey50"),
        axis.title.y =      element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "none") +
  ggtitle("") +
  guides(colour = guide_legend(reverse=TRUE))

plot
