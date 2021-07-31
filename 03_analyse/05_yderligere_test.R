# Indlæser nødvendige pakker
pacman::p_load(cregg, 
               dplyr,
               stringr,
               ggplot2,
               cregg,
               estimatr,
               grid,
               emmeans,
               openxlsx)

# Load themes and colors 
source("06_analyse/epinion_style.R")
source("06_analyse/epinion_color.R")

#### DATA ####
survey <- read_sav("P10080.sav") %>% as_factor()

ggplot(survey, aes(x=bagg1))+
  geom_density(linetype="dashed")


## 1. Indl?s og merge data
# Indl?ser data
#choice_df<-readRDS(file="C:/Users/asev/OneDrive - Epinion/Conjoint Analyse/04. Databehandling/02_data/conjoint_all.Rds")
choice_df<-readRDS(file="05_data/conjoint_choice.rds")
choice_df <- choice_df %>%
  filter(!Id %in% "1124")

# Tjek data
#Egenskaber er tilf?ldigt. Variable med umulige v?rdier er bundne
# Variable uden prohibitions
table(choice_df$Feat01.Navn)
table(choice_df$Feat01a.Koen)
table(choice_df$Feat01b.Etnicitet)
table(choice_df$Feat02.Alder)
table(choice_df$Feat03.civilstatus)
table(choice_df$Feat04a.beskaeftigelse)
table(choice_df$Feat04b.beskaeftigelse_koen)
table(choice_df$Feat05.politisk_erfaring)
table(choice_df$Feat06.partimedlemskab)

# 2. Hovedmodel med Leepers Cregg pakke

# Sætter labels for variable
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

# 3.0 Bilag
#### 3.1 Robusthed - med og uden vægt ----

### med vægt
mm_ww <- cj(data=choice_df, 
            formula.main,
            id = ~Id,
            weights = ~weight_parti,
            estimate = "mm", h0=0.5)

mm_ww$model <- "Med vægt"

## uden vægt 
mm_wow <- cj(data=choice_df, 
             formula.main,
             id = ~Id,
             estimate = "mm", h0=0.5)

mm_wow$model <- "Uden vægt"

mm_wow_ww <- data.frame(rbind(mm_wow, mm_ww)) 

plot <- mm_wow_ww[!mm_wow_ww$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = model)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.26,0.73))+
  geom_hline(yintercept = 0.5,
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
        legend.key=element_blank()) +
  ggtitle("") +
  scale_color_grey() +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave("06_Analyse/02_results/03_conjoint/Med_uden_weight.png", 
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

##### Bilag c.7
mm <- cj(data=choice_df, 
         preferred_profile ~ Feat01.Navn + Feat01a.Koen + Feat01b.Etnicitet + 
           Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
          Feat05.politisk_erfaring + 
           Feat06.partimedlemskab,
         id = ~Id,
         estimate = "mm", 
         by = ~Feat04b.beskaeftigelse_koen,
         h0=0.5)

mm <- mm %>%
  filter(!is.na(estimate))

plot <- mm[mm$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                color = Feat04b.beskaeftigelse_koen)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.30,0.70))+
  geom_hline(yintercept = 0.5,
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
        legend.position = "bottom") +
  ggtitle("") +
  scale_color_grey() +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.MM_beskaef.png",
       plot = plot, 
       width = 9, 
       height = 7, 
       dpi = 320)


## bilag c.7 ----
choice_df_rating <- choice_df %>%
  filter(!is.na(rating))

amce <- cj(data=choice_df_rating, 
           rating ~ Feat01a.Koen + Feat01b.Etnicitet + Feat02.Alder + 
             Feat03.civilstatus + Feat04b.beskaeftigelse_koen + 
             Feat05.politisk_erfaring + Feat06.partimedlemskab,
         id = ~Id,
         estimate = "amce")

plot <- amce %>%  
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
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("AMCE for støtte til kandidat på skala fra 0-10") +
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

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.AMCE_samlet_rating.png",
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

##### 3.0 Estimate most and least likely profiles to be selected -------------------------------------------------------------------------
### 3.1 Fit model
model <- lm_robust(formula = preferred_profile ~ Feat01a.Koen + Feat01b.Etnicitet + Feat02.Alder + Feat03.civilstatus + Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + Feat06.partimedlemskab,
                   data = choice_df,
                   clusters = Id)

### 3.3 Predict probability for every profile
predicted <- bind_cols(choice_df, 
                       as.data.frame(predict(object = model, 
                                             newdata = choice_df,
                                             se.fit = TRUE,
                                             interval = "confidence")))

### 3.4 select percentiles
# Make a vector of percentiles to be selected
percentiles <- quantile(predicted$fit.fit, c(0.0099, 0.25, 0.50, 0.75, 0.999))

# Calculate distance to percentile 
predicted <- predicted %>% 
  mutate(dist1 = abs(percentiles[1] - fit.fit),
         dist25 = abs(percentiles[2] - fit.fit),
         dist50 = abs(percentiles[3] - fit.fit),
         dist75 = abs(percentiles[4] - fit.fit),
         dist99 = abs(percentiles[5] - fit.fit))

# select profiles with the minimum distances to percentiles
profiles <- predicted %>% 
  filter(dist1==min(dist1) | dist25==min(dist25) | dist50==min(dist50) | dist75==min(dist75) | dist99==min(dist99)) %>% 
  select(Feat01a.Koen, Feat01b.Etnicitet, Feat02.Alder, Feat03.civilstatus, Feat04b.beskaeftigelse_koen, Feat05.politisk_erfaring, Feat06.partimedlemskab, fit.fit, fit.lwr, fit.upr, se.fit) %>% 
  distinct() %>% 
  arrange(fit.fit) %>% 
  mutate(percentiles = c(1, 25, 50, 75, 99),
         feature = "Profiltyper")

# Correct confidence interval such that it can't go below zero
profiles <- profiles %>% 
  mutate(fit.lwr = ifelse(fit.lwr<0, 0.00, fit.lwr))

### 3.5 Add text label to profiles
# Add label as a variable
profiles <- profiles %>% 
  mutate(label = paste(paste(Feat01a.Koen),
                       paste(Feat01b.Etnicitet),
                       paste(Feat02.Alder),
                       paste(Feat03.civilstatus),
                       paste(Feat04b.beskaeftigelse_koen),
                       paste(Feat05.politisk_erfaring),
                       paste(Feat06.partimedlemskab),
                       sep = "\n"))

# Draw text
text1 <- textGrob(label = profiles$label[1], 
                  gp = gpar(fontsize = 7))

text25 <- textGrob(label = profiles$label[2], 
                   gp = gpar(fontsize = 7))

text50 <- textGrob(label = profiles$label[3], 
                   gp = gpar(fontsize = 7))

text75 <- textGrob(label = profiles$label[4], 
                   gp = gpar(fontsize = 7))

text99 <- textGrob(label = profiles$label[5], 
                   gp = gpar(fontsize = 7))

### 3.6 Visualise results
plot <- profiles %>% 
  ggplot(., aes(x = percentiles, 
                y = fit.fit)) +
  geom_point(fill = "black",
             size = 2.5) +
  geom_linerange(aes(ymin = fit.lwr ,
                     ymax = fit.upr),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  scale_x_continuous("Percentiler", 
                     breaks = c(1, 25, 50, 75, 99),
                     expand = c(0.05, 0.2)) +
  scale_y_continuous("Estimeret Pr(støtte til kandidat)",
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8)) +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  annotation_custom(text1, 
                    xmin = 1,
                    xmax = 1,
                    ymin = -0.15,
                    ymax = -0.25) + 
  annotation_custom(text25, 
                    xmin = 25,
                    xmax = 25,
                    ymin = -0.15,
                    ymax = -0.25) + 
  annotation_custom(text50,
                    xmin = 50, 
                    xmax = 50,
                    ymin = -0.15,
                    ymax = -0.25) + 
  annotation_custom(text75,
                    xmin = 75,
                    xmax = 75,
                    ymin = -0.15,
                    ymax = -0.25) + 
  annotation_custom(text99,
                    xmin = 99,
                    xmax = 99,
                    ymin = -0.15,
                    ymax = -0.25) +
  coord_cartesian(clip = "off") +
  add_hygge() +
  theme(plot.margin = unit(c(1, 2, 8, 1), "lines"))
plot

ggsave("06_Analyse/02_results/round2/percentil.eps", 
       plot = plot, 
       width = 8.5, 
       height = 6, 
       dpi = 320)


ggsave("06_Analyse/02_results/round2/percentil.png", 
       plot = plot, 
       width = 8.5, 
       height = 6, 
       dpi = 320)


##### 4.0 Andel indvandrere -------------------------------------------------------------------------
### 4.1 Fit model
choice_df.etni <- choice_df 

model <- lm_robust(formula = preferred_profile ~  Feat01b.Etnicitet*log(as.numeric(Andel_ind)) + 
                     Feat02.Alder + Feat03.civilstatus + 
                     Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
                     Feat06.partimedlemskab,
                   data = choice_df.etni,
                   clusters = Id)

model <- lm_robust(formula = preferred_profile ~ Feat01b.Etnicitet*as.numeric(Andel_ind)*Blok,
                   data = choice_df.etni,
                   clusters = Id)


summary(model)

round(model$p.value,3)

dd <- choice_df %>%
  group_by(Andel_bef) %>%
  tally

### 4.3 Predict probability for every profile
predicted <- bind_cols(choice_df.etni, 
                       as.data.frame(predict(object = model, 
                                             newdata = choice_df.etni,
                                             se.fit = TRUE,
                                             interval = "confidence")))
predicted <- predicted %>%
  filter(Feat01b.Etnicitet %in% "Minoritet") %>%
  filter(!is.na(Andel_ind))

plot <- predicted %>% 
  ggplot(., aes(x = as.numeric(Andel_ind), 
                y = fit.fit, 
                color = Blok,
                fill = Blok)) +
  geom_ribbon( aes(ymin = fit.lwr, ymax = fit.upr , fill = Blok, color = NULL), alpha = .15) +
  stat_smooth(geom = 'line', alpha = 0.8, se = TRUE, level = 0.95) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                    limits = c(0,0.45)) +
  scale_fill_manual(name = "Blok", labels = c("Blå blok", "Rød blok"), values = c("#0F283C","#FF4646")) +
  scale_color_manual(name = "Blok", labels = c("Blå blok", "Rød blok"), values = c("#0F283C","#FF4646")) +
  xlab("Andel indvandrere i kommunen") + 
  ylab("Sandsynlighed for at vælge minoritetskandidat") + 
  theme_minimal() 

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/andel_ind_blok.png",
       plot = plot, 
       width = 6, 
       height = 4, 
       dpi = 320)



model <- lm_robust(formula = preferred_profile ~ Feat01b.Etnicitet*log(as.numeric(Andel_ind)) + 
                     Feat02.Alder + Feat03.civilstatus + 
                     Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
                     Feat06.partimedlemskab,
                   data = choice_df.etni,
                   clusters = Id)

round(model$p.value, 4)

summary(model)

model %>% tidy %>% xtable()

model <- lm_robust(formula = preferred_profile ~ Feat01b.Etnicitet*log(as.numeric(Andel_ind)),
                   data = choice_df.etni,
                   clusters = Id)

dk <- as.data.frame(as.numeric(choice_df$Andel_ind))

stargazer(dk)

### 4.3 Predict probability for every profile
predicted <- bind_cols(choice_df.etni, 
                       as.data.frame(predict(object = model, 
                                             newdata = choice_df.etni,
                                             se.fit = TRUE,
                                             interval = "confidence")))
predicted <- predicted %>%
  filter(Feat01b.Etnicitet %in% "Minoritet") %>%
  filter(!is.na(Andel_ind))

plot <- predicted %>% 
  ggplot(., aes(x = as.numeric(Andel_ind), 
                y = fit.fit)) +
  geom_ribbon( aes(ymin = fit.lwr, ymax = fit.upr, color = NULL), alpha = .15) +
  stat_smooth(geom = 'line', alpha = 0.8, se = TRUE, level = 0.95) +
  geom_rug(sides = "b", size = 0.1) +
  geom_hline(yintercept = 0.5,
            linetype = "longdash") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,0.45)) +
  xlab("Andel indvandrere i kommunen") + 
  ylab("Sandsynlighed for at vælge minoritetskandidat") + 
  theme_minimal()
plot

ggsave(filename = "06_Analyse/02_results/andel_ind.png",
       plot = plot, 
       width = 6, 
       height = 4, 
       dpi = 320)


lm(predicted)

ggplot(predicted, aes(x= antal)) +
  geom_bar(aes(y=mean(predicted$antal)), stat="identity", size=.1, color="black", alpha=.4)
  

ggplot(choice_df, aes(y = preferred_profile, x = as.numeric(Andel_ind))) +
  geom_smooth()

andel_bef <- read.xlsx("05_data/andel_ind.xlsx")

gghistogram(
  andel_bef, x = "antal")

?geom_smooth()



##### distrubutions #####
##### 7.0 Distributions ----------------------------------------------------------------------------------------------------------------
### 7.1 Make data frame

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
  geom_col(fill = "#798E87") +
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

ggsave("06_Analyse/02_results/round2/fordelinger.eps", 
       plot = plot1, 
       width = 8, 
       height = 10, 
       dpi = 320)



#### DAHL forsøg med to intervaller
interval1 <- -qnorm((1-0.9)/2) # 90% multiplier
interval2 <- -qnorm((1-0.95)/2) # 95% multiplier

mm <- cj(data=choice_df, 
         formula.main,
         id = ~Id,
         estimate = "mm", h0=0.5)

pal <- wes_palette("Moonrise2")

my_grey = brewer.pal(n = 9, "Greys")[5:9] # Fix at kvinder er for lys
  
mm$model <- "Alle kandidater"

plot <- mm[!mm$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = model)) +
    geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                       ymax = estimate + std.error*interval1),
                   lwd = 1, position = position_dodge(width = 1/2)) +
    geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                        ymax = estimate + std.error*interval2), fatten = 2.5,
                    lwd = 1/2, position = position_dodge(width = 1/2),
                    shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.30,0.70))+
  geom_hline(yintercept = 0.5,
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
  scale_colour_manual(values=pal) +
  guides(colour = guide_legend(reverse=TRUE))

ggsave("06_Analyse/02_results/round2/1.MM_samlet.eps", 
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

#### MM med ny plot opsætning
mm_all <- cj(data=choice_df, 
             preferred_profile ~ Feat01a.Koen + Feat01b.Etnicitet + 
               Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
               Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
               Feat06.partimedlemskab,
           id = ~Id,
           estimate = "mm", h0=0.5)

mm_all$model <- "Alle kandidater"

choice_df.male <- subset(choice_df,Feat01a.Koen=="Mand")

mm_m <- cj(data=choice_df.male, 
           preferred_profile ~ Feat01b.Etnicitet + 
             Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
             Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
             Feat06.partimedlemskab,
         id = ~Id,
         estimate = "mm", h0=0.5)

mm_m$model <- "Mandlige kandidater"

### kvindelige kandidater
choice_df.female <- subset(choice_df, Feat01a.Koen=="Kvinde")

mm_f <- cj(data=choice_df.female, 
           preferred_profile ~ Feat01b.Etnicitet + 
             Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
             Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
             Feat06.partimedlemskab,
         id = ~Id,
         estimate = "mm", h0=0.5)

mm_f$model <- "Kvindelige kandidater"

mm_all_m.f <- data.frame(rbind(mm_all, mm_f,mm_m)) 

plot <- mm_all_m.f[!mm_all_m.f$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = model)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.26,0.73))+
  geom_hline(yintercept = 0.5,
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
        legend.key=element_blank()) +
  ggtitle("") +
  scale_colour_manual(values=pal) +
  guides(colour = guide_legend(reverse=TRUE))

ggsave("06_Analyse/02_results/round2/1.MM_koen.eps", 
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

#### AMCE #####
amce_all <- cj(data=choice_df, 
             preferred_profile ~ Feat01a.Koen + Feat01b.Etnicitet + 
               Feat02.Alder + Feat03.civilstatus + 
               Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
               Feat06.partimedlemskab,
             id = ~Id,
             estimate = "amce")

amce_all$model <- "Alle kandidater"

plot <- amce_all[!amce_all$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = model)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(-0.2,0.4))+
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("%-points ændring i, at være den foretrukne kandidat") +
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
  scale_colour_manual(values=pal) +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave("06_Analyse/02_results/round2/1.AMCE.eps", 
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

plot <- amce_all[amce_all$feature=="KØN",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)))) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(-0.1,0.01))+
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("AMCE\n%-points ændring i, at være den foretrukne kandidat") +
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
  ggtitle("") 

plot

ggsave("06_Analyse/02_results/round2/1.AMCE_Koen_only.eps", 
       plot = plot, 
       width = 6, 
       height = 3, 
       dpi = 320)

ggsave("06_Analyse/02_results/round2/1.AMCE_Koen_only.png", 
       plot = plot, 
       width = 6, 
       height = 3, 
       dpi = 320)

# Create LaTeX output
output <- amce_all %>% 
  filter(feature=="KØN")

output <- kable(x = output,
                format = "latex",
                digits = 2)

# Save results
write_file(x = output,
           path = "07. Analyse/03. Økonomihypoteserne/03. results/beskæftigelse_amce.txt")


choice_df.male <- subset(choice_df,Feat01a.Koen=="Mand")

amce_m <- cj(data=choice_df.male, 
           preferred_profile ~ Feat01b.Etnicitet + 
             Feat02.Alder + Feat03.civilstatus  + 
             Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
             Feat06.partimedlemskab,
           id = ~Id,
           estimate = "amce")

amce_m$model <- "Mandlige kandidater"

choice_df.female <- subset(choice_df, Feat01a.Koen=="Kvinde")

amce_f <- cj(data=choice_df.female, 
           preferred_profile ~ Feat01b.Etnicitet + 
             Feat02.Alder + Feat03.civilstatus  + 
             Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
             Feat06.partimedlemskab,
           id = ~Id,
           estimate = "amce")

amce_f$model <- "Kvindelige kandidater"

amce_all_m.f <- data.frame(rbind(amce_all, amce_f,amce_m)) 

amce_all_m.f <- amce_all_m.f %>% 
 distinct(feature, estimate, .keep_all = TRUE)


plot <- amce_all_m.f[!amce_all_m.f$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = model)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(-0.2,0.4))+
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("%-points ændring i, at være den foretrukne kandidat") +
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
        legend.key=element_blank()) +
  ggtitle("") +
  scale_colour_manual(values=pal) +
  guides(colour = guide_legend(reverse=TRUE))

ggsave("06_Analyse/02_results/round2/1.AMCE_koen.eps", 
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

#### MM KØN (respondentens) 
choice_df.koen <- choice_df %>%
  filter(Koen %in% c("Mand", "Kvinde"))

table(choice_df.koen$Koen)

choice_df.koen$Koen<- factor(choice_df.koen$Koen)

mm<- cj(data = choice_df.koen,
        formula.main,
        by = ~ Koen,
        id = ~ Id,
        estimate = "mm", h0=0.5)

pal2 <- wes_palette("Royal1")

plot <- mm[!mm$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = Koen)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.26,0.73))+
  geom_hline(yintercept = 0.5,
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
        legend.position = "bottom") +
  ggtitle("") +
  scale_colour_manual(values=pal2) +
  guides(colour = guide_legend(reverse=TRUE)) 

ggsave(filename = "06_Analyse/02_results/round2/1.MM_Koen_resp_koen.eps",
       plot = plot, 
       width = 12, 
       height = 15, 
       dpi = 320)

#AMCE køn (respondentens)
amce<- cj(data = choice_df.koen,
          preferred_profile ~ Feat01a.Koen + Feat01b.Etnicitet + 
            Feat02.Alder + Feat03.civilstatus + Feat04a.beskaeftigelse + 
             Feat05.politisk_erfaring + 
            Feat06.partimedlemskab,
        by = ~ Koen,
        id = ~ Id,
        estimate = "amce")

amce <- amce %>% 
 distinct(feature, estimate, .keep_all = TRUE)

pal2 <- wes_palette("Royal1")

plot <- amce[!amce$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = Koen)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(-0.2,0.4))+
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("%-points ændring i, at være den foretrukne kandidat") +
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
        legend.position = "bottom") +
  ggtitle("") +
  scale_colour_manual(values=pal2) +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/round2/1.AMCE_Koen_resp_koen.eps",
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)


#### Robusthed - med og uden vægt

### med vægt
mm_ww <- cj(data=choice_df, 
           formula.main,
           id = ~Id,
           weights = ~weight_parti,
           estimate = "mm", h0=0.5)

mm_ww$model <- "Med vægt"

## uden vægt 
mm_wow <- cj(data=choice_df, 
             formula.main,
            id = ~Id,
            estimate = "mm", h0=0.5)

mm_wow$model <- "Uden vægt"

mm_wow_ww <- data.frame(rbind(mm_wow, mm_ww)) 

plot <- mm_wow_ww[!mm_wow_ww$feature=="BESKÆFTIGELSE",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = model)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.26,0.73))+
  geom_hline(yintercept = 0.5,
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
        legend.key=element_blank()) +
  ggtitle("") +
  scale_color_grey() +
  guides(colour = guide_legend(reverse=TRUE))

mm_wow$p <- round(mm_wow$p, 4)

plot

ggsave("06_Analyse/02_results/round2/1.Med_uden_weight.eps", 
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

ggsave("06_Analyse/02_results/round2/1.Med_uden_weight.png", 
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)
