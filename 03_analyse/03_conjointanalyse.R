#rm(list=ls())

# Load themes and colors 
source("06_analyse/epinion_style.R")
source("06_analyse/epinion_color.R")

## 1. Indlæs og merge data
# Indlæser data
choice_df<-readRDS(file="05_data/conjoint_choice.rds")

# Tjek data
#Egenskaber er tilfældigt tildelt.
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

# 1 Overordnede resultater (minus køn og etnicitet) ------------------
## 1.1 MM-plot: Alle respondenter -----------------------------
interval1 <- -qnorm((1-0.9)/2) # 90% multiplier
interval2 <- -qnorm((1-0.95)/2) # 95% multiplier

mm <- cj(data=choice_df, 
         formula.main,
         id = ~Id,
         estimate = "mm", 
         h0=0.5)

mm[mm$feature=="KØN",]

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
        legend.position = "none",
        text=element_text(family="Times New Roman")) +
  ggtitle("") +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.MM_samlet.png",
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

## 1.2 amce-plot: Alle respondenter -----------------------------
amce <- cj(data=choice_df, 
           preferred_profile ~ Feat01a.Koen + Feat01b.Etnicitet + 
             Feat02.Alder + Feat03.civilstatus + 
             Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
             Feat06.partimedlemskab,
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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(-0.15,0.3))+
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("AMCE for Pr(støtte til kandidat)") +
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
        legend.position = "none",
        text=element_text(family="Times New Roman")) +
  ggtitle("") +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.AMCE_samlet.png",
       plot = plot, 
       width = 8, 
       height = 10, 
       dpi = 320)

## 1.3 Percentiler --------
model <- lm_robust(formula = preferred_profile ~ Feat01a.Koen + Feat01b.Etnicitet + Feat02.Alder + Feat03.civilstatus + Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + Feat06.partimedlemskab,
                   data = choice_df,
                   clusters = Id)

# Predict probability for every profile
predicted <- bind_cols(choice_df, 
                       as.data.frame(predict(object = model, 
                                             newdata = choice_df,
                                             se.fit = TRUE,
                                             interval = "confidence")))

# select percentiles
# Make a vector of percentiles to be selected
percentiles <- quantile(predicted$fit.fit, c(0.0098, 0.25, 0.50, 0.75, 0.999))

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

#Add text label to profiles
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

#Visualise results
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
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "none",
        text=element_text(family="Times New Roman")) +
  #add_hygge() +
  theme(plot.margin = unit(c(1, 2, 8, 1), "lines"))
plot

ggsave("06_Analyse/02_results/03_conjoint/percentil.png", 
       plot = plot, 
       width = 8.5, 
       height = 6, 
       dpi = 320)

# 2 Hypoteserne for køn ------------------
## 2.1 mm-plot: Alle respondenter - KØN -----------------------------
plot <- mm[mm$feature=="KØN",] %>%  
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
                     limits = c(0.44,0.56))+
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
        legend.position = "none",
        text=element_text(family="Times New Roman")) +
  ggtitle("") +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.MM_Koen_only.png",
       plot = plot, 
       width = 6, 
       height = 3, 
       dpi = 320)

## 2.2 amce-plot: Alle respondenter - KØN -----------------------------
plot <- amce[amce$feature=="KØN",] %>%  
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
                     limits = c(-0.1,0.02))+
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("AMCE for Pr(støtte til kandidat)") +
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
        legend.position = "none",
        text=element_text(family="Times New Roman")) +
  ggtitle("") +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.AMCE_Koen_only.png",
       plot = plot, 
       width = 6, 
       height = 3, 
       dpi = 320)

output <- amce %>% 
  filter(feature=="KØN")

# Create LaTeX output
output <- kable(x = output,
                format = "latex",
                digits = 2)

## 2.3 mm-plot: MÆND PÅ MÆND (og kvinder på kvinder? - H3/H4 --------------------
choice_df.koen <- choice_df %>%
  filter(Koen %in% c("Mand", "Kvinde"))

choice_df.koen$Koen<- factor(choice_df.koen$Koen)

mm<- cj(data = choice_df.koen,
        formula.main,
        by = ~ Koen,
        id = ~ Id,
        estimate = "mm", h0=0.5)

plot <- mm[mm$feature=="KØN",] %>%  
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
                     limits = c(0.44,0.56))+
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
        legend.key=element_blank(),
        legend.position ="bottom",
        text=element_text(family="Times New Roman")) +
  ggtitle("") +
  scale_colour_grey(name = "Respondent") +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.MM_Koen_by_koen.png",
       plot = plot, 
       width = 6, 
       height = 3, 
       dpi = 320)

output <- mm %>% 
  filter(feature=="KØN")

# Create LaTeX output
output <- kable(x = output,
                format = "latex",
                digits = 2)

## 2.4 MM-diff-plot: MÆND PÅ MÆND (og kvinder på kvinder? - H3/H4 ####
#### MM-diff - kandidat præferencer ift. køn på respondentens køn 
mm_diff <- cj(data=choice_df.koen, 
              preferred_profile ~ Feat01b.Etnicitet + Feat01a.Koen +
                Feat02.Alder + Feat03.civilstatus + 
                Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + Feat06.partimedlemskab,
              id = ~Id,
              by = ~Koen,
              estimate = "mm_diff")

mm_diff$model <- "Alle kandidater"
round(mm_diff$p,3)

plot <- mm_diff[mm_diff$feature=="KØN",] %>%  
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
                     limits = c(-0.03,0.03))+
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
        legend.position = "top",
        text=element_text(family="Times New Roman")) +
  labs(title = "Mandlig vs. kvindelig respondent") +
  guides(colour = guide_legend(reverse=TRUE))
plot

output <- mm %>% 
  filter(feature=="KØN")

# Create LaTeX output
output <- kable(x = output,
                format = "latex",
                digits = 2)

##### DIFF in DIFF estimat - på køn på køn####
fit <- lm_robust(formula = preferred_profile ~ Feat01a.Koen*Koen + Koen,
                 data = choice_df.koen,
                 clusters = Id)

results <- tidy(fit)

plot1 <- results %>%
  filter(term %in% "Feat01a.KoenMand:KoenMand") %>% 
  mutate(navn = case_when(term=="Feat01a.KoenMand:KoenMand" ~ "Mandlig kandidat * Mandlig respondent")) %>%
  ggplot(., aes(x = navn, y = estimate)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = navn, ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = navn, y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_bw() +
  xlab("") +
  ylab("") +
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
        text=element_text(family="Times New Roman")) +
  ggtitle("Diff-in-diff estimat") +
  guides(colour = guide_legend(reverse=TRUE))

plot.grid <- plot_grid(plot, plot1, nrow=1, align = "h", rel_widths = c(3/4, 1/4))
plot.grid
ggsave(filename = "06_Analyse/02_results/03_conjoint/1.mm_diffs_interaktion_koen_by_koen.png",
       plot = plot.grid, 
       width = 10.5, 
       height = 4.5, 
       dpi = 320)

output <- results %>%
  mutate(navn = case_when(term=="Feat01a.KoenMand:KoenMand" ~ "Mandlig kandidat * Mandlig respondent")) %>% 
  filter(feature=="KØN")

# Create LaTeX output
output <- kable(x = output,
                format = "latex",
                digits = 2)

# 3 Hypoteserne for etnicitet ------------------
## 3.1 mm-plot: Alle respondenter - ETNICTET -----------------------------
mm <- cj(data=choice_df, 
         formula.main,
         id = ~Id,
         estimate = "mm", 
         h0=0.5)

plot <- mm[mm$feature=="ETNICITET",] %>%  
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
                     limits = c(0.465,0.535))+
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
        legend.position = "none",
        text=element_text(family="Times New Roman")) +
  ggtitle("") +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.MM_Etnicitet_only.png",
       plot = plot, 
       width = 6, 
       height = 3, 
       dpi = 320)

## 3.2 AMCE-plot: Alle respondenter - ETNICTET -----------------------------
amce <- cj(data=choice_df, 
           preferred_profile ~ Feat01a.Koen + Feat01b.Etnicitet + 
             Feat02.Alder + Feat03.civilstatus + 
             Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
             Feat06.partimedlemskab,
           id = ~Id,
           estimate = "amce")

plot <- amce[amce$feature=="ETNICITET",] %>%  
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
                     limits = c(-0.04,0.02))+
  geom_hline(yintercept = 0,
             linetype = "longdash") +
  ylab("AMCE for Pr(støtte til kandidat)") +
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
        legend.position = "none",
        text=element_text(family="Times New Roman")) +
  ggtitle("") +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.AMCE_Etnicitet_only.png",
       plot = plot, 
       width = 6, 
       height = 3, 
       dpi = 320)

## 3.3 mm-plot: Eksplorativt analyse - køn*etnicitet -----------------------------
mm <- cj(data=choice_df, 
         formula.main,
         id = ~Id,
         estimate = "mm", h0=0.5)

plot <- mm[mm$feature=="NAVN",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                color = statistic)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.43,0.57))+
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
        legend.position = "none",
        text=element_text(family="Times New Roman")) +
  ggtitle("") +
  scale_colour_grey() +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.MM_navn.png",
       plot = plot, 
       width = 6, 
       height = 3, 
       dpi = 320)

## 3.4 mm-plot: Eksplorativt analyse - køn*etnicitet - diff-in-diff -----------------------------
mm_diff <- cj(data=choice_df, 
              preferred_profile ~ 
                #Feat01.Navn +
                Feat01a.Koen +
                Feat02.Alder + Feat03.civilstatus + 
                Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
                Feat06.partimedlemskab,
              id = ~Id,
              by = ~Feat01b.Etnicitet,
              estimate = "mm_diff")

mm_diff$mm_diff <- "Alle kandidater"
round(mm_diff$p,3)

plot <- mm_diff[mm_diff$feature=="KØN",] %>%  
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
                     limits = c(-0.06,0.01))+
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
        legend.position = "top",
        text=element_text(family="Times New Roman")) +
  ggtitle("Minoritet vs. majoritet") +
  guides(colour = guide_legend(reverse=TRUE))
plot

fit <- lm_robust(formula = preferred_profile ~ Feat01a.Koen*Feat01b.Etnicitet + Feat01b.Etnicitet,
                 data = choice_df,
                 clusters = Id)

results <- tidy(fit)

plot1 <- results %>%
  filter(term %in% "Feat01a.KoenMand:Feat01b.EtnicitetMinoritet") %>% 
  mutate(navn = case_when(term=="Feat01a.KoenMand:Feat01b.EtnicitetMinoritet" ~ "Mandlig kandidat * Minoritet")) %>%
  ggplot(., aes(x = navn, y = estimate)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = navn, 
                 y = estimate)) + 
  geom_linerange(aes(x = navn, 
                     ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1) +
  geom_linerange(aes(x = navn, 
                     ymin = estimate - std.error*interval2,
                     ymax = estimate + std.error*interval1),
                 lwd = 1/2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_bw() +
  xlab("") +
  ylab("") +
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
        text=element_text(family="Times New Roman")) +
  ggtitle("Diff-in-diff estimat") +
  guides(colour = guide_legend(reverse=TRUE))

plot.grid <- plot_grid(plot, plot1, nrow=1, align = "h", rel_widths = c(3/4, 1/4))

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.diff_diff_interaktion_k_e.png",
       plot = plot.grid, 
       width = 10.5, 
       height = 4.5, 
       dpi = 320)


# 4 Sidste tre hypoteser - geo, blok og ind.krit -------
# 4.1 Andel indvandrere -------
ols1 <- felm(formula = preferred_profile ~ Feat01b.Etnicitet*log(as.numeric(Andel_ind)) + 
               Feat02.Alder + Feat03.civilstatus + 
               Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
               Feat06.partimedlemskab|0|0|Id, data = choice_df.etni)

stargazer(ols1,report=('vc*p'))

model <- lm_robust(formula = preferred_profile ~ Feat01b.Etnicitet*log(as.numeric(Andel_ind)) + 
               Feat02.Alder + Feat03.civilstatus + 
               Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
               Feat06.partimedlemskab, 
              data = choice_df.etni,
              clusters = Id)

model %>% tidy %>% select(!df) %>% select(!outcome) %>% xtable()

## model til visualisering
model <- lm_robust(formula = preferred_profile ~ Feat01b.Etnicitet*log(as.numeric(Andel_ind)),
                   data = choice_df.etni,
                   clusters = Id)

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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = seq(0.44, 0.58, by = 0.02)) +
  xlab("Andel indvandrere i kommunen") + 
  ylab("Sandsynlighed for at vælge minoritetskandidat") + 
  theme_bw() +
  theme(text=element_text(family="Times New Roman"),
        panel.grid.minor = element_blank())

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/lm_andel_ind.png",
       plot = plot, 
       width = 6, 
       height = 4, 
       dpi = 320)

# Spredning på andel_indvandrere 
dk <- as.data.frame(as.numeric(choice_df$Andel_ind))

stargazer(dk)


# 4.2 Blok forskelle -------
choice_df.blok <- choice_df %>%
  filter(Blok %in% c("Blaa Blok", "Rod Blok"))

choice_df.blok$Blok<- factor(choice_df.blok$Blok)

#marginal means på blok
mm<- cj(data = choice_df.blok,
        formula.main,
        by = ~ Blok,
        id = ~ Id,
        estimate = "mm", h0=0.5)

plot <- mm[mm$feature=="KØN",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = Blok)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.4,0.6))+
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
        axis.ticks = element_line(colour = "grey50"),
        axis.title.y = element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "bottom",
        text=element_text(family="Times New Roman")) +
  ggtitle("") +
  scale_color_manual(labels = c("Blå blok", "Rød blok"), values = c("#0F283C","#FF4646")) +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.MM_Blok_Koen.png",
       plot = plot, 
       width = 6, 
       height = 3, 
       dpi = 320)

#diff in marginal means på blok - diff-in-diff
mm_diff <- cj(data=choice_df.blok, 
              preferred_profile ~ 
                #Feat01.Navn +
                Feat01b.Etnicitet +
                Feat01a.Koen +
                Feat02.Alder + Feat03.civilstatus + 
                Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
                Feat06.partimedlemskab,
              id = ~Id,
              by = ~Blok,
              estimate = "mm_diff")

plot <- mm_diff[mm_diff$feature=="KØN",] %>%  
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
        legend.position = "top",
        text=element_text(family="Times New Roman")) +
  ggtitle("Blå vs. rød blok") +
  guides(colour = guide_legend(reverse=TRUE))
plot

##diff-in-diff estimat
fit <- lm_robust(formula = preferred_profile ~ Feat01a.Koen*Blok + Blok,
                 data = choice_df.blok,
                 clusters = Id)

results <- tidy(fit)

plot1 <- results %>%
  filter(term %in% "Feat01a.KoenMand:BlokRod Blok") %>% 
  mutate(navn = case_when(term=="Feat01a.KoenMand:BlokRod Blok" ~ "Mandlig kandidat * Rød blok")) %>%
  ggplot(., aes(x = navn, y = estimate)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = navn, ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = navn, y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_bw() +
  xlab("") +
  ylab("") +
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
        text=element_text(family="Times New Roman")) +
  ggtitle("Diff-in-diff estimat") +
  guides(colour = guide_legend(reverse=TRUE))

plot.grid <- plot_grid(plot, plot1, nrow=1, align = "h", rel_widths = c(3/4, 1/4))
plot.grid

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.interaktion_k.blok.png",
       plot = plot.grid, 
       width = 10, 
       height = 4.5, 
       dpi = 320)

# 4.2.1 yderligere blok analyse - erhverv ----- 
choice_df.erhverv.k <- choice_df.blok %>%
  filter(Feat01a.Koen %in% "Kvinde")

choice_df.erhverv.k <- within(choice_df.erhverv.k, Blok <- relevel(Blok, ref = "Rod Blok"))

mm_diff <- cj(data=choice_df.erhverv.k, 
              preferred_profile ~ 
                Feat01b.Etnicitet +
                Feat02.Alder + Feat03.civilstatus + 
                Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
                Feat06.partimedlemskab,
              id = ~Id,
              by = ~Blok,
              estimate = "mm_diff")

mm_diff$model <- "Kvindelige kandidater"
round(mm_diff$p,12)

plot <- mm_diff[mm_diff$feature=="ERHVERV",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                color =Blok)) +
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
        legend.position = "",
        text=element_text(family="Times New Roman")) +
  scale_color_grey() +
  ggtitle("Rød vs. blå bloks vurdering af kvindelige kandidater") +
  guides(colour = guide_legend(reverse=TRUE))
plot

choice_df.erhverv.k <- within(choice_df.erhverv.k, Blok <- relevel(Blok, ref = "Rod Blok"))

choice_df.erhverv.k.v2 <- within(choice_df.erhverv.k, Feat04b.beskaeftigelse_koen <- relevel(Feat04b.beskaeftigelse_koen, ref = "Højtuddannet kvindefag"))

fit <- lm_robust(formula = preferred_profile ~ Feat04b.beskaeftigelse_koen*Blok + Blok,
                 data = choice_df.erhverv.k.v2,
                 clusters = Id)

results <- tidy(fit)

round(fit$p.value,4)
plot1 <- results %>%
  filter(term %in% "Feat04b.beskaeftigelse_koenHøjtuddannet mandefag:BlokBlaa Blok") %>% 
  mutate(navn = case_when(term=="Feat04b.beskaeftigelse_koenHøjtuddannet mandefag:BlokBlaa Blok" ~ "Blå blok * højtudd. mandefag")) %>%
  ggplot(., aes(x = navn, y = estimate)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = navn, ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = navn, y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_bw() +
  xlab("") +
  ylab("") +
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
        text=element_text(family="Times New Roman")) +
  labs(title = "Diff-in-diff estimat", caption = "*Reference: Højtudd. kvindefag") +
  guides(colour = guide_legend(reverse=TRUE))

plot.grid <- plot_grid(plot, plot1, nrow=1, align = "h", rel_widths = c(3/4, 1/4))

plot.grid

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.interaktion.blok_erhv.png",
       plot = plot.grid, 
       width = 10.5, 
       height = 4.5, 
       dpi = 320)




plot <- mm[!mm$feature=="BESKÆFTIGELSE"&!mm$feature=="NAVN"&!mm$feature=="KØN"&!mm$feature=="ETNICITET",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = Blok)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.28,0.7))+
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
        axis.ticks = element_line(colour = "grey50"),
        axis.title.y = element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "bottom") +
  ggtitle("") +
  scale_color_manual(labels = c("Blå blok", "Rød blok"), values = c("#0F283C","#FF4646")) +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.MM_Blok.png",
       plot = plot, 
       width = 10, 
       height = 10, 
       dpi = 320)

# 4.3 Indvandrerer kritiske partier ------
choice_df.ind_krit <- choice_df %>%
  filter(Ind_krit %in% c("Stram udlaendingepolitik", "Lempelig udlaendingepolitik"))

choice_df.ind_krit$Ind_krit<- factor(choice_df.ind_krit$Ind_krit)

choice_df.ind_krit$Ind_krit <- relevel(choice_df.ind_krit$Ind_krit, "Lempelig udlaendingepolitik")

mm<- cj(data = choice_df.ind_krit,
        formula.main,
        by = ~ Ind_krit,
        id = ~ Id,
        estimate = "mm", h0=0.5)

plot <- mm[mm$feature=="ETNICITET",] %>%  
  ggplot(., aes(y = estimate, 
                x = reorder(level, desc(level)),
                colour = Ind_krit)) +
  geom_linerange(aes(x = reorder(level, desc(level)), ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = reorder(level, desc(level)), y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.4,0.6))+
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
        axis.ticks = element_line(colour = "grey50"),
        axis.title.y = element_text(angle=90,vjust=.01,hjust=.1),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "bottom",
        text=element_text(family="Times New Roman")) +
  ggtitle("") +
  scale_color_grey()+
  #scale_colour_brewer(labels = c("Stram udlændingepolitik","Lempelig udlændingepolitik"), palette = "Set1", direction = -1) +
  guides(colour = guide_legend(reverse=TRUE))

plot

ggsave(filename = "06_Analyse/02_results/03_conjoint/1.MM_Ind_krit_E.png",
       plot = plot, 
       width = 6, 
       height = 3, 
       dpi = 320)

#udlændingepolitik diff-in-diff ----
mm_diff <- cj(data=choice_df.ind_krit, 
              preferred_profile ~ 
                #Feat01.Navn +
                Feat01b.Etnicitet +
                Feat01a.Koen +
                Feat02.Alder + Feat03.civilstatus + 
                Feat04b.beskaeftigelse_koen + Feat05.politisk_erfaring + 
                Feat06.partimedlemskab,
              id = ~Id,
              by = ~Ind_krit,
              estimate = "mm_diff")

plot <- mm_diff[mm_diff$feature=="ETNICITET",] %>%  
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
                     limits = c(-0.15,0.07))+
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
        legend.position = "top",
        text=element_text(family="Times New Roman")) +
  ggtitle("Lempelig vs. stram udlændepolitik") +
  guides(colour = guide_legend(reverse=TRUE))
plot

fit <- lm_robust(formula = preferred_profile ~ Feat01b.Etnicitet*Ind_krit + Ind_krit,
                 data = choice_df.ind_krit,
                 clusters = Id)

results <- tidy(fit)

round(fit$p.value,4)
plot1 <- results %>%
  filter(term %in% "Feat01b.EtnicitetMinoritet:Ind_kritStram udlaendingepolitik") %>% 
  mutate(navn = case_when(term=="Feat01b.EtnicitetMinoritet:Ind_kritStram udlaendingepolitik" ~ "Minoritet * Stram udlænd.pol")) %>%
  ggplot(., aes(x = navn, y = estimate)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = navn, ymin = estimate - std.error*interval1,
                     ymax = estimate + std.error*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = navn, y = estimate, ymin = estimate - std.error*interval2,
                      ymax = estimate + std.error*interval2), fatten = 2.5,
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_bw() +
  xlab("") +
  ylab("") +
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
        text=element_text(family="Times New Roman")) +
  ggtitle("Diff-in-diff estimat") +
  guides(colour = guide_legend(reverse=TRUE))

plot.grid <- plot_grid(plot, plot1, nrow=1, align = "h", rel_widths = c(3/4, 1/4))
plot.grid
ggsave(filename = "06_Analyse/02_results/03_conjoint/1.interaktion_e.udlaen.png",
       plot = plot.grid, 
       width = 10, 
       height = 4.5, 
       dpi = 320)
