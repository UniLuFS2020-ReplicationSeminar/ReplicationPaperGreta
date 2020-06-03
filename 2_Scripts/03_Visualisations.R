## VISUALISATIONS ##
library(psy)
library(stargazer)
library(grid)
library(gridExtra)
library(scales)
library(psych)
library(xtable)
library(magrittr)
library(tidyverse)
library(rio)
library(foreign)
library(sjmisc)
library(sjlabelled)
library(stringr)
library(ggplot2)


## Import ----
df <- import(here::here("1_Data","lm_coeffs_moderator.csv"))
df_direct <- import(here::here("1_Data","lm_coeffs.csv"))
df_meta <- import(here::here("1_Data","df_correlations.csv"))

## Creating Theme ----
theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
)
## Plotting P Values ----
df_sign <- ifelse(df$pval<0.05,1,0)
fig_all_pvals <- ggplot(df, aes(x=pval, fill=ifelse(df_sign==1,"74 Interactions","461 Interactions"))) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour="black", bins=20, binwidth=0.05,boundary=-0.5) +
  scale_y_continuous("", labels=percent) +
  scale_x_continuous(expression(paste(italic("p"), " value")), 
                     breaks = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 1), 
                     labels = c("0", "0.05", "0.1", "0.25", "0.5", "0.75", "1"))

fig_all_pvals
fig_all_pvals %T>%
  print() %T>% 
  ggsave(here::here("3_Figures", "fig1-results-all_pvals.pdf"), plot = ., height=4, width=8) %T>%
  ggsave(here::here("3_Figures", "fig1-results-all_pvals.png"), plot = ., height=4, width=8)

# Counting interaction
NROW(df[!is.na(df$est),])
NROW(df[!is.na(df$est) & df$pval < 0.05,])


## Results of Personality Traits ----
df_direct %>%
  filter(trait == "Openness" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Openness") +
  coord_flip()

ggsave(here::here("3_Figures", "results-trait-Openness.pdf"), height=8, width=6)

df_direct %>%
  filter(trait == "Conscientiousness" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Conscientiousness") +
  coord_flip()

ggsave(here::here("3_Figures", "results-trait-Conscientiousness.pdf"), height=8, width=6)

df_direct %>%
  filter(trait == "Extraversion" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Extraversion") +
  coord_flip()

ggsave(here::here("3_Figures", "results-trait-Extraversion.pdf"), height=8, width=6)

df_direct %>%
  filter(trait == "Agreeableness" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Agreeableness") +
  coord_flip()

ggsave(here::here("3_Figures", "results-trait-Agreeableness.pdf"), height=8, width=6)

df_direct %>%
  filter(trait == "Neuroticism" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Neuroticism") +
  coord_flip()

ggsave(here::here("3_Figures", "results-trait-Neuroticism.pdf"), height=8, width=6)




## Plotting Traits with Moderator INTERNET ----
df %>%
  filter(trait == "Openness" & mod == "internet" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Openness\nModerator: Internet") +
  coord_flip()

ggsave(here::here("3_Figures", "results-internet_Openness.pdf"), height=7, width=6)

df %>%
  filter(trait == "Conscientiousness" & mod == "internet" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Conscientiousness\nModerator: Internet") +
  coord_flip()

ggsave(here::here("3_Figures", "results-internet_Conscientiousness.pdf"), height=7, width=6)

df %>%
  filter(trait == "Extraversion" & mod == "internet" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Extraversion\nModerator: Internet") +
  coord_flip()

ggsave(here::here("3_Figures", "results-internet_Extraversion.pdf"), height=7, width=6)

df %>%
  filter(trait == "Agreeableness" & mod == "internet" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Agreeableness\nModerator: Internet") +
  coord_flip()

ggsave(here::here("3_Figures", "results-internet_Agreeableness.pdf"), height=7, width=6)

df %>%
  filter(trait == "Neuroticism" & mod == "internet" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Neuroticism\nModerator: Internet") +
  coord_flip()

ggsave(here::here("3_Figures", "results-internet_Neuroticism.pdf"), height=7, width=6)



## Plotting Traits with Moderator STUDENT ----

df %>%
  filter(trait == "Openness" & mod == "student" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Openness\nModerator: Student") +
  coord_flip()

ggsave(here::here("3_Figures", "results-student_Openness.pdf"), height=7, width=6)

df %>%
  filter(trait == "Conscientiousness" & mod == "student" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Conscientiousness\nModerator: Student") +
  coord_flip()

ggsave(here::here("3_Figures", "results-student_Conscientiousness.pdf"), height=7, width=6)

df %>%
  filter(trait == "Extraversion" & mod == "student" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Extraversion\nModerator: Student") +
  coord_flip()

ggsave(here::here("3_Figures", "results-student_Extraversion.pdf"), height=7, width=6)

df %>%
  filter(trait == "Agreeableness" & mod == "student" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Agreeableness\nModerator: Student") +
  coord_flip()

ggsave(here::here("3_Figures", "results-student_Agreeableness.pdf"), height=7, width=6)

df %>%
  filter(trait == "Neuroticism" & mod == "student" & !is.na(est)) %>%
  ggplot(aes(x = Data, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(colour="black", width=0) +
  geom_point() +
  geom_text(aes(x = Data, y = est, label = pval_text), nudge_x = -0.4, colour="gray50", size = 2.5) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Trait: Neuroticism\nModerator: Student") +
  coord_flip()

ggsave(here::here("3_Figures", "results-student_Neuroticism.pdf"), height=7, width=6)

## Creating figs 2-3 ----
fig2_student <- df %>%
  filter(mod == "student" & !is.na(est)) %>%
  ggplot(aes(x = trait, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_jitter(height=0, width=0.15, size=3, aes(colour = Data, shape = ifelse(pval < 0.05, "Significant", "Insignificant"))) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(1,16)) +
  labs(x = "",
       y = "") +
  coord_flip()

fig2_student %T>%
  print() %T>% 
  ggsave(here::here("3_Figures", "fig2-results-student.pdf"), plot = ., height=7, width=9) %T>%
  ggsave(here::here("3_Figures", "fig2-results-student.png"), plot = ., height=7, width=9)


fig3_internet <- df %>%
  filter(mod == "internet" & !is.na(est)) %>%
  ggplot(aes(x = trait, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_jitter(height=0, width=0.15, size=3, aes(colour = Data, shape = ifelse(pval < 0.05, "Significant", "Insignificant"))) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(1,16)) +
  labs(x = "",
       y = "") +
  coord_flip()

fig3_internet %T>%
  print() %T>% 
  ggsave(here::here("3_Figures", "fig3-results-internet.pdf"), plot = ., height=7, width=9) %T>%
  ggsave(here::here("3_Figures", "fig3-results-internet.png"), plot = ., height=7, width=9)
