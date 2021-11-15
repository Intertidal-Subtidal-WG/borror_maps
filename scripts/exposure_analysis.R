#' ------------------------------
#' Models to look at if exposure modified global change
#' ------------------------------

# libraries
library(dplyr)
library(emmeans)
library(broom)
library(betareg)
library(readr)
library(car)

# data
dat <- read_csv("data/df_reduced_exposure.csv")

dat_1980s <- filter(dat, year!=2014) 
dat_2014 <- filter(dat, year==2014)

#helper workflows and functions
anova_tab <- . %>%
  Anova(.) %>% 
  tidy %>% 
  rename(` ` = term, `LR` = statistic,
         `P` = p.value) %>%
  knitr::kable(digits = 2) %>%
  kableExtra::kable_minimal()

em_tab <- . %>%
  tidy %>%
  select(-df) %>%
  rename(` ` = quadrant, Estimate = estimate, SE = std.error,
         Z = z.ratio, p = p.value) %>%
  knitr::kable(digits=2) %>%
  kableExtra::kable_minimal()

# 2014 analysis of barens
urchin_mod <- betareg(std_length ~ quadrant+year,
                      data = dat_1980s %>% filter(dominant_cover=="Urchin barrens"))

#LRT
urchin_mod %>%
  anova_tab %>%
  kableExtra::save_kable(file = "tables/barren_anova.html")

#emmeans
urchin_em <- emmeans(urchin_mod, ~quadrant) 

urchin_em %>%
  em_tab %>%
  kableExtra::save_kable(file = "tables/barren_em.html")

#posthoc
contrast(urchin_em, "pairwise", method = "fdr") %>% 
  plot() + geom_vline(xintercept = 0) +
  labs(y = "", title = "Posthoc with fdr for Barrens")
ggsave("figures/barren_posthoc.jpg", dpi = 600)

# kelp
# 2014 analysis of barens
kelp_mod <- betareg(std_length ~ quadrant + year,
                      data = dat_1980s %>% 
                        filter(!(dominant_cover %in% 
                                   c("Urchin barrens", "Mixed Reds"))) %>%
                        group_by(quadrant, year) %>%
                      summarize(std_length = sum(std_length)) %>%
                      ungroup)


#LRT
kelp_mod %>%
  anova_tab %>%
  kableExtra::save_kable(file = "tables/kelp_anova.html")

#emmeans
kelp_em <- emmeans(kelp_mod, ~quadrant) 

kelp_em %>%
  em_tab %>%
  kableExtra::save_kable(file = "tables/kelp_em.html")


contrast(kelp_em, "pairwise", method = "fdr") %>% 
  plot() + geom_vline(xintercept = 0) +
  labs(y = "", title = "Posthoc with fdr for Kelp")
ggsave("figures/kelp_posthoc.jpg", dpi = 600)
