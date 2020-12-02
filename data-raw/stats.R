#--do stats on 'clean' data
#--11/20/20

library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(stringr)

library(scales)
library(janitor)
library(tidytext)

theme_set(theme_bw())

data("professors")

profs <- professors %>% as_tibble()

#--who makes $0-50000? Get rid of them for now.
#--log makes gender effect a ratio
bsal <-
  profs %>%
  mutate(lsal = log(base_salary)) %>%
  filter(dept_chair == "N", base50 == "Y") %>%
  select(college, dept, gender, rank, base_salary, lsal)


# frequentist -------------------------------------------------------------

library(lme4)
library(lmerTest)
library(emmeans)
library(broom)


# random effects ----------------------------------------------------------

fm1 <- lmerTest::lmer(lsal ~ rank * gender + (1|rank:dept), data = bsal, REML = F)
# without random slope
fm2 <- lmerTest::lmer(lsal ~ rank * gender + (1|dept), data = bsal, REML = F)

#let effect vary by dept and rank?
anova(fm1, fm2)


summary(fm1)

fm1_coefs <-
  emmeans(fm1, specs = pairwise ~ rank:gender, type = "response")$contrasts %>%
  confint() %>%
  rbind() %>%
  as_tibble()

fm1_stats <-
  fm1_coefs %>%
  separate(contrast, into = c("x1", "x2"), sep = "-") %>%
  mutate_if(is.character, str_trim) %>%
  mutate(x1prof = str_sub(x1, 1, -3),
         x2prof = str_sub(x2, 1, -3)) %>%
  filter(x1prof == x2prof) %>%
  mutate_if(is.numeric, function(x) (x*-1)) %>%
  mutate(SE = abs(SE)) %>%
  select(-x2prof) %>%
  rename(rank = x1prof) %>%
  select(rank, x1, x2, everything())


fm1_stats %>%
  ggplot(aes(rank, estimate)) +
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), color = "gray60") +
  geom_point(color = "red", size = 5) +
  coord_flip() +
  labs(title = "Random effect of rank:dept")



# fixed effects -----------------------------------------------------------
fm3 <- lm(lsal ~ rank * gender * dept, data = bsal)

fm3_coefs <-
  emmeans(fm3, specs = pairwise ~ rank:gender|dept, type = "response")$contrasts %>%
    confint(level = 0.8) %>%
    rbind() %>%
    as_tibble()

cystats <-
  fm3_coefs %>%
    separate(contrast, into = c("x1", "x2"), sep = "-") %>%
    mutate_if(is.character, str_trim) %>%
    mutate(x1prof = str_sub(x1, 1, -3),
           x2prof = str_sub(x2, 1, -3)) %>%
    filter(x1prof == x2prof) %>%
  mutate_if(is.numeric, function(x) (x*-1)) %>%
  mutate(SE = abs(SE)) %>%
  filter(!is.na(estimate)) %>%
  mutate(dept2 = reorder_within(dept, estimate, x1),
         year = 2019) %>%
  select(-x2prof) %>%
  rename(rank = x1prof) %>%
  select(year, rank, x1, x2, dept, dept2, everything())


#usethis::use_data(cystats, overwrite = T)

cystats %>%
  ggplot(aes(dept2, estimate)) +
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), color = "gray60") +
  geom_point(color = "red", aes(size = abs(estimate))) +
  scale_x_reordered() +
  facet_wrap(~x1, scales = "free") +
  coord_flip()
