# Merge salaries and affiliations based on a key constructed from the
# merge and munge

library(dplyr) # for %>%
library(readr)
library(stringr)

# Load the data ------------------------------------------------------------

#--try with just 2019 first

sals <-
  read_csv("data-raw/professors/01_sals-profs.csv") %>%
  rename(sal_lfm20 = name_lfm20) %>%
  filter(year == 2019)

affs <-
  read_csv("data-raw/professors/02_munged-affs.csv") %>%
  rename(aff_last_name = last_name,
         aff_first_name = first_name,
         aff_mid_name = mid_name,
         aff_lfm20 = name_lfm20) %>%
  filter(year == 2019)

# join ----------------------------------------------------------

# PROBLEMS:
# the affiliation data came with last, first, middle names separated
# salary data the names are listed as one column. Separation by space may not be accurate
# Pasting tog the lfm name from affiliation might not exactly match what is in salary data
# there might be a liang wang in eeob and electrical eng. In the salary data, they will not have unique ids.
# affs doesn't include their position, only their dept. So it will include non-profs.
# theoretically sals data only includes profs

j19 <-
  left_join(sals, affs, by = c("sal_lfm20" = "aff_lfm20", "year" = "year")) %>%
  arrange(sal_lfm20)

# munging -----------------------------------------------------------------


j19_simp <-
  j19 %>%
  select(year, org_short_name, dept_short_name, sal_lfm20,
         aff_last_name, aff_first_name, aff_mid_name, gender, title, total_salary_paid)


#--first, people with sals but no affil. Fixed!
j19_simpnas <-
  j19_simp %>%
  filter(is.na(dept_short_name))

#--duplicates. Fixed!
j19_simp %>%
  group_by(year, sal_lfm20) %>%
  summarise(n = n()) %>%
  filter(n > 1)

j19




# make title_simp ----------------------------------------------------------------

clean_punc <- function(x) {
  stringr::str_replace_all(x, "[^[:alnum:]]", " ") %>%
    stringr::str_trim(.) %>%
    stringr::str_squish(.)
}


p1 <-
  j19 %>%
  mutate(dept_chair = ifelse(grepl("chair|chr", title), "Y", "N"),
         title_simp = case_when(
           grepl("distg|univ|morrill", title) ~ "awarded prof",
           TRUE ~ title),
         title_simp = stringr::str_replace_all(title_simp, "chair", ""),
         title_simp = clean_punc(title_simp),
         rank = ifelse(title_simp != "prof", str_remove_all(title_simp, "prof"), title_simp)) %>%
  mutate_if(is.character, str_trim)





# clean cols ----------------------------------------------------------------

p2 <-
  p1 %>%
  rename(
    name = sal_lfm20,
    college = org_short_name,
    dept = dept_short_name
  ) %>%
  select(year, college, dept, gender, name,
         title, title_simp, rank, dept_chair,
         base_salary, travel_subsistence, total_salary_paid)



# look at issues ----------------------------------------------------------

# 9 people make less than $50,000.
# linda allen's total salary is double her base, something weird there
# diane birt is a dist. prof, money is prob  weird
# nancy grudens, her colleagues make double, maybe half her salary comes from somewhere else?
# ronald leonard is a professor of practice
# alan murdoch is old but is an assistant prof. all his collegues make more
#  I think these are all not acurate reflections of a professor salary

p3 <- p2 %>%
  mutate(base50 = ifelse(base_salary > 50000, "Y", "N"))


# write it ----------------------------------------------------------------

professors <- p3

usethis::use_data(professors, overwrite = TRUE)
