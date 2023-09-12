
library(tidyr)
library(readr)
library(tibble)
library(janitor)
library(stringr)
library(forcats)
library(dplyr)
library(ggplot2)
fn = file.path('data','subset-7-sous-ensemble-7.csv') # question  1:97 by like organization or something
fn = file.path('data','subset-6-sous-ensemble-6.csv') # question  1:97 by language and province and area
fn = file.path('data','subset-5-sous-ensemble-5.csv') # question  1:97 by self id'd group, ie 'Federal regulators'
fn = file.path('data','subset-4-sous-ensemble-4.csv') # question  1:97 by ocupational group i.e. 'PC-2'
fn = file.path('data','subset-3-sous-ensemble-3.csv')# question  1:97 by demographics (minority/sexual orientation)
fn = file.path('data','subset-2-sous-ensemble-2.csv')# question  1:97 by age group / seniority / and supervisors
fn = file.path('data','subset-1-sous-ensemble-1.csv')# question  1:97 by department
f1 <- '2020-public-service-employee-survey-open-dataset-ensemble-de-donnees-ouvertes-du-sondage-aupres-.csv'
fn = file.path('data', f1)
dat <- read_csv(fn)  

dat <- 
  file.path('data', '2020-pses-employment-equity-derived-variable-dataset-saff-2020-ensemble-de-donnees-variables-der.csv') |> 
  read_csv()


dat |> sample_n(5000) |> View()


dat |> pull(DESCRIP_E) |> unique()
dat |> distinct(BYCOND, DESCRIP_E) |> View()


dat |> filter(str_detect(BYCOND, '^(D121).*1$')) |> distinct(BYCOND, DESCRIP_E )
dat |> filter(str_detect(DESCRIP_E, 'white')) 


# Race
regex_for_question <-  '^(D121).*'

# Gender
regex_for_question <-  '^(D115).*'

# First Nation
regex_for_question <-  '^(D117).*'

# Disability
regex_for_question <-  '^(D119).*'

# SEX OROENTAION
regex_for_question <-  '^(D122).*'




dat2 <- 
  dat |> 
  filter(str_detect(BYCOND, regex_for_question)) |>
  filter(LEVEL1ID == '00') |>
  group_by(BYCOND, DESCRIP_E, DESCRIP_F) |>
  summarise(ANSCOUNT_MAX = max(ANSCOUNT, na.rm = TRUE)) |>
  separate(BYCOND, c('Q_NUM', 'Q_VAL')) |>
  group_by(Q_NUM) |> 
  mutate(f = ANSCOUNT_MAX/sum(ANSCOUNT_MAX)) 

desc <- 
  dat2 |>
  filter(Q_VAL == 1) |> 
  select(Q_NUM, DESCRIP_E,DESCRIP_F)

dat2 |>
  select(-DESCRIP_E, -DESCRIP_F, -ANSCOUNT_MAX) |> 
  pivot_wider(names_from = c('Q_VAL'), values_from = f, names_prefix = 'val_') |>
  left_join(desc,  by = "Q_NUM") |> 
  arrange(desc(val_1))




dat |> 
  select(BYCOND, DESCRIP_E, DESCRIP_F) |> 
  filter(!is.na(BYCOND)) |>
  distinct()



dat |> 
  filter(str_detect(BYCOND, '^(D121).*1$')) |> 
  filter(LEVEL1ID == '00') |>
  group_by(DESCRIP_E) |> 
  summarise(ANSCOUNT_MAX = max(ANSCOUNT, na.rm = TRUE)) |> 
  mutate(total = all_ps_answers) |>
  mutate(f = ANSCOUNT_MAX/all) |>
  summarise(f = sum(f), n = sum(ANSCOUNT_MAX))

all_ps_answers <- 
  dat |> 
    filter(LEVEL1ID == '00') |> 
    filter(is.na(BYCOND)) |> 
    group_by(DESCRIP_E) |> 
    summarise(ANSCOUNT_MAX = max(ANSCOUNT, na.rm = TRUE)) |>
    pull(ANSCOUNT_MAX)

  7674580	/ 34460065	
  26785480 / 34460065	
  
  D115A 
  
  dat |> 
    filter(str_detect(BYCOND, '^(D115).*1$')) |> 
    filter(LEVEL1ID == '00') |>
    group_by(DESCRIP_E) |> 
    summarise(ANSCOUNT_MAX = max(ANSCOUNT, na.rm = TRUE)) |> 
    mutate(f = ANSCOUNT_MAX/all) 
    filter(str_detect(DESCRIP_E, 'gender')) |> 
    View()

  
  
  fn = file.path('data','subset-6-sous-ensemble-6.csv') # question  1:97 by
dat <- read_csv(fn)  
dat |> count(BYCOND, DESCRIP_E ) |> view()
dat |> count(BYCOND)  |> View()
dat |> count(QUESTION, TITLE_E ) |> View()
dat |> filter(str_detect(str_to_lower(TITLE_E), 'age' )) |> count(QUESTION, TITLE_E ) |> View()


x <- 
  file.path('data','subset-5-sous-ensemble-5.csv') |> 
  read_csv() |> 
  filter(str_detect(BYCOND, '^Q107')) |>
  #filter(str_detect(BYCOND, '^Q114')) |>
  #filter(str_detect(BYCOND, '^Q83')) |>
  filter(LEVEL1ID  == '00') |>
  #filter(DESCRIP_E %in% c('Federal regulators','Compliance, inspection and enforcement')) |> 
  filter(QUESTION  %in% c('Q53', 'Q54')) |> 
  filter(LEVEL1ID == '00')|>
  filter(SURVEYR  == 2020)


y <- 
file.path('data','subset-1-sous-ensemble-1.csv') |> 
  read_csv() |>
  #filter(str_detect(BYCOND, '^Q107')) |>
  #filter(str_detect(BYCOND, '^Q114')) |>
  #filter(str_detect(BYCOND, '^Q83')) |>
  filter(LEVEL1ID  == '00') |>
  #filter(DESCRIP_E %in% c('Federal regulators','Compliance, inspection and enforcement')) |> 
  filter(QUESTION  %in% c('Q53', 'Q54')) |> 
  filter(LEVEL1ID == '00')|>
  filter(SURVEYR  == 2020)

z <-
  file.path('data','subset-2-sous-ensemble-2.csv') |>
  read_csv() |>
  #filter(str_detect(BYCOND, '^Q107')) |>
  filter(str_detect(BYCOND, '^Q114')) |>
  #filter(str_detect(BYCOND, '^Q83')) |>
  filter(LEVEL1ID  == '00') |>
  #filter(DESCRIP_E %in% c('Federal regulators','Compliance, inspection and enforcement')) |> 
  filter(QUESTION  %in% c('Q53', 'Q54')) |> 
  filter(LEVEL1ID == '00')|>
  filter(SURVEYR  == 2020)


get_expected_retires <- function(x, for_highlight){
    inner_join(
      x |> 
        filter(QUESTION  %in% c('Q53')) |>   
        select(DESCRIP_E , ANSWER1 ) |>
        rename(leave_in_two_years = ANSWER1)
      ,
      x |> 
        filter(QUESTION  %in% c('Q54')) |>   
        select(DESCRIP_E , ANSWER1 ) |>
        rename(retire_if_leave = ANSWER1)
    ) |> 
    mutate(expeted_retires = (leave_in_two_years)*(retire_if_leave)/100) |>
    mutate(DESCRIP_E = fct_reorder(DESCRIP_E, expeted_retires)) |>
    mutate(highlight = DESCRIP_E %in% for_highlight) 
  
}
library(writexl)
library(svglite)
main_df |> write_xlsx('retirement_by_job_type.xlsx')
main_df <- get_expected_retires(x, for_highlight = c('Federal regulators','Compliance, inspection and enforcement'))
age_df <- get_expected_retires(z, for_highlight = c())
age_df |> write_xlsx('retirement_by_age.xlsx')
ps_df <- get_expected_retires(y, for_highlight = c())
theme_set(theme_minimal())


p_title = str_to_title('Percent Expected Retirements from the Federal Public Service within the Next 2 Years')
p <- 
bind_rows(age_df,main_df |> filter(highlight == TRUE)
) |>
            
  ggplot(aes(x = expeted_retires , y = DESCRIP_E, fill = highlight)) +
  geom_col(color ='black' ) +
  scale_fill_manual(values=c("#cccccc", "#E69F00")) +
  #geom_text(aes(x = 0, label = paste0( round(expeted_retires,1), '% ', DESCRIP_E), y = DESCRIP_E), inherit.aes = FALSE, alpha =1, hjust = 0, linetype = 'none', nudge_x =0.1, fontface = "bold", fill = 'white') +
  geom_label(aes(x = 0, label = paste0( round(expeted_retires,1), '% '), y = DESCRIP_E), inherit.aes = FALSE, alpha =1) +
  geom_vline(xintercept = ps_df$expeted_retires, linetype = 'dotted', color = 'red', size = 1.5) +
  guides(fill = 'none') +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = p_title, 
       subtitle = paste0('PSES 2020, Public Service Average ', round(ps_df$expeted_retires, 1), '%'), 
       caption = 'https://open.canada.ca/data/en/dataset/4301f4bb-1daa-4b50-afab-d1193b5d2284') 
p    
ggsave(file=paste0(make_clean_names(p_title), '_B.svg'), plot=p, width=10, height=8)


write.table(x, "clipboard", sep="\t") 


Q54
  distinct()
