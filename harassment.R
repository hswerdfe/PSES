




library(tidyr)
library(readr)
library(tibble)
library(janitor)
library(stringr)
library(forcats)
library(dplyr)
library(ggplot2)
library(writexl)
library(readxl)
library(stringi)
library(glue)
library(purrr)
library(here)
library(Hmisc)
library(ggrepel)

getwd()
#setwd(file.path(path.expand('~'),  'projects', 'PSES'))


dat_raw_all <- 
list.files(path = 'data', pattern = "^(s|S)ubset.*.csv") |> 
  map_dfr(\(.fn){
    print(.fn)
    file.path('data', .fn) |> 
    read_csv() |> 
      mutate(fn = .fn) |>
      mutate_all(as.character) |>
      clean_names()
    })
#######################
# Set some ints and doubles
dat_raw_all <- 
  dat_raw_all |>
  mutate(across(any_of(c('most_positive_or_least_negative', 'neutral_or_middle_category', 'most_negative_or_least_positive', 'agree','score100', 'anscount', 'indicatorid', 'agree', 'surveyr')), as.integer )) |> 
  mutate(across(matches('^(level[0-9]id)'), as.integer)) |> 
  mutate(across(any_of('score5'), as.double)) |> 
  mutate(across(matches('^answer[0-9]$'), \(.x){ case_when(.x == '9999' ~ as.double(NA), .default = as.double(.x))})) 


#########################
# Find the question
dat_raw_all |> 
  filter(str_detect(title_e, 'Having carefully read the definition of harassment')) |> 
  count(question, title_e, surveyr) 
  


curr_surveyr <- '2022'
curr_question <- 'Q57'
DEPART_OF_INEREST <- 'Canada School of Public Service'








problems(dat_raw_all)
#curr_fn <- 'subset-1-2022_2023-pses-open-dataset-with-labels_sous-ensemble-1-ensemble-de-donnees-ouvertes-du.csv'
fn_meta <- '2022-2023 PSES Supporting Documentation-Documents de référence du SAFF 2022-2023.xlsx'

dem_codes <- file.path('data', fn_meta) |> read_excel(, sheet = 'DEMCODE') |> clean_names()
questions <- file.path('data', fn_meta) |> read_excel(, sheet = 'QUESTIONS') |> clean_names()
level_1_to_2_id <- file.path('data', fn_meta) |> read_excel(, sheet = 'LEVEL1ID_LEVEL2ID') |> clean_names()
level_2_to_5_id <- file.path('data', fn_meta) |> read_excel(, sheet = 'LEVEL2ID_LEVEL5ID') |> clean_names()


dem_codes |> View()
raw_data <- 
  dat_raw_all |> 
  filter(fn == curr_fn)




byconds <- dem_codes |> filter(str_detect(descrip_e, 'LGBTQ')) |> select(bycond,descrip_e)


dat_raw_all |>
  filter(surveyr  ==  curr_surveyr) |>
  filter(!is.na(bycond)) |> 
  count(question, bycond, demcode) |>
  filter(question == curr_question) |>
  filter(str_detect(bycond, 'D122'))
  

dat_raw_all |> 
  count(bycond) filter(str_detect(bycond, 'D120'))
  inner_join(byconds, by = join_by(bycond))


plot_q_yr_depart <- function(dat_all,ps = 'Public Service',  
                             depart = DEPART_OF_INEREST,
                             curr_q = curr_question
                             ){
  
  dat_raw_q_yr <-
    dat_all |>
    filter(level2id  == 0 & 
           level3id == 0 & 
           level4id == 0 & 
           level5id == 0) |> 
    filter(question == curr_q) |> 
    filter(dept_e %in% c(ps, depart)) |>
    select(matches('level.*id'), 
           matches('^answer'),
           anscount, surveyr, 
           bycond, 
           demcode, 
           descrip_e, 
           dept_e, 
           question, 
           fn, 
           title_e ) |>
    mutate(highlight = case_when(dept_e == depart ~ depart, 
                                 dept_e == ps ~ ps, 
                                 .default = 'other' )) |>
    filter((!is.na(answer1)) & (!is.na(anscount))) |>
    #mutate(title_e = str_trim(str_remove(title_e, 'Question [0-9]{1,3}\\.'))) |>
    mutate(lbl = glue('{dept_e} (~{answer1}%)')) |>
    filter(is.na(demcode) & is.na(demcode)) |>
    select(-fn) |>
    distinct()
  
  p_dat <- 
    bind_cols(
      dat_raw_q_yr,  
      binconf((dat_raw_q_yr$answer1/100)*dat_raw_q_yr$anscount , dat_raw_q_yr$anscount ) |> as_tibble() |> clean_names() |>mutate(typ = 'pos')
    ) |> 
    select(-matches('level.*id'))
  
  lbl_dat <- p_dat |> group_by(descrip_e, dept_e, highlight) |> summarise(surveyr  = max(range(surveyr)), point_est = mean(range(point_est))) |>arrange(desc(point_est) )
  lbl_dat$point_est = c(quantile(range(p_dat$point_est), 0.90), quantile(range(p_dat$point_est), 0.8))
  
  p_dat |>
    ggplot(aes(x = surveyr , y = point_est , group = descrip_e, color = highlight )) +
    scale_color_manual(breaks = c('Public Service', DEPART_OF_INEREST, 'other'),
                       values=c("black", "red", 'grey')) +
    scale_size_manual(breaks = c('Public Service', DEPART_OF_INEREST, 'other'),
                      values=c(4, 4, 0.01)) +  
    geom_line(aes(size = highlight)) +
    geom_label(data = p_dat |> filter(surveyr  %in% range(surveyr)), mapping = aes( label = glue('{point_est*100}%'))) +
    geom_text(data = lbl_dat, mapping = aes( label = glue('{dept_e}')), size = 6, hjust = 1) +
    #geom_point(aes(size = highlight)) +
    #geom_smooth(method = lm, se = FALSE) +
    # geom_errorbar(aes(ymin=lower  , ymax=upper), width=.2,
    #               #position=position_dodge(.9)
    #               )  +
    guides(color = 'none', size = 'none') +
    scale_y_continuous(labels = scales::percent) +
    labs(title = str_wrap(p_dat$title_e |> unique(), 60), y = '') +
    theme_minimal() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_text(size = 20, color = 'grey'),
          plot.title = element_text(size = 20, hjust = 0, color = 'grey'),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()
    )
  
}
    










plot_departs <- function(dat_all = dat_raw_all, 
                         ps = 'Public Service',  
                         depart = DEPART_OF_INEREST, 
                         curr_q = curr_question
                         ){
  
  curr_q_dat <- 
    dat_all |>
    filter(surveyr  ==  curr_surveyr & level2id  == 0 & level3id == 0 & level4id == 0 & level5id == 0) |> 
    filter(question == curr_q) |> 
    select(matches('level.*id'), matches('^answer'),anscount, surveyr, bycond, demcode, descrip_e, dept_e, question, fn, title_e ) |>
    filter((!is.na(answer1)) & (!is.na(anscount))) |>
    #mutate(title_e = str_trim(str_remove(title_e, 'Question [0-9]{1,3}\\.'))) |>
    mutate(lbl = glue('{dept_e} (~{answer1}%)')) |>
    filter(is.na(demcode) & is.na(demcode)) |>
    select(-fn) |>
    distinct()

  curr_q_dat_err <- 
    bind_cols(
      curr_q_dat,  
      binconf((curr_q_dat$answer1/100)*curr_q_dat$anscount , curr_q_dat$anscount ) |> as_tibble() |> clean_names() |>mutate(typ = 'pos')
    )
  
  curr_q_dat_ps <- curr_q_dat_err |> filter(dept_e == 'Public Service')
  curr_q_dat_depts <-curr_q_dat_err |> filter(dept_e != 'Public Service')
  
  
  
  p_dat <- 
    curr_q_dat_depts |> 
    mutate(dept_e = fct_reorder(dept_e, point_est)) |>
    mutate(highlight = dept_e == DEPART_OF_INEREST)
  
  
  p_dat |> 
    ggplot(aes(x = point_est, y = dept_e, fill = highlight, color = highlight)) + 
    geom_segment(aes(xend = lower, x = upper, yend = dept_e, y = dept_e), size = 1.2) +
    geom_text(aes( label = lbl , x = 0), hjust = 1, nudge_x = -0.01, size = 3) + 
    geom_vline(xintercept =  curr_q_dat_ps$point_est[1], linetype  = 'dashed', color = 'black', size = 1.5) + 
    #geom_rect(data = curr_q_dat_ps, alpha = 0.25,mapping =  aes(xmin = lower, xmax = upper, ymax = max(as.integer(p_dat$dept_e)+1), ymin = min(as.integer(p_dat$dept_e)))) +
    geom_point(shape = 23) +
    #geom_label(aes(label = glue('{point_est*100} %')), fill = 'white', size =3) + 
    annotate(geom = 'text', 
             x = curr_q_dat_ps$point_est + 0.02, 
             y = quantile(as.integer(p_dat$dept_e), 0.5), 
             label = curr_q_dat_ps$lbl,
             angle = 90, 
             size = 5) +
    scale_x_continuous(labels = scales::percent, limits = c(-0.5,NA)) +
    labs(title = str_wrap(p_dat$title_e |> unique(), 60), y = '') +
    guides(fill = 'none', color = 'none') +
    scale_color_manual(breaks = c(TRUE, FALSE),
                       values=c("red", "grey")) +
    scale_fill_manual(breaks = c(TRUE, FALSE),
                      values=c("red", "grey")) +  
    theme_minimal() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.title = element_text(size = 20, hjust = 0, color = 'grey'),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()
    )
}

  


plot_demographics <- function(dat_all = dat_raw_all, 
                              ps = 'Public Service',  
                              depart = DEPART_OF_INEREST,
                              bycond_starts_with = 'Q117', 
                              curr_surveyr  = max(dat_all$surveyr),
                              curr_q = curr_question
                              ){
                                
  #dat_all |> filter(str_detect(bycond, glue('^{bycond_starts_with}') )) |> distinct(bycond , demcode, descrip_e) |> filter(!is.na(bycond) & !is.na(demcode)) |> View()
  curr_q_dat <- 
    dat_all |>
    filter(question == curr_q) |>
    filter(dept_e %in% c(ps, depart)) |> 
    filter(str_detect(bycond, glue('^{bycond_starts_with}') )) |>
    filter(!is.na(answer1), !is.na(anscount)) |>
    filter(surveyr == curr_surveyr)
      
  curr_q_dat_err <- 
  bind_cols(
    curr_q_dat,  
    binconf((curr_q_dat$answer1/100)*curr_q_dat$anscount , curr_q_dat$anscount ) |> as_tibble() |> clean_names() |>mutate(typ = 'pos')
  )
  
  curr_q_dat_err |>
    mutate(descrip_e = iconv(descrip_e, to = 'UTF-8', from = 'ASCII')) |>
    ggplot(aes( x = point_est , y = descrip_e, color = dept_e, fill = dept_e     )) +
    #geom_col() +
    geom_segment(aes(yend = descrip_e, xend = lower, x = upper ), size = 2) +
    #geom_label(aes(label = glue('{point_est*100}%')), color = 'white') +
    geom_label(aes(x = lower, label = glue('{round(lower*100,0)}%')), fill = 'white', alpha = 1) +
    geom_label(aes(x = upper, label = glue('{round(upper*100,0)}%')), fill = 'white', alpha = 1) +
    geom_text(aes(label = glue('{descrip_e}')), hjust = 0.5, nudge_y = 0.2, size = 5) +
    facet_grid(cols = vars(dept_e)) +
    guides(color = 'none',fill = 'none') +
    labs(title = str_wrap(curr_q_dat_err$title_e |> unique(), 60), subtitle = glue('Demographic code, {bycond_starts_with}'),
         
         y = '') +
    scale_color_manual(breaks = c('Public Service', DEPART_OF_INEREST, 'other'),
                       values=c("black", "red", 'grey')) +
    scale_fill_manual(breaks = c('Public Service', DEPART_OF_INEREST, 'other'),
                       values=c("black", "red", 'grey')) +    
    theme_bw() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          strip.text = element_text(size=15, color = 'grey'),
          strip.background = element_rect(colour="black", fill="white"),
          plot.title = element_text(size = 20, hjust = 0, color = 'grey'),
          plot.subtitle = element_text(size = 15, hjust = 0, color = 'grey'),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()
    )
  
}
  

curr_question <- 'Q57'
bycond_starts_with ='Q117'
plot_q_yr_depart(dat_raw_all, curr_q = 'Q55')
plot_q_yr_depart(dat_raw_all, curr_q = curr_question)
plot_q_yr_depart(dat_raw_all, curr_q = 'Q64')
plot_q_yr_depart(dat_raw_all, curr_q = 'Q70')
plot_q_yr_depart(dat_raw_all, curr_q = 'Q71')
plot_departs(dat_raw_all, curr_q = curr_question)
plot_departs(dat_raw_all, curr_q = 'Q64')
plot_departs(dat_raw_all, curr_q = 'Q55')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q81')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q93')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q102')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q103')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q104')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q106')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q109')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q112')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q113')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q116')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'Q117')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'D118')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'D119')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'D120')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'D121')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'D122')
plot_demographics(dat_raw_all, curr_q = curr_question, bycond_starts_with = 'D105')

Q64 




dat_raw_all |> distinct(question , title_e ) |> filter(str_detect(title_e, 'disc'))

curr_q_dat_depts |>

  filter(!is.na(anscount )) 
  
dat_raw_all |> count(bycond)
dat_raw_all |> count(demcode)
dat_raw_all |> count(bycond,demcode) |>group_by(demcode) |> summarise(n=n(), bycond = paste0(bycond , collapse = ',')) |> arrange(n , decreasing = TRUE) |> View()
curr_q_dat |> 

dat_raw_all |> count(bycond,demcode, question)
  
  
curr_q_dat |> filter(descrip_e != dept_e)
curr_q_dat |> filter(level2id == '0') |> count(level2id, bycond , demcode )

curr_q_dat |> View()
# dat_raw |>
#   #filter(anscount == min(anscount, na.rm = TRUE)) |>
#   select(matches('level.*id'), matches('^answer'),anscount, surveyr, bycond, demcode, descrip_e, dept_e, question) |>
#   filter(level5id != '000') |>
#   filter(level5id != '0') |>  
#   mutate(across(matches('^answer'), \(.x){as.integer(.x)})) |>
#   #mutate(across(matches('^answer'), \(.x){(as.integer(.x)*as.integer(anscount))/100})) |>
#   pivot_longer(cols = matches('^answer')) |>
#   filter(value > 0) |>
#   filter(value != 9999) |>
#   filter(question == 'Q55') |>
#   #filter(value == 100) |>
#   filter(name == 'answer1') |>
#   arrange(desc(value)) |> View()
#   head(20) 
  

#Question 56b. From whom did you experience harassment on the job? Individuals with authority over me
q56b <- 

    #filter(level5id != '000') |>
    #filter(level5id != '0') 
  

  DEPART_OF_INEREST <- 'Canada School of Public Service'
level1id_OF_INTERST <- c('00', '78')

# theme_set(theme_classic())
# theme_set()

######################################################
#
# Response counts !
#
dat_raw |>
  select(level1id, dept_e, descrip_e, anscount, bycond , surveyr) |>
  filter(surveyr  == '2020') |>
  mutate(anscount = as.integer(anscount )) |>
  group_by(level1id, dept_e, descrip_e, bycond) |>
  summarise(n = max(anscount, na.rm = TRUE)) |>
  mutate(n = pmax(0, n)) |>
  ungroup() |>
  arrange(desc(n)) |>
  filter(str_detect(bycond, '(Q120|Q115|Q116|Q118|Q122)')) |>
  filter(level1id %in% level1id_OF_INTERST) |>
  separate(col = bycond, into = c('q','a'), sep = ' = ') |>
  group_by(dept_e, level1id, q) |>
  mutate(n_sum = sum(n, na.rm = TRUE), f = n/sum(n, na.rm = TRUE)) |> ungroup() |>
  mutate(descrip_e = fct_reorder(.f = descrip_e, .x = f)) |>  
  mutate(lbl = paste0(round(f*100, 1), "%")) |>
  ggplot(aes(y = descrip_e, x = f, color = q, label = lbl)) +
  geom_segment(aes(yend = descrip_e, xend = 0), size = 1)+
  geom_label(size = 4) +
  
  facet_grid(cols = vars(dept_e), rows = vars(q), scales = 'free_y') +
  guides(label = 'none', color = 'none') +
  scale_x_continuous(labels = scales::percent) +
  labs(title = 'Response Counts from PSES 2020', y ='', x = '')
  
#   count(bycond)
#   
#   
#   distinct(dept_e)
# 
# Q121
# Q121

#Question 55. Having carefully read the definition of harassment, have you been the victim of harassment on the job in the past 12 months?   
q55 <-
  dat_raw |>
  filter(str_detect(question, 'Q55')) |> 
  select(matches('level.*id'), matches('^answer'),anscount, surveyr, bycond, demcode, descrip_e, dept_e, question) |>
  filter(!is.na(anscount )) |>
  filter(surveyr  == '2020') |>
  mutate(across(matches('^answer'), \(.x){as.integer(.x)})) |>
  mutate(across(matches('^anscount'), \(.x){as.integer(.x)})) |>
  mutate(across(matches('^surveyr'), \(.x){as.integer(.x)})) 
  #filter(level5id != '000')# |>
  #filter(level5id != '0') 




#Question 62. Having carefully read the definition of discrimination, have you been the victim of discrimination on the job in the past 12 months? 
q62 <-
  dat_raw |>
  filter(str_detect(question, 'Q62')) |> 
  select(matches('level.*id'), matches('^answer'),anscount, surveyr, bycond, demcode, descrip_e, dept_e, question) |>
  filter(!is.na(anscount )) |>
  filter(surveyr  == '2020') |>
  mutate(across(matches('^answer'), \(.x){as.integer(.x)})) |>
  mutate(across(matches('^anscount'), \id <- 'recHPMhnwiZ2CJtUM'
                dat <-
                  db_tbl_cleaned('survey') |>
                  filter(question_id == 'rating_overall_satisfaction') |>
                  count(offering_id, answer_rating) |>
                  group_by(offering_id) |>
                  mutate(f = n/sum(n)
                  ) |>
                  ungroup() |>
                  full_join(db_tbl_cleaned('bridge_event_brightspace'), by = join_by(offering_id))
                
                
                dat |>
                  group_by(event_id) |>
                  summarise(mean_rating = weighted.mean(x=answer_rating,w= n)) |>
                  (.x){as.integer(.x)})) |>
  mutate(across(matches('^surveyr'), \(.x){as.integer(.x)})) 
#filter(level5id != '000')# |>
#filter(level5id != '0') 




#Question 23. In my work unit, I would feel free to speak about racism in the workplace without fear of reprisal. 
q23 <-
  dat_raw |>
  filter(str_detect(question, 'Q23')) |> 
  select(matches('level.*id'), matches('^answer'),anscount, surveyr, bycond, demcode, descrip_e, dept_e, question) |>
  filter(!is.na(anscount )) |>
  filter(surveyr  == '2020') |>
  mutate(across(matches('^answer'), \(.x){as.integer(.x)})) |>
  mutate(across(matches('^anscount'), \(.x){as.integer(.x)})) |>
  mutate(across(matches('^surveyr'), \(.x){as.integer(.x)})) 
#filter(level5id != '000')# |>
#filter(level5id != '0') 

# 
# 
# 
# f_gender <- 
#   dat_raw |> 
#   filter(str_detect(bycond, '^Q115')) |> 
#   mutate(anscount = as.integer(anscount)) |>
#   #count(bycond, descrip_e)
#   #filter(bycond%in% c('Q115 = 1', 'Q115 = 2')) |>
#   group_by(bycond, descrip_e, dept_e) |>
#   summarise(anscount_max = max(anscount, na.rm = TRUE), .groups = 'drop') |>
#   mutate(anscount_max = if_else(
#     is.infinite(anscount_max), 0, anscount_max 
#   )) |>
#   group_by(dept_e) |> 
#   mutate(num_response = sum(anscount_max)) |>
#   ungroup() |>
#   mutate(f = anscount_max / num_response) |>
#   arrange(dept_e)
#   
  

#q_to_use <- 'discrimination'
#q_to_use <- 'harassment'
q_to_use <- 'racism'


data_to_use <- 
  if (q_to_use == 'discrimination'){q62
  }else if (q_to_use == 'harassment'){q55
  }else if (q_to_use == 'racism'){q23
  }else{NULL}


q_wording <- 
  if (q_to_use == 'discrimination'){'Question 62. Having carefully read the definition of discrimination, have you been the victim of discrimination on the job in the past 12 months?' 
  }else if (q_to_use == 'harassment'){'Question 55. Having carefully read the definition of harassment, have you been the victim of harassment on the job in the past 12 months?'
  }else if (q_to_use == 'racism'){'Question 23. In my work unit, I would feel free to speak about racism in the workplace without fear of reprisal.'
  }else{NULL}





#f_gender |> distinct(dept_e) |> filter(str_detect(dept_e, 'School'))
data_to_use|> distinct(dept_e) |> filter(str_detect(dept_e, 'School'))


data_to_use_2 <-
  data_to_use |> 
  filter(str_detect(dept_e, 'School')) |>
  filter(descrip_e != dept_e) |>
  ungroup() |>
  mutate(across(matches('^answer'), 
                \(.x){
                  as.integer(anscount * (.x/100))
                })) |>
  mutate(pos = answer1+answer2, neg =answer4 + answer5) |>
  select(-matches('^answer')) |>
  select(-matches('^level\\did$')) |>
  separate(col = bycond, into = c('q','a'), sep = ' = ') |>
  mutate(q = str_extract(q, '^(Q|D)[:digit:]+')) |>
  select(-surveyr, -demcode, -dept_e , -question) |>
  mutate(delta = (pos - neg)/anscount) |>
  mutate(delta_n = pos - neg) |>
  group_by(q) |>
  mutate(delta_q = (max(pos) - min(neg))/sum(anscount)) |>
  mutate(delta_q_n = (sum(pos) - sum(neg))) |>
  #select(-anscount) |>
  arrange(delta_q, delta) |> ungroup() |>
  mutate(descrip_e = fct_reorder(descrip_e, delta_q*1000 + delta))

bind_rows(
  bind_cols(
    data_to_use_2 |> select(q, descrip_e),  
    binconf(data_to_use_2$pos, data_to_use_2$anscount) |> as_tibble() |> clean_names() |>mutate(typ = 'pos')
  ),
  bind_cols(
    data_to_use_2 |> select(q, descrip_e),  
    binconf(data_to_use_2$neg, data_to_use_2$anscount) |> as_tibble() |> clean_names() |>mutate(typ = 'neg')
  ) 
) |> 
  filter(q == 'Q121') |>
  #arrange(descrip_e, typ)
  ggplot(aes(x = lower, xend=upper, y = descrip_e, yend = descrip_e, color = typ)) +
  geom_segment() +
  facet_grid(rows = vars(q))
  
data_to_use |> 
  filter(str_detect(dept_e, 'School')) |>
  filter(descrip_e == dept_e)
  
  
  bind_cols(
    data_to_use_2 |> select(q, descrip_e),  
    binconf(data_to_use_2$delta_n, data_to_use_2$anscount) |> as_tibble() |> clean_names() |>mutate(typ = 'delta')
  ) |>
    filter(q %in% c('Q110',
                    'Q115', 
                    'Q116', 
                    'Q117', 
                    'Q118', 
                    'Q119', 
                    'Q120', 
                    'Q122'
                    )) |>
    ggplot(aes(x = lower, xend=upper, y = descrip_e, yend = descrip_e, color = q )) +
    geom_segment() +
    geom_label(mapping = aes(x = lower, label = paste0(round(lower*100,0)))) +
    geom_label(mapping = aes(x = upper, label = paste0(round(upper*100,0)))) +
    geom_vline(xintercept = 0.75) +
    facet_grid(rows = vars(q), scales = 'free_y') +
    labs(title = 'PSES 2020', subtitle = q_wording, x = 'Delta between Possitive and Negative Responses', y = '') +
    scale_x_continuous(labels = scales::percent) +
    guides(label = 'none', color = 'none')
  


  pivot_longer(cols = )

df <- 
#q55 |> 
  data_to_use |>
  filter(dept_e  =='Public Service') |>
  select(answer1, anscount, bycond, demcode , descrip_e, dept_e) |>
  mutate(x =as.integer(as.double(answer1)*as.double(anscount)/100)) |>
  mutate(n = anscount) |>
  select(x, n,bycond, demcode  ,  descrip_e, dept_e) |>
  separate(col = 'bycond', into = c('q','a'), sep = ' = ', remove = FALSE)


answers_words <- tibble(anwser = ordered(c('Strongly agree',	'Somewhat agree',	'Neither agree nor disagree', "Don't know",	'Not applicable','Somewhat disagree',	'Strongly disagree')),  
                        index =          c(    1,                   2,                   3,                      6,               7,               4,                     5), 
                        color_hex = c('#0a5d00',                '#1fc600',        	'#d2d4dc',	             '#f8f8fa',	      '#c0c2ce',      '#fe8181',	         '#b62020'))

color_scale_to_use  <- answers_words$color_hex |> setNames(answers_words$anwser)



data_to_use 


data_to_use |> 
  select(matches('^answer'), anscount, bycond, demcode , descrip_e, dept_e) |>
  mutate(across(matches('^answer'), as.integer))  |>
  select(-anscount, -bycond, -demcode) |>
  filter(descrip_e == dept_e) |>
  select(-dept_e) |>
  mutate(delta = answer1 +answer2 - answer4 - answer5) |>
  mutate(descrip_e = fct_reorder(descrip_e, delta)) |>
  select(-answer3, -answer6, -answer7) |>
  mutate(answer4 = -answer4, answer5 = -answer5) |>
  pivot_longer(cols = matches('^answer')) |>
  mutate(index = as.integer(str_remove(name, '^answer'))) |> 
  select(-name) |>
  left_join(answers_words, by = join_by(index)) |> 
  mutate(anwser = fct_relevel(anwser, 'Strongly agree',	'Somewhat agree',	'Neither agree nor disagree', "Don't know",	'Not applicable','Somewhat disagree',	'Strongly disagree')) |>
  distinct() |>
  ggplot(aes(x = value, y = descrip_e, fill = anwser)) +
  geom_col() +
  geom_text(aes(label = paste0('+',delta), x = -30)) +
  scale_fill_manual(values=color_scale_to_use) +
  labs(title = 'PSES 2020', subtitle = q_wording, x = '', y = '') 




  data_to_use |> 
    select(matches('^answer'), anscount, bycond, demcode , descrip_e, dept_e) |>
    mutate(across(matches('^answer'), as.integer))  |>
    select(-anscount, -bycond, -demcode) |>
    filter(descrip_e == dept_e) |>
    select(-dept_e) |>
    mutate(delta = answer1 +answer2 - answer4 - answer5) |>
    mutate(descrip_e = fct_reorder(descrip_e, delta)) |>
    pivot_longer(cols = matches('^answer')) |>
    mutate(index = as.integer(str_remove(name, '^answer'))) |> 
    select(-name) |>
    left_join(answers_words, by = join_by(index)) |> 
    mutate(anwser = fct_relevel(anwser, 'Strongly agree',	'Somewhat agree',	'Neither agree nor disagree', "Don't know",	'Not applicable','Somewhat disagree',	'Strongly disagree')) |>
    distinct() |>
    ggplot(aes(x = value, y = descrip_e, fill = anwser)) +
    geom_col() +
    scale_fill_manual(values=color_scale_to_use) +
    
    scale_color_identity(guide = "legend",
                         labels = answers_words$anwsers,
                         breaks = answers_words$color_hex) +
  

    
    
    
    
    
    
dat <- 
  bind_cols(df, binconf(df$x, df$n)) |>
  group_by(descrip_e, bycond , demcode ) |>
    mutate(lower_rank = rank(desc(Lower),   ties.method = 'random'), 
           upper_rank = rank(Upper,   ties.method = 'random'),
           nloc = n()) |>
  ungroup()


ps_mean <- 
  dat |>  
  filter(dept_e == 'Public Service' & descrip_e == 'Public Service') |> 
  pull(PointEst) |> 
  mean()

group_mean <- 
  dat  |> 
  filter(dept_e == 'Public Service' & descrip_e != 'Public Service') |>
  group_by(bycond   , descrip_e, q, a) |> 
  summarise(PointEst   = mean(PointEst)) |>
  ungroup() |>
  rename(all_of(c(group_mean = 'PointEst'))) 
  


dep_mean <- 
  dat  |> 
  filter(dept_e ==  descrip_e) |>#count(bycond   , descrip_e, dept_e, q, a, sort = T) |> 
  distinct() |>
  group_by(bycond   , dept_e, descrip_e, q, a) |> 
  summarise(PointEst   = mean(PointEst)) |>
  ungroup() |> 
  select(dept_e, PointEst) |>
  set_names(c('dept_e', 'dept_mean'))



q_for_graph <- c(
              'Q122', # sexual orientation
              #'Q121'#, # visible minority Type
              'Q120', # visible minority
              #'Q119'##, # disability type
              'Q118', # disability
              #'Q117'#, # Indigenous  type
              'Q116', # Indigenous
              'Q115' # Gender
                 )


q_for_graph_regex <- paste0('^(',paste0(q_for_graph, collapse = '|'), ')')
dept_for_graph <- c('Public Service','Canada School of Public Service')

d_g <- 
  dat |> 
  left_join(dep_mean, by = join_by(dept_e)) |>
  left_join(group_mean, by = join_by(bycond, q, a, descrip_e)) |> 
  #separate(col = 'bycond', into = c('q','a'), sep = ' = ', remove = FALSE) |> # distinct(bycond, q, a, demcode , descrip_e) |> View()
  filter(q %in% q_for_graph | str_detect(q, q_for_graph_regex)) |>
  filter(dept_e %in% dept_for_graph) |> #View()
  mutate(dept_e = paste0(dept_e,' (', round(dept_mean*100, 1), '%)')) |>  
  mutate(dept_e = fct_reorder(iconv(dept_e,to = 'UTF-8', from = 'ASCII'), dept_mean  )) |>
  mutate(descrip_e = paste0(descrip_e,' (', round(group_mean*100, 1), '%)')) |>  
  mutate(descrip_e = fct_reorder(iconv(descrip_e,to = 'UTF-8', from = 'ASCII'), PointEst  ))
# iconv(dataSet, 'UTF-8', 'ASCII')
# enc2utf8
d_g_lbl <-
d_g |> 
  select(q, descrip_e, dept_e, Lower, Upper) |>
  pivot_longer(cols = c('Lower','Upper')) |>
  mutate(lbl  = paste0(round(value*100, 1), '%'))

d_g |> 
  group_by(q, dept_e) |> mutate(num_q=n()) |> ungroup() |>
  group_by(q) |> mutate(num_q = max(num_q)) |> ungroup() |> 
  ggplot(aes(x = Lower, y = descrip_e , color = q))  +
  geom_segment(mapping = aes(xend= Upper, yend = descrip_e), size = 1.5) +
  geom_label(data = d_g_lbl, mapping = aes(x = value, label = lbl)) +
  geom_segment(mapping = aes(x = dept_mean, xend = dept_mean, y = 0, yend = num_q+1), color = 'black',   linetype ='dashed' ) +
  geom_vline(xintercept = ps_mean) +
  geom_point(aes(x = group_mean), color = 'black', shape = 4, size = 4) +
  facet_grid(rows = vars(q), cols = vars(dept_e), scales = 'free_y') +
  guides(color = 'none') +
  labs(title = 'PSES 2020', subtitle = q_wording, x = '', y = '') +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.35))



'Q85' # manage remote employees
'Q83' # supervisor

'Q122' # sexual orientation

'Q121' # visible minority Type
'Q120' # visible minority
'Q119' # disability type

'Q118' # disability
'Q117' # Indigenous  type
'Q116' # Indigenous
'Q115' # Gender
'Q114' # Age
'Q113' # Region (Dont use)
'Q112' # Province
'Q111' # services to public
'Q110' # first Official Language
'D109' # Time at current department
'D108' # Time in GOC
'D106' # type of position
'Q100' # Shift
'Q101' # Part time
'Q102' # Remote work
'Q103' # LWP 699
'Q104' # LWP 699 2
'Q105' # Student Seasonal casual term
'Q107' # type of work DS IT health, security etc

dat_2 <- 
  dat |> 
  #filter(nloc > 10) |>
  #ungroup() |>
  #arrange(descrip_e, Upper_rank) |>  
  #distinct(bycond, descrip_e) |> View()
  filter(str_detect(bycond, 'Q116')) |>
  mutate(best = upper_rank <= 3, worste = lower_rank <= 3) |>
  #filter(best | worste) |>
  mutate(type = 
           case_when(best ~ 'best', 
                     worste ~ 'worste',
                     TRUE ~ 'other')) |>
  mutate(dept_e = fct_reorder(dept_e, PointEst)) |>
  group_by(dept_e) |> 
  mutate(any_best = any(best), any_worst = any(worste)) |>
  ungroup() |>
  mutate(lowest_of_ranks = pmin(lower_rank, upper_rank)) |>
  group_by(dept_e) |> 
  mutate(lowest_of_ranks = min(lowest_of_ranks)) |>
  ungroup() |>
  arrange(lowest_of_ranks) |>
  #head(10) |>
  #filter(lowest_of_ranks <= 2) |>
  #filter(any_best | any_worst) |>
  ungroup() |>
  inner_join(group_mean, by = c("descrip_e", "bycond"),  suffix = c("", ".group_mean") ) |> #distinct(descrip_e)
  inner_join(dep_mean,  by = c("dept_e"),suffix = c("", ".dept_mean") )







n_y <- dat_2 |> distinct(dept_e) |> nrow()

dat_2 |> 
  #select (bycond, demcode ) |> 
  inner_join(group_mean, by = c("descrip_e", "bycond"),  suffix = c("", ".group_mean") ) |> #distinct(descrip_e)
  inner_join(dep_mean,  by = c("dept_e"),suffix = c("", ".dept_mean") ) |>
  mutate(dept_e = fct_reorder(dept_e, PointEst.dept_mean)) |>
  mutate(descrip_e = glue('{descrip_e} ({round(PointEst.group_mean*100,0)}%)')) |> 
  mutate(descrip_e = fct_reorder(descrip_e, PointEst.group_mean)) |>
  mutate(type = case_when(
    dept_e == 'Canada School of Public Service' ~ 'us',
    .default = type
  )) |>
  #pull(descrip_e)
  #filter(descrip_e == 'Visible minority') |> 
  ggplot(aes(y = dept_e, yend = dept_e, x = Lower  , xend = Upper, color = type, fill = type  )) +
  geom_segment(size = 1) +
  geom_point(shape = 21, size = 3) +
  geom_point(aes(x = Upper), shape = 21, size = 3, alpha = 0.5) +
  geom_point(aes(x = PointEst.dept_mean), 
             color = 'black', 
             shape = 4,  #cross
             size = 5, 
             alpha = 0.5) +
  geom_vline(xintercept = ps_mean) +
  geom_segment(mapping = aes(y = 0, yend = n_y+1, x = PointEst.group_mean, xend = PointEst.group_mean), 
               color = 'black', 
               linetype ='dashed' # GROUP MEAN is DASHED
               ) +
  facet_wrap(vars(descrip_e)) + 
  labs(title = q_wording, y = '', x = '') +
  guides(color = 'none', fill = 'none', shape = 'none')
    
    




or(c(TRUE TRUE TRUE))
    
    Q28 Q112 m Q114 ,Q115, Q116,  Q117 , Q118 , Q120 , Q119A , Q121A ,Q83  
  view()


Q119

c(0.002, 0.11, 0.30) |> desc() |> rank()

|> View()








|>
  mutate(sem =binconf(x = x, n = answer1, method = 'wilson'))
  mutate(sem = sqrt((x*(anscount -x) )/ (anscount^3)
    
    ))
  group_by(bycond, descrip_e) |>
  

    
    
    
    
c(rep(1, 11), rep(0, 89)) |> sd()
c(rep(1, 11), rep(0, 89)) |> mean()
Q115 = 1
Q115 = 2
q55 |> 
  #filter(dept_e == descrip_e) |> 
  filter(dept_e == 'Canada School of Public Service') |> 
  #distinct(bycond, descrip_e) |> View()
  filter(str_detect(bycond, 'LEVEL3ID')) |> 
  select(bycond, dept_e, descrip_e, answer1, anscount) 


  View()
  filter(anscount > 1000) |>
  #View()
  select(answer1, dept_e) |>
  mutate(dept_e  = fct_reorder(dept_e, answer1)) |>
  left_join(f_gender, by = 'dept_e') |>
  filter(bycond == 'Q115 = 1') |> #View()
  ggplot(aes(x = answer1, y = f)) +
  geom_point()
  geom_density()



q55 |> distinct(descrip_e)


inner_join(q55, q56b, by =c('level1id','level2id','level3id','level4id','level5id', 'surveyr', 'bycond', "demcode", 'descrip_e', 'dept_e'), 
           suffix = c("_55", "_56b")) |> 
            mutate(across(matches('^answer'), \(.x){as.integer(.x)})) |>
            mutate(across(matches('^anscount'), \(.x){as.integer(.x)})) |>
            mutate(across(matches('^surveyr'), \(.x){as.integer(.x)})) |>
            View()



count(answer1, answer2, anscount) |>
    
    
    
    arrange(anscount)
    filter(level5id != '000') |>
    filter(level5id != '0') |>    
  
  #filter(anscount == min(anscount, na.rm = TRUE)) |>
  #  filter(level5id == '501') |>
  #  filter(level4id == '444') |>
  #  filter(level3id == '311') |>
  #  filter(bycond == 'LEVEL5ID = 501') |> 
    select(matches('level.*id'), matches('^answer'),anscount, surveyr, bycond, demcode, descrip_e, dept_e, question) |>
    mutate(across(matches('^answer'), \(.x){as.integer(.x)})) |>
    #mutate(across(matches('^answer'), \(.x){(as.integer(.x)*as.integer(anscount))/100})) |>
    pivot_longer(cols = matches('^answer')) |> View()
    
      #filter(str_detect(question, 'Q56')) |>  
    #filter(bycond == 'LEVEL5ID = 501') |>  
    filter(value > 0) |>
    filter(value != 9999) |> 
  
  
  
  
group_by_all() |>
  summarise(n = n()) |>
  arrange(desc(n))
  count(matches('level.*id'), bycond, demcode, descrip_e, sort = T)


dat <- read_csv(fn)  


dat$ANSCOUNT |> min(na.rm = TRUE)


dat |> 
  ggplot(aes(x = ANSCOUNT)) + 
  geom_density() + 
  scale_x_log10()


dat_m <- 
dat |>
  filter(ANSCOUNT == min(ANSCOUNT, na.rm = TRUE)) 
  


dat_m |>
  count(DEMCODE, DESCRIP_E, sort = T)
