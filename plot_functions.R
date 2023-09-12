

plot_q_yr_depart <- function(dat_all,
                             ps = 'Public Service',  
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
  
  lbl_dat <- p_dat |> group_by(descrip_e, dept_e, highlight) |> summarise(surveyr  = mean(range(surveyr)), point_est = mean(range(point_est))) |>arrange(desc(point_est) )
  #lbl_dat$point_est = c(quantile(range(p_dat$point_est), 0.90), quantile(range(p_dat$point_est), 0.8))
  
  p_dat |>
    ggplot(aes(x = surveyr , y = point_est , group = descrip_e, color = highlight )) +
    scale_color_manual(breaks = c('Public Service', DEPART_OF_INEREST, 'other'),
                       values=c("black", "red", 'white')) +
    scale_size_manual(breaks = c('Public Service', DEPART_OF_INEREST, 'other'),
                      values=c(1, 1, 0.01)) +  
    geom_line(aes(size = highlight)) +
    geom_point(aes(size = highlight)) +
    #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 1) +
    geom_label(data = p_dat |> filter(surveyr  %in% range(surveyr)), mapping = aes( label = glue('{point_est*100}%'))) +
    geom_label_repel(data = lbl_dat, alpha = 1, mapping = aes( label = glue('{dept_e}')), size = 6, hjust = 0.5) +
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
    ) |>
    mutate(mid_point = (lower + upper)/2)
  
  curr_q_dat_err |>
    mutate(descrip_e = iconv(descrip_e, to = 'UTF-8', from = 'ASCII')) |>
    ggplot(aes( x = point_est , y = descrip_e, color = dept_e, fill = dept_e     )) +
    #geom_col() +
    geom_segment(aes(yend = descrip_e, xend = lower, x = upper ), size = 2) +
    #geom_label(aes(label = glue('{point_est*100}%')), color = 'white') +
    geom_label(aes(x = lower, label = glue('{round(lower*100,0)}%')), fill = 'white', alpha = 1) +
    geom_label(aes(x = upper, label = glue('{round(upper*100,0)}%')), fill = 'white', alpha = 1) +
    geom_text_repel(aes(label = glue('{str_wrap(descrip_e, 60)}'), x = mid_point), 
              hjust = 0.5, 
              nudge_y = 0.2, 
              size = 5) +
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
