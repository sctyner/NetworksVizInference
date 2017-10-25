# create trials for experiment

# 10 total
# 5 most complex
# 5 most simple 

details_tri <- data_frame(data_file = list.files("SenateLineupData/trials/"))

details_tri <- details_tri %>% mutate(data_name = data_file) %>% 
  mutate(data_file = str_replace(data_file, ".csv", "")) %>%
  separate(data_file, c("model", "type", "rep"), "_") 
details_tri
details_tri$model <- as.factor(details_tri$model)
levels(details_tri$model) <- c(7,3,4,6,5)
details_tri$model <- as.integer(as.character(details_tri$model))
details_tri$type <- as.factor(details_tri$type)
levels(details_tri$type) <- c(1, 5)
details_tri$type <- as.integer(as.character(details_tri$type))

get_dat_plot_tri <- function(data_name){
  dat <- read_csv(paste0("SenateLineupData/trials/", data_name))
  sims <- unique(dat$sim)
  datplot <- setdiff(sims, 1:5)
  unique(dat$ord[which(dat$sim == datplot)])
}

details_tri %>% mutate(obs_plot_location = map_int(data_name, get_dat_plot_tri)) -> details_tri
details_tri %>% mutate(pic_id = as.integer(paste0(model,type,rep))) -> details_tri

senate_lu_storage3 <- function(details, url = "http://www.hofroe.net/examples/lineup/action-back.js"){
  #  browser()
  picture_details <- data_frame(pic_id = integer(0), sample_size = integer(0), test_param = character(0),
                                param_value = character(0), p_value = numeric(0), obs_plot_location = integer(0),
                                pic_name = character(0), experiment = character(0), difficulty = character(0), data_name = character(0))
  
  for(i in 1:nrow(details)){
    dat <- read_csv(paste0("SenateLineupData/trials/", details$data_name[i]))
    p <-ggplot(data = dat) + 
      geom_net(aes(from_id = from, to_id = to), 
               arrow = arrow(type = 'open', length = unit(2, "points") ), 
               linewidth = .25, singletons = T, fiteach = T, directed = T, 
               color = 'black', arrowgap = .015, arrowsize = .3, size = .3) + 
      theme_net() + 
      theme(panel.background = element_rect(color = 'black')) +
      facet_wrap(~ord)
    savepdf(file = paste0("Lineup-Images/senate/ForExperiment10-15/pdfs/trial-senate-mod-",
                          details$model[i], '-type-', details$type[i],  '-rep-', 
                          details$rep[i]), width = 7.2*2.54, height = 4.5*2.54)
    print(p)
    tmpfile <- sprintf("%s.svg",tempfile(tmpdir="Lineup-Images/senate/ForExperiment10-15/trials/"))
    make_interactive(filename2 = tmpfile, script=url,  
                     high="#d5d5d5",  background="#ffffff")
    dev.off()
    picture_details <- picture_details %>% add_row(
      pic_id = details$pic_id[i], sample_size = NA, test_param = details$model[i],
      param_value = details$type[i], p_value = NA, obs_plot_location = details$obs_plot_location[i],
      pic_name = tmpfile, experiment = "turk22", difficulty = NA, data_name = details$data_name[i])
    
  }
  return(picture_details)
}

trial_details <- senate_lu_storage3(details = details_tri)
write_csv(trial_details, "trial_details.csv")
