#########
### Obtaining the play-by-play data for 2014-2020 CFB seasons from "cfbfastr" R package,
### saving them into "pwp_on_campus_full_data.Robj" file.
#########

if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

library(pacman)
pacman::p_load(tidyverse, cfbfastR, zoo, ggimage, gt)


# usethis::edit_r_environ()


# https://community.rstudio.com/t/readrds-url-error/49323


tictoc::tic()
pbp <- data.frame()
seasons <- 2014:cfbfastR:::most_recent_season()

progressr::with_progress({
  
  # Resolves futures asynchronously (in parallel) in separate R sessions running 
  # in the background on the same machine.
  future::plan("multisession")
  
  # Only worked while ON CAMPUS...
  pbp <- cfbfastR::load_cfb_pbp(seasons)
  
  
  # pbp <- NULL
  # for (season in seasons){
  #   # print(season)
  #   pbp <- rbind(pbp,
  #                cfbfastR::cfbd_pbp_data(season))
  # }
  
  
})
tictoc::toc()


## ON-CAMPUS, the ENTIRE DATA SET
dim(pbp)
save(pbp, file= "pbp_on_campus_full_data.Robj")
