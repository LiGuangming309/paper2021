#if (rlang::is_empty(args)) {
  year <- 2009
  agr_by <- "nation"
  source <- "nvss"
  
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  pafDir <- "/Users/default/Desktop/paper2021/data/07_paf"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/13_attr_burd"
  
  dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
  tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  pafDir <- "C:/Users/Daniel/Desktop/paper2021/data/07_paf"
  totalBurdenParsed2Dir <- "C:/Users/Daniel/Desktop/paper2021/data/12_total_burden_parsed2"
  attrBurdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/13_attr_burd"
#}


total_burdenDir <- file.path(totalBurdenParsed2Dir,agr_by,source) 
files <- list.files(total_burdenDir)
total_burden <- lapply(files, function(file) fread(file.path(total_burdenDir, file))) %>% rbindlist

total_burden <- total_burden %>% filter(measure1 == "Deaths" & Education == 666)

test <- total_burden %>%
  select(Year,measure2, min_age, max_age) %>%
  distinct

test2 <- test %>%
  group_by(Year, measure2) %>%
  summarise(n = n())

test3 <- test %>%
  filter(Year %in% 2008:2010 & measure2 == "age-adjusted rate")
