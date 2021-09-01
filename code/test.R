df <- data.frame(A = c(56L, NA, NA, 67L, NA),
                     B = c(75L, 45L, 77L, 41L, 65L),
                     Year = c(1921L, 1921L, 1922L, 1923L, 1923L))
df                                                                             
df %>% 
  mutate(A = coalesce(A,B))


setdiff(1:56,states$STATEFP)

totalBurdenDir <- "/Users/default/Desktop/paper2021/raw_restricted_fake"
# Where the parsed files should be stored
totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/raw_restricted_fake"

#### ----- ---
file_list <- list.files(totalBurdenDir)
agr_bys <- c("nation", "STATEFP")
years <- 2003:2016

findreplace <- read.csv("https://raw.github.com/FridljDa/paper2021/master/data/09_total_burden_parsed/findreplace.csv")
causes <- read.csv("https://raw.github.com/FridljDa/paper2021/master/data/09_total_burden_parsed/causes.csv")

for (year in years) {
  findreplaceX <- findreplace %>% filter(Year == year)
  
  totalBurdenDirX <- file.path(totalBurdenDir, file_list[grepl(year, file_list)])
  ## ----- read total burden ---------
  # total_burden <- narcan:::.import_restricted_data(totalBurdenDirX, year = year, fix_states = FALSE)
  total_burden <- data.table::fread(cmd = paste("unzip -p", totalBurdenDirX))
  print(year)
  interested_names <- c("staters" ,"fipsctyr" ,  "countyrs")
  
  #print(intersect(colnames(total_burden), interested_names))
  print(unique(total_burden$staters))
}