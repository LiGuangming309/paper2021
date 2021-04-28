
summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
attrBurden <- rbind(fread(file.path(summaryDir, "attr_burd.csv")), 
                    fread(file.path(summaryDir, "attr_burd_prop.csv")))

test <- attrBurden %>% filter(measure3 == "proportion of disparity to White, Not Hispanic attributable",
                      measure1 == "YLL",
                      measure2 == "crude rate per 100,000",
                      source == "Mortality Data from CDC WONDER",
                      method == "GBD",
                      Gender.Code == "All genders",
                      Region == "United States")

nrow(test)/17

nrow(attrBurden_disp1)*4