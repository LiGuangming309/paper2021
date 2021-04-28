summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
attrBurden <- rbind(fread(file.path(summaryDir, "attr_burd.csv")), 
                    fread(file.path(summaryDir, "attr_burd_prop.csv")))
all_burden <- fread(file.path(summaryDir, "all_burd.csv"))

test1 <- attrBurden %>% filter(measure3 == "value",
                              Year == 2005,
                              measure1 == "YLL",
                              measure2 == "crude rate per 100,000",
                              source == "Mortality Data from CDC WONDER",
                              method == "GBD",
                              Gender.Code == "All genders",
                              Region == "United States")

test2 <- all_burden %>% filter(attr == "overall",
                              Year == 2005,
                              measure1 == "YLL",
                              measure2 == "crude rate per 100,000",
                              source == "Mortality Data from CDC WONDER",
                              Gender.Code == "All genders",
                              Region == "United States")

test3 <- attrBurden %>% filter(measure3 == "proportion of disparity to White, Not Hispanic attributable",
                              measure1 == "YLL",
                              measure2 == "crude rate per 100,000",
                              source == "Mortality Data from CDC WONDER",
                              method == "GBD",
                              Gender.Code == "All genders",
                              Region == "United States",
                              Year == 2005)
(508.2409- 477.5711)/(22111.935-20393.489)*100
