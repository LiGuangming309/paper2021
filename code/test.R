any(is.na(pafs2))
pafs2 <- left_join(pafs,census_meta, by= c("censMeta.variable"="variable"))


pafs3 <- left_join(pafs,census_meta, by= c("censMeta.variable"="variable"))
any(is.na(pafs3))
