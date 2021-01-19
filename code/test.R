test <- censData_joined[is.na(censData_joined$state), ] %>%
  mutate(
    GEO_ID = GEO_ID %>%
      as.character() %>%
      str_pad(., 11, pad = "0"),
    state = str_sub(GEO_ID, 1, 2),
    county = str_sub(GEO_ID, 3, 6),
    tract = str_sub(GEO_ID, 7, 11)
  )

# 06031001702,Cali,0310,01702
#6109005202

test <- censData_joined %>%
  mutate(
    GEO_ID = GEO_ID %>%
      as.character() %>%
      str_pad(., 11, pad = "0"),
    state = sapply(state, function(x) {
      ifelse(is.na(x),
        str_sub(GEO_ID, 1, 2),
        x
      )
    })
    # county = str_sub(GEO_ID,3,6),
    # tract = str_sub(GEO_ID,7,11)
  )

#co 830425