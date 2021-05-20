paf_burnett1 <- pm_summ_pop %*% outer(
  colnames(pm_summ_pop) %>% as.numeric(),
  thetas,
  function(pm, theta) {
     burnett_gemm(pm, 0.1430, 1.6, 15.5, 36.8) - 1 #TODO
    #1 - 1 / burnett_gemm(pm, theta, 1.6, 15.5, 36.8)
  }
)
paf_burnett2 <- pm_summ_pop %*% outer(
  colnames(pm_summ_pop) %>% as.numeric(),
  thetas,
  function(pm, theta) {
    # burnett_gemm(pm, 0.1430, 1.6, 15.5, 36.8) - 1 #TODO
    1 - 1 / burnett_gemm(pm, theta, 1.6, 15.5, 36.8)
  }
)

cbind(paf_burnett1, paf_burnett2)
