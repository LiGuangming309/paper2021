interp <- approx(exp_mrbrt$exposure_spline,
              exp_mrbrt$mean,
              xout = aim)

test(2)
