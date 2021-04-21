
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: parent file for project
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# install packages if missing
packages <- c(
  "bit64", "cdcfluview", "censusapi", "data.table", "dplyr", "ggplot2", "magrittr", "matrixStats",
  "MALDIquant", "plyr", "RCurl", "readxl", "sf", "sp", "stringr", "testthat", "tictoc",
  "tidyverse", "tigris", "tmap", "viridis", "hrbrthemes", "rlang", "stats", "xlsx"
)

options(tigris_use_cache = FALSE)
for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

# download rhdf5
if ("rhdf5" %in% rownames(installed.packages()) == FALSE) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  BiocManager::install("rhdf5")
}

# download DataCombine
if ("DataCombine" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("christophergandrud/DataCombine")
}
# runtime configuration
# run cripts from command line depending on OS
if (Sys.info()["sysname"] == "Darwin") {
  runscript <- function(script, args = "") {
    system(paste("Rscript", script, args))
  }
} else if (Sys.info()["sysname"] == "Windows") {
  memory.limit(size = 500000)

  exec <- paste0("C:/Program Files/R/R-", R.Version()$major, ".", R.Version()$minor, "/bin/Rscript.exe")
  exec <- shQuote(exec)
  runscript <- function(script, args = "") {
    system(paste(exec, "--vanilla", script, args))
  }
} else {
  print(paste("no handler for", Sys.info()["sysname"], "implemented yet."))
}

## ----------------directories--------------------------------------------------------------------------------
# create data directory, setwd
code.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
h_root <- dirname(code.dir)
setwd(h_root)

# create directory, where all downloaded and calculated data is stored
data.dir <- file.path(h_root, "data")
dir.create(data.dir, recursive = T, showWarnings = F)


# directory contains variables used in calculations, which several scripts might need
tmp.dir <- file.path(data.dir, "tmp")
dir.create(tmp.dir, recursive = T, showWarnings = F)

# directory for downloaded PM exposure data
exp.dir <- file.path(data.dir, "01_exposure")
dir.create(exp.dir, recursive = T, showWarnings = F)


# directory for downloaded TIGER/Line tract shape files
trac.dir <- file.path(data.dir, "02_tracts")
dir.create(trac.dir, recursive = T, showWarnings = F)

# this directory contains calculated year - census tract - pm level tuples
trac.exp.dir <- file.path(data.dir, "03_exp_tracts")
dir.create(trac.exp.dir, recursive = T, showWarnings = F)

exp.rr.dir <- file.path(data.dir, "04_exp_rr")
if (!file.exists(exp.rr.dir)) warning("The mrbrt_summary files from Cohen (2019) need to be downloaded")

# directory for downloaded demographic census data
dem.dir <- file.path(data.dir, "05_demog")
dir.create(dem.dir, recursive = T, showWarnings = F)

# directory for demographic data grouped by PM exposure and aggregated by county/hhs region/census region
dem.agr.dir <- file.path(data.dir, "06_dem.agr")
dir.create(dem.agr.dir, recursive = T, showWarnings = F)
agr_bys <- c("nation","STATEFP") # c("county","Census_Region","Census_division","hhs_region_number","STATEFP","nation")

paf.dir <- file.path(data.dir, "07_paf")
dir.create(paf.dir, recursive = T, showWarnings = F)

total.burden.dir <- file.path(data.dir, "08_total_burden")
if (!file.exists(total.burden.dir)) warning("The total burden data from CDC wonder need to be downloaded")

total.burden.parsed.dir <- file.path(data.dir, "09_total_burden_parsed")
dir.create(total.burden.parsed.dir, recursive = T, showWarnings = F)
sources <- c("wonder"
             ,"nvss"
             )

cdc.pop.dir <- file.path(data.dir, "10_cdc_population")
if (!file.exists(cdc.pop.dir)) warning("The population data from CDC wonder need to be downloaded")

pop.summary.dir <- file.path(data.dir, "11_population_summary")
dir.create(pop.summary.dir, recursive = T, showWarnings = F)

total.burden.parsed2.dir <- file.path(data.dir, "12_total_burden_parsed2")
dir.create(total.burden.parsed2.dir, recursive = T, showWarnings = F)

attr.burden.dir <- file.path(data.dir, "13_attr_burd")
dir.create(attr.burden.dir, recursive = T, showWarnings = F)

summary.dir <- file.path(data.dir, "14_summary")
dir.create(summary.dir, recursive = T, showWarnings = F)

plot.dir <- file.path(data.dir, "15_plot")
dir.create(plot.dir, recursive = T, showWarnings = F)


# paths of scripts
mrbrtRR.script <- file.path(code.dir, "01_mrbrt_rr.R")
download.meta.script <- file.path(code.dir, "02_download_meta.R")
meta.cross.script <- file.path(code.dir, "03_meta_cross.R")
download.cens.script <- file.path(code.dir, "04_download_cens.R")
interp.script <- file.path(code.dir, "05_interp.R")
download.other.script <- file.path(code.dir, "06_download_other.R")
assignTract.script <- file.path(code.dir, "07_ass_trac.R")
assignTractAKHI.script <- file.path(code.dir, "08_ass_trac_AKHI.R")
cens_agr.script <- file.path(code.dir, "09_aggregate.R")
paf.script <- file.path(code.dir, "10_paf.R")
read.total.burden.script <- file.path(code.dir, "11_read_tot.R")
read.nvs.findrepl.script <- file.path(code.dir, "12_nvss_findrepl.R")
read.total.burden.nvs.script <- file.path(code.dir, "13_read_tot_nvss.R")
pop.summary.script <- file.path(code.dir, "14_popsum.R")
pop.summary.educ.script <- file.path(code.dir, "15_popsum_educ.R")
add.rate.tot.burd <- file.path(code.dir, "16_add_rate_totburd.R")
calc.attr.burd.script <- file.path(code.dir, "17_calc_attr_burd.R")
summary.script <- file.path(code.dir, "19_summary.R")
plot.script <- file.path(code.dir, "20_plot.R")

#--------parameters of code-------------------
args <- paste(tmp.dir, exp.rr.dir)
# runscript(script=mrbrtRR.script, args = args)

years <- c(2000, 2010, 2001:2009, 2011:2016)
# years <- c(2000:2001)
# years <- c(2001)
for (agr_by in agr_bys) {
  for (source in sources) {
    for (year in years) {
      args <- paste(
        year, # 1
        data.dir, # 2
        tmp.dir, # 3
        exp.dir, # 4
        trac.dir, # 5
        exp.rr.dir, # 6
        trac.exp.dir, # 7
        dem.dir, # 8
        dem.agr.dir, # 9
        agr_by, # 10
        paf.dir, # 11
        total.burden.dir, # 12
        total.burden.parsed.dir,  #13
        source, # 14
        cdc.pop.dir, # 15
        pop.summary.dir,# 16
        total.burden.parsed2.dir, #17
        attr.burden.dir # 18
      )
      # runscript(script = download.meta.script, args = args)
       #runscript(script = meta.cross.script, args = args)
       if(year %in% c(2000, 2009:2016)){
      #   runscript(script = download.cens.script, args = args)
       } else{
       #  runscript(script = interp.script, args = args)
       }
       
      # runscript(script = download.other.script, args = args)
      # runscript(script=assignTract.script, args = args)
      # runscript(script = assignTractAKHI.script, args = args)
      #  runscript(script = cens_agr.script, args = args)
      #  runscript(script = paf.script, args = args)
      if (source == "wonder") {
      #  runscript(script = read.total.burden.script, args = args)
      } else if (source == "nvss") {
      #   runscript(script = read.nvs.findrepl.script, args = args)
        runscript(script = read.total.burden.nvs.script, args = args)
      }
    #  runscript(script=pop.summary.script, args = args)
    #  runscript(script=pop.summary.educ.script, args = args)
      #runscript(script = add.rate.tot.burd, args = args)
     # runscript(script = calc.attr.burd.script, args = args)
    }
  }
}

for (agr_by in agr_bys) { 
  args <- paste(
    tmp.dir, # 1
    agr_by, # 2
    dem.dir, # 3
    total.burden.parsed2.dir, # 4
    attr.burden.dir, # 5
    summary.dir, # 6
    plot.dir # 7
  )
  
 # runscript(script = summary.script, args = args)
#  runscript(script = plot.script, args = args)
  
}
