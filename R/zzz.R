# Message when attached (library(censuspyrID))
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "Welcome to censuspyrID ", utils::packageVersion(pkgname), "!\n",
    "An R package for exploring harmonized and unharmonized\n",
    "population pyramids of Indonesia based on census data (1971-2020).\n",
    "Type citation(\"censuspyrID\") for citation info."
  ))
}

# suppress for notes when devtools::check()
utils::globalVariables(c(
  "Female", "Male", "age5", "age_grp3", "code", "label", "pop",
  "province", "province_id", "province_id_h", "province_id_y",
  "sex", "year"
))
