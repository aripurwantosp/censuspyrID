# Global variables

utils::globalVariables('censuspyrID_data_env')

assign('censuspyrID_data_env', new.env(), envir=parent.env(environment()))
       # envir = .GlobalEnv)

# Main ----

## Sidebar for main parameter ----

# # geographic (province) mode
# censuspyrID_data_env$reg_mode <-
#   structure(
#     c(1,2),
#     names = c("Harmonized","Non-harmonized")
#     )

# before-after expand option
censuspyrID_data_env$expand_phase <-
  structure(
    c(1,2),
    names = c("Before", "After")
  )

# smoothing option
censuspyrID_data_env$smoothing <-
  structure(
    c(1,2,3),
    names = c("None","Arriaga","Karup-King-Newton")
    )

# color
censuspyrID_data_env$col_name <- names(ggthemes::canva_palettes)
censuspyrID_data_env$col_name_opt <-
  lapply(names(ggthemes::canva_palettes),
         function(pal){
           cols <- ggthemes::canva_palettes[[pal]]
           swatches <- paste0(
             "<span style='display:inline-block;width:15px;height:15px;
             margin-right:2px;background-color:", cols, ";'></span>",
             collapse = ""
           )
           htmltools::HTML(paste0(swatches, pal))
         })

## Data ----
data('hpop5', envir = censuspyrID_data_env)
data('ypop5', envir = censuspyrID_data_env)
censuspyrID_data_env$ref_age_grp3 <- c("0-14","15-64","65+")

# Pyramids ----

# Trends ----
censuspyrID_data_env$trends_type <-
  structure(
    c(1,2),
    names = c("Pyramid","Area")
  )

censuspyrID_data_env$trends_pyr_mode <-
  structure(
    c(1,2),
    names = c("Grid","Overlay")
  )

censuspyrID_data_env$trends_area_sex <-
  structure(
    c(1,2,3,4),
    names = c("All","Male","Female","Male+Female")
  )

# Data ----

# Help ----
