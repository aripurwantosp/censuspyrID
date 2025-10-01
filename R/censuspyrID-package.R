#' censuspyrID: Explore and visualize Indonesian census pyramids
#'
#' @docType package
#' @keywords internal
#' @import ggplot2
#' @importFrom dplyr filter all_of pull mutate group_by ungroup summarise relocate case_when
#' @importFrom tidyr pivot_wider
#' @importFrom ggthemes scale_color_canva scale_fill_canva
#' @importFrom scales pretty_breaks trans_new log_trans
#' @importFrom DT dataTableOutput renderDT datatable formatRound
#' @importFrom networkD3 sankeyNetworkOutput renderSankeyNetwork sankeyNetwork
#' @importFrom shinyWidgets materialSwitch radioGroupButtons pickerInput sliderTextInput
#' @importFrom shinyjs disable enable useShinyjs
#' @importFrom shinythemes shinytheme
"_PACKAGE"
