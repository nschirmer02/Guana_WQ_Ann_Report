library(here)
source(here('R', '04.2_agm-plot-fxn.R'))


# chlorophyll -------------------------------------------------------------

agm_plot(param = "CHLa_C", site = "Micklers", ylab = chla_y_title) + geom_hline(yintercept = 11, color = "black", linetype = "dashed")
agm_plot(param = "CHLa_C", site = "GL1", ylab = chla_y_title) + geom_hline(yintercept = 11, color = "black", linetype = "dashed")
agm_plot(param = "CHLa_C", site = "GL2", ylab = chla_y_title) + geom_hline(yintercept = 11, color = "black", linetype = "dashed")
agm_plot(param = "CHLa_C", site = "Lake Middle", ylab = chla_y_title) + geom_hline(yintercept = 11, color = "black", linetype = "dashed")
agm_plot(param = "CHLa_C", site = "GL4", ylab = chla_y_title) + geom_hline(yintercept = 11, color = "black", linetype = "dashed")
agm_plot(param = "CHLa_C", site = "Lake South", ylab = chla_y_title) + geom_hline(yintercept = 11, color = "black", linetype = "dashed")
agm_plot(param = "CHLa_C", site = "River North", ylab = chla_y_title) + geom_hline(yintercept = 6.6, color = "black", linetype = "dashed")
agm_plot(param = "CHLa_C", site = "GR1", ylab = chla_y_title) + geom_hline(yintercept = 6.6, color = "black", linetype = "dashed")
agm_plot(param = "CHLa_C", site = "Guana River", ylab = chla_y_title) + geom_hline(yintercept = 6.6, color = "black", linetype = "dashed")
agm_plot(param = "CHLa_C", site = "GR3", ylab = chla_y_title) + geom_hline(yintercept = 6.6, color = "black", linetype = "dashed")


# entero ------------------------------------------------------------------

agm_plot(param = "ENTERO", site = "Micklers", ylab = entero_y_title)
agm_plot(param = "ENTERO", site = "GL1", ylab = entero_y_title)
agm_plot(param = "ENTERO", site = "GL2", ylab = entero_y_title)
agm_plot(param = "ENTERO", site = "Lake Middle", ylab = entero_y_title)
agm_plot(param = "ENTERO", site = "GL4", ylab = entero_y_title)
agm_plot(param = "ENTERO", site = "Lake South", ylab = entero_y_title)
agm_plot(param = "ENTERO", site = "River North", ylab = entero_y_title)
agm_plot(param = "ENTERO", site = "GR1", ylab = entero_y_title)
agm_plot(param = "ENTERO", site = "Guana River", ylab = entero_y_title)
agm_plot(param = "ENTERO", site = "GR3", ylab = entero_y_title)


# TP ----------------------------------------------------------------------

agm_plot(param = "TP", site = "Micklers", ylab = phos_y_title)
agm_plot(param = "TP", site = "GL1", ylab = phos_y_title)
agm_plot(param = "TP", site = "GL2", ylab = phos_y_title)
agm_plot(param = "TP", site = "Lake Middle", ylab = phos_y_title)
agm_plot(param = "TP", site = "GL4", ylab = phos_y_title)
agm_plot(param = "TP", site = "Lake South", ylab = phos_y_title)
agm_plot(param = "TP", site = "River North", ylab = phos_y_title)
agm_plot(param = "TP", site = "GR1", ylab = phos_y_title)
agm_plot(param = "TP", site = "Guana River", ylab = phos_y_title)
agm_plot(param = "TP", site = "GR3", ylab = phos_y_title)


# TKN ---------------------------------------------------------------------

agm_plot(param = "TKN", site = "Micklers", ylab = "Total Kjeldahl Nitrogen (mg/L)")
agm_plot(param = "TKN", site = "GL1", ylab = "Total Kjeldahl Nitrogen (mg/L)")
agm_plot(param = "TKN", site = "GL2", ylab = "Total Kjeldahl Nitrogen (mg/L)")
agm_plot(param = "TKN", site = "Lake Middle", ylab = "Total Kjeldahl Nitrogen (mg/L)")
agm_plot(param = "TKN", site = "GL4", ylab = "Total Kjeldahl Nitrogen (mg/L)")
agm_plot(param = "TKN", site = "Lake South", ylab = "Total Kjeldahl Nitrogen (mg/L)")
agm_plot(param = "TKN", site = "River North", ylab = "Total Kjeldahl Nitrogen (mg/L)")
agm_plot(param = "TKN", site = "GR1", ylab = "Total Kjeldahl Nitrogen (mg/L)")
agm_plot(param = "TKN", site = "Guana River", ylab = "Total Kjeldahl Nitrogen (mg/L)")
agm_plot(param = "TKN", site = "GR3", ylab = "Total Kjeldahl Nitrogen (mg/L)")