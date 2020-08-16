## server.R ##
library(shiny)
library(ggplot2)
library(reshape2)
library(plotly)
library(bs4Dash)

summary = read.csv(file = "St_Lucia_2016_Election_Results.csv")

####################################################################################
# General Summary
####################################################################################
slpTotals = as.integer(summary$Total[which(summary$Electoral.Districts == "SLP Votes")])
uwpTotals = as.integer(summary$Total[which(summary$Electoral.Districts == "UWP Votes")])
lgTotals = as.integer(summary$Total[which(summary$Electoral.Districts == "LG Votes")])
lpmTotals = as.integer(summary$Total[which(summary$Electoral.Districts == "LPM")])
indTotals = as.integer(summary$Total[which(summary$Electoral.Districts == "Independents")])
cajTotals = as.integer(summary$Total[which(summary$Electoral.Districts == "Compton-Antoine J #")])

party = c("SLP", "UWP", "LG", "LPM", "IND", "CAJ") 
votes = c(slpTotals, uwpTotals, lgTotals, lpmTotals, indTotals, cajTotals) 
results = data.frame(party, votes)

color = c("yellow", "red", "darkblue", "green", "purple", "lightblue")

# Reorder the factor levels before sending to ggplot.
results$party = with(results, reorder(party, -votes))

# Bar graph showing overall elections results.
generalResults = ggplot(results, aes(x = party, y = votes, fill = party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = color) + 
  theme_minimal()

# Convert to plotly
generalResults = ggplotly(generalResults, tooltip = c("votes", "colour"))

####################################################################################
# Police Data
####################################################################################
police_data = summary[which(summary$Electoral.Districts == "Police"),]
# Remove unwanted columns
police_data = subset(police_data, select = -c(Electoral.Districts, Total))
districts = c("Gros Islet", "Babonneau", "Castries North", "Castries East", "Castries Central", "Castries South",
              "Anse La Raye - Canaries", "Soufriere", "Choiseul", "Laborie", "Vieux-Fort South", "Vieux-Fort North", 
              "Micoud South", "Micoud North", "Dennery South", "Dennery North", "Castries South East")
p_votes = as.numeric(police_data[1, ])
police_data = data.frame(districts, p_votes)

police = ggplot(police_data, aes(x = districts, y = p_votes, fill = "")) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Districts") + ylab("Voters") +
  coord_flip() +
  theme_bw()

# Convert to plotly
police = hide_legend(ggplotly(police, tooltip = c("p_votes")))

###############################################################################
# Rejected Votes Data
###############################################################################
r_votes_data = summary[which(summary$Electoral.Districts == "Rejected Votes"),]
# Remove unwanted columns
r_votes_data = subset(r_votes_data, select = -c(Electoral.Districts, Total))

r_votes = as.numeric(r_votes_data[1, ])
r_votes_data = data.frame(districts, r_votes)

r_votes_graph = ggplot(r_votes_data, aes(x = districts, y = r_votes, fill = districts)) + 
  geom_segment(aes(xend = districts, yend = 0)) +
  geom_point(size = 4, color = 'orange', show.legend = FALSE) +
  xlab("Districts") + ylab("Rejected Votes") +
  coord_flip() +
  theme_light()

# Convert to plotly
r_votes_graph = hide_legend(ggplotly(r_votes_graph, tooltip = c("r_votes")))

###############################################################################
# Graph for Anse La Raye Canaries
###############################################################################
alrc_summary = read.csv(file = "Anse_La_Raye_Canaries/Anse_La_Raye_Canaries.csv")

alrc_party = c("SLP", "UWP") 
alrc_votes = c(alrc_summary$SLP, alrc_summary$UWP) 
alrc_results = data.frame(alrc_party, alrc_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
alrc_results$party = with(alrc_results, reorder(alrc_party, -alrc_votes))

# Bar graph showing election results in Anse La Raye / Canaries District.
alrc_graph = ggplot(alrc_results, aes(x = alrc_party, y = alrc_votes, fill = alrc_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

alrc_graph = ggplotly(alrc_graph, tooltip = c("alrc_votes"))

###############################################################################
# Graph to show results by polling station in Anse La Raye Canaries District
###############################################################################
ps_alrps = read.csv(file = "Anse_La_Raye_Canaries/ANSE_LA_RAYE_PRIMARY_SCHOOL.csv")
ps_cps = read.csv(file = "Anse_La_Raye_Canaries/CANARIES_PRIMARY_SCHOOL.csv")
ps_cfav = read.csv(file = "Anse_La_Raye_Canaries/CHRISTIAN_FAITH_ASSEMBLY_VANARD.csv")
ps_mps = read.csv(file = "Anse_La_Raye_Canaries/MILLET_PRIMARY_SCHOOL.csv")
ps_rcs = read.csv(file = "Anse_La_Raye_Canaries/ROSEAU_COMBINED_SCHOOL.csv")

ps_alrps_uwp_votes = ps_alrps$UWP[which(ps_alrps$Section == "Sub")]
ps_cps_uwp_votes = ps_cps$UWP[which(ps_cps$Section == "Sub")]
ps_cfav_uwp_votes = ps_cfav$UWP[which(ps_cfav$Section == "Sub")]
ps_mps_uwp_votes = ps_mps$UWP[which(ps_mps$Section == "Sub")]
ps_rcs_uwp_votes = ps_rcs$UWP[which(ps_rcs$Section == "Sub")]

ps_alrps_slp_votes = ps_alrps$SLP[which(ps_alrps$Section == "Sub")]
ps_cps_slp_votes = ps_cps$SLP[which(ps_cps$Section == "Sub")]
ps_cfav_slp_votes = ps_cfav$SLP[which(ps_cfav$Section == "Sub")]
ps_mps_slp_votes = ps_mps$SLP[which(ps_mps$Section == "Sub")]
ps_rcs_slp_votes = ps_rcs$SLP[which(ps_rcs$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c(
    "ANSE LA RAYE PRIMARY SCHOOL", 
    "CANARIES PRIMARY SCHOOL", 
    "CHRISTIAN FAITH ASSEMBLY VANARD", 
    "MILLET PRIMARY SCHOOL", 
    "ROSEAU COMBINED SCHOOL"), 
  y = c(
    ps_alrps_uwp_votes, 
    ps_cps_uwp_votes, 
    ps_cfav_uwp_votes, 
    ps_mps_uwp_votes, 
    ps_rcs_uwp_votes), 
  marker = list(color = "yellow")
)
trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c(
    "ANSE LA RAYE PRIMARY SCHOOL", 
    "CANARIES PRIMARY SCHOOL", 
    "CHRISTIAN FAITH ASSEMBLY VANARD", 
    "MILLET PRIMARY SCHOOL", 
    "ROSEAU COMBINED SCHOOL"), 
  y = c(
    ps_alrps_slp_votes, 
    ps_cps_slp_votes, 
    ps_cfav_slp_votes, 
    ps_mps_slp_votes, 
    ps_rcs_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_alrc_graph = plot_ly()
ps_alrc_graph = add_trace(ps_alrc_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_alrc_graph = add_trace(ps_alrc_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_alrc_graph = layout(ps_alrc_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)

###############################################################################
# Graph for Babonneau
###############################################################################
b_summary = read.csv(file = "Babonneau/Babonneau.csv")

b_party = c("SLP", "UWP") 
b_votes = c(b_summary$SLP, b_summary$UWP) 
b_results = data.frame(b_party, b_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
b_results$party = with(b_results, reorder(b_party, -b_votes))

# Bar graph showing election results in Babonneau District.
b_graph = ggplot(b_results, aes(x = b_party, y = b_votes, fill = b_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

b_graph = ggplotly(b_graph, tooltip = c("b_votes"))

###############################################################################
# Graph to show results by polling station in Babonneau District
###############################################################################
ps_bmpc = read.csv(file = "Babonneau/BABONNEAU_MULTI_PURPOSE_CENTRE.csv")
ps_bps = read.csv(file = "Babonneau/BABONNEAU_PRIMARY_SCHOOL.csv")
ps_bss = read.csv(file = "Babonneau/BABONNEAU_SECONDARY_SCHOOL.csv")
ps_balcs = read.csv(file = "Babonneau/BALATA_COMBINED_SCHOOL.csv")
ps_bogcs = read.csv(file = "Babonneau/BOGUIS_COMBINED_SCHOOL.csv")
ps_dbcs = read.csv(file = "Babonneau/DES_BARRA_COMBINED_SCHOOL.csv")
ps_facs = read.csv(file = "Babonneau/FOND_ASSAU_COMBINED_SCHOOL.csv")
ps_gmfh = read.csv(file = "Babonneau/GARRAND_MOTHERS_FATHERS_HALL.csv")
ps_lgcs = read.csv(file = "Babonneau/LA_GUERRE_COMBINED_SCHOOL.csv")
ps_lgcc = read.csv(file = "Babonneau/LA_GUERRE_COMMUNITY_CENTRE.csv")

ps_bmpc_uwp_votes = ps_bmpc$UWP[which(ps_bmpc$Section == "Sub")]
ps_bps_uwp_votes = ps_bps$UWP[which(ps_bps$Section == "Sub")]
ps_bss_uwp_votes = ps_bss$UWP[which(ps_bss$Section == "Sub")]
ps_balcs_uwp_votes = ps_balcs$UWP[which(ps_balcs$Section == "Sub")]
ps_bogcs_uwp_votes = ps_bogcs$UWP[which(ps_bogcs$Section == "Sub")]
ps_dbcs_uwp_votes = ps_dbcs$UWP[which(ps_dbcs$Section == "A-Z")]
ps_facs_uwp_votes = ps_facs$UWP[which(ps_facs$Section == "Sub")]
ps_gmfh_uwp_votes = ps_gmfh$UWP[which(ps_gmfh$Section == "Sub")]
ps_lgcs_uwp_votes = ps_lgcs$UWP[which(ps_lgcs$Section == "Sub")]
ps_lgcc_uwp_votes = ps_lgcc$UWP[which(ps_lgcc$Section == "Sub")]

ps_bmpc_slp_votes = ps_bmpc$SLP[which(ps_bmpc$Section == "Sub")]
ps_bps_slp_votes = ps_bps$SLP[which(ps_bps$Section == "Sub")]
ps_bss_slp_votes = ps_bss$SLP[which(ps_bss$Section == "Sub")]
ps_balcs_slp_votes = ps_balcs$SLP[which(ps_balcs$Section == "Sub")]
ps_bogcs_slp_votes = ps_bogcs$SLP[which(ps_bogcs$Section == "Sub")]
ps_dbcs_slp_votes = ps_dbcs$SLP[which(ps_dbcs$Section == "A-Z")]
ps_facs_slp_votes = ps_facs$SLP[which(ps_facs$Section == "Sub")]
ps_gmfh_slp_votes = ps_gmfh$SLP[which(ps_gmfh$Section == "Sub")]
ps_lgcs_slp_votes = ps_lgcs$SLP[which(ps_lgcs$Section == "Sub")]
ps_lgcc_slp_votes = ps_lgcc$SLP[which(ps_lgcc$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("BABONNEAU MULTI PURPOSE CENTRE", 
        "BABONNEAU PRIMARY SCHOOL", 
        "BABONNEAU SECONDARY SCHOOL", 
        "BALATA COMBINED SCHOOL", 
        "BOGUIS COMBINED SCHOOL",
        "DES BARRA COMBINED SCHOOL",
        "FOND ASSAU COMBINED SCHOOL",
        "GARRAND MOTHERS FATHERS HALL",
        "LA GUERRE COMBINED SCHOOL",
        "LA GUERRE COMMUNITY CENTRE"), 
  y = c(ps_bmpc_uwp_votes, 
        ps_bps_uwp_votes, 
        ps_bss_uwp_votes, 
        ps_balcs_uwp_votes, 
        ps_bogcs_uwp_votes,
        ps_dbcs_uwp_votes,
        ps_facs_uwp_votes,
        ps_gmfh_uwp_votes,
        ps_lgcs_uwp_votes,
        ps_lgcc_uwp_votes), 
  marker = list(color = "yellow")
)
trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("BABONNEAU MULTI PURPOSE CENTRE", 
        "BABONNEAU PRIMARY SCHOOL", 
        "BABONNEAU SECONDARY SCHOOL", 
        "BALATA COMBINED SCHOOL", 
        "BOGUIS COMBINED SCHOOL",
        "DES BARRA COMBINED SCHOOL",
        "FOND ASSAU COMBINED SCHOOL",
        "GARRAND MOTHERS FATHERS HALL",
        "LA GUERRE COMBINED SCHOOL",
        "LA GUERRE COMMUNITY CENTRE"), 
  y = c(ps_bmpc_slp_votes, 
        ps_bps_slp_votes, 
        ps_bss_slp_votes, 
        ps_balcs_slp_votes, 
        ps_bogcs_slp_votes,
        ps_dbcs_slp_votes,
        ps_facs_slp_votes,
        ps_gmfh_slp_votes,
        ps_lgcs_slp_votes,
        ps_lgcc_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_b_graph = plot_ly()
ps_b_graph = add_trace(ps_b_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_b_graph = add_trace(ps_b_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_b_graph = layout(ps_b_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


###############################################################################
# Graph for Castries Central
###############################################################################
cc_summary = read.csv(file = "Castries_Central/Castries_Central.csv")

cc_party = c("INDEPENDENT", "SLP", "UWP") 
cc_votes = c(cc_summary$INDEP, cc_summary$SLP, cc_summary$UWP) 
cc_results = data.frame(cc_party, cc_votes)

color = c("blue", "red", "yellow")

# Reorder the factor levels before sending to ggplot.
cc_results$party = with(cc_results, reorder(cc_party, -cc_votes))

# Bar graph showing election results in Central Castries District.
cc_graph = ggplot(cc_results, aes(x = cc_party, y = cc_votes, fill = cc_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

cc_graph = ggplotly(cc_graph, tooltip = c("cc_votes"))

###############################################################################
# Graph to show results by polling station in Castries Central District
###############################################################################
ps_ais = read.csv(file = "Castries_Central/ANGLICAN_INFANT_SCHOOL.csv")
ps_amis = read.csv(file = "Castries_Central/AVE_MARIA_INFANT_SCHOOL.csv")
ps_amps = read.csv(file = "Castries_Central/AVE_MARIA_PRIMARY_SCHOOL.csv")
ps_crms = read.csv(file = "Castries_Central/CARMEN_RENE_MEMORIAL_SCHOOL.csv")
ps_mph = read.csv(file = "Castries_Central/METHODIST_PARISH_HALL.csv")
ps_rcbis = read.csv(file = "Castries_Central/R_C_BOYS_INFANT_SCHOOL.csv")
ps_rcbps = read.csv(file = "Castries_Central/R_C_BOYS_PRIMARY_SCHOOL.csv")


ps_ais_uwp_votes = ps_ais$UWP[which(ps_ais$Section == "Sub")]
ps_amis_uwp_votes = ps_amis$UWP[which(ps_amis$Section == "Sub")]
ps_amps_uwp_votes = ps_amps$UWP[which(ps_amps$Section == "Sub")]
ps_crms_uwp_votes = ps_crms$UWP[which(ps_crms$Section == "Sub")]
ps_mph_uwp_votes = ps_mph$UWP[which(ps_mph$Section == "Sub")]
ps_rcbis_uwp_votes = ps_rcbis$UWP[which(ps_rcbis$Section == "Sub")]
ps_rcbps_uwp_votes = ps_rcbps$UWP[which(ps_rcbps$Section == "Sub")]


ps_ais_slp_votes = ps_ais$SLP[which(ps_ais$Section == "Sub")]
ps_amis_slp_votes = ps_amis$SLP[which(ps_amis$Section == "Sub")]
ps_amps_slp_votes = ps_amps$SLP[which(ps_amps$Section == "Sub")]
ps_crms_slp_votes = ps_crms$SLP[which(ps_crms$Section == "Sub")]
ps_mph_slp_votes = ps_mph$SLP[which(ps_mph$Section == "Sub")]
ps_rcbis_slp_votes = ps_rcbis$SLP[which(ps_rcbis$Section == "Sub")]
ps_rcbps_slp_votes = ps_rcbps$SLP[which(ps_rcbps$Section == "Sub")]

ps_ais_indep_votes = ps_ais$INDEP[which(ps_ais$Section == "Sub")]
ps_amis_indep_votes = ps_amis$INDEP[which(ps_amis$Section == "Sub")]
ps_amps_indep_votes = ps_amps$INDEP[which(ps_amps$Section == "Sub")]
ps_crms_indep_votes = ps_crms$INDEP[which(ps_crms$Section == "Sub")]
ps_mph_indep_votes = ps_mph$INDEP[which(ps_mph$Section == "Sub")]
ps_rcbis_indep_votes = ps_rcbis$INDEP[which(ps_rcbis$Section == "Sub")]
ps_rcbps_indep_votes = ps_rcbps$INDEP[which(ps_rcbps$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("ANGLICAN INFANT SCHOOL", 
        "AVE MARIA INFANT SCHOOL", 
        "AVE MARIA PRIMARY SCHOOL", 
        "CARMEN RENE MEMORIAL SCHOOL", 
        "METHODIST PARISH HALL",
        "RC BOYS INFANT SCHOOL",
        "RC BOYS PRIMARY SCHOOL"), 
  y = c(ps_ais_uwp_votes, 
        ps_amis_uwp_votes, 
        ps_amps_uwp_votes, 
        ps_crms_uwp_votes, 
        ps_mph_uwp_votes,
        ps_rcbis_uwp_votes,
        ps_rcbps_uwp_votes), 
  marker = list(color = "yellow")
)
trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("ANGLICAN INFANT SCHOOL", 
        "AVE MARIA INFANT SCHOOL", 
        "AVE MARIA PRIMARY SCHOOL", 
        "CARMEN RENE MEMORIAL SCHOOL", 
        "METHODIST PARISH HALL",
        "RC BOYS INFANT SCHOOL",
        "RC BOYS PRIMARY SCHOOL"), 
  y = c(ps_ais_slp_votes, 
        ps_amis_slp_votes, 
        ps_amps_slp_votes, 
        ps_crms_slp_votes, 
        ps_mph_slp_votes,
        ps_rcbis_slp_votes,
        ps_rcbps_slp_votes), 
  marker = list(color = "red")
)
trace3 = list(
  uid = "IND", 
  name = "INDEPENDENT", 
  type = "bar", 
  x = c("ANGLICAN INFANT SCHOOL", 
        "AVE MARIA INFANT SCHOOL", 
        "AVE MARIA PRIMARY SCHOOL", 
        "CARMEN RENE MEMORIAL SCHOOL", 
        "METHODIST PARISH HALL",
        "RC BOYS INFANT SCHOOL",
        "RC BOYS PRIMARY SCHOOL"), 
  y = c(ps_ais_indep_votes, 
        ps_amis_indep_votes, 
        ps_amps_indep_votes, 
        ps_crms_indep_votes, 
        ps_mph_indep_votes,
        ps_rcbis_indep_votes,
        ps_rcbps_indep_votes), 
  marker = list(color = "blue")
)

data = list(trace1, trace2, trace3)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_cc_graph = plot_ly()
ps_cc_graph = add_trace(ps_cc_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_cc_graph = add_trace(ps_cc_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_cc_graph = add_trace(ps_cc_graph, uid=trace3$uid, name=trace3$name, type=trace3$type, x=trace3$x, y=trace3$y, marker=trace3$marker)
ps_cc_graph = layout(ps_cc_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


###############################################################################
# Graph for Castries East
###############################################################################
ce_summary = read.csv(file = "Castries_East/Castries_East.csv")

ce_party = c("SLP", "UWP") 
ce_votes = c(ce_summary$SLP, ce_summary$UWP) 
ce_results = data.frame(ce_party, ce_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
ce_results$party = with(ce_results, reorder(ce_party, -ce_votes))

# Bar graph showing election results in Castries East District.
ce_graph = ggplot(ce_results, aes(x = ce_party, y = ce_votes, fill = ce_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

ce_graph = ggplotly(ce_graph, tooltip = c("ce_votes"))

###############################################################################
# Graph to show results by polling station in Castries East District
###############################################################################
ps_bgs = read.csv(file = "Castries_East/BOCAGE_GOVERNMENT_SCHOOL.csv")
ps_cp = read.csv(file = "Castries_East/CADET_PAVILLION.csv")
ps_ess = read.csv(file = "Castries_East/ENTREPOT_SECONDARY_SCHOOL.csv")
ps_mcs = read.csv(file = "Castries_East/MARCHAND_COMBINED_SCHOOL.csv")
ps_mpc = read.csv(file = "Castries_East/MARCHAND_PARISH_CENTRE.csv")
ps_mpp = read.csv(file = "Castries_East/MINDOO_PHILLIP_PARK.csv")
ps_pa = read.csv(file = "Castries_East/POLICE_AUDITORIUM.csv")


ps_bgs_uwp_votes = ps_bgs$UWP[which(ps_bgs$Section == "Sub")]
ps_cp_uwp_votes = ps_cp$UWP[which(ps_cp$Section == "Sub")]
ps_ess_uwp_votes = ps_ess$UWP[which(ps_ess$Section == "Sub")]
ps_mcs_uwp_votes = ps_mcs$UWP[which(ps_mcs$Section == "Sub")]
ps_mpc_uwp_votes = ps_mpc$UWP[which(ps_mpc$Section == "Sub")]
ps_mpp_uwp_votes = ps_mpp$UWP[which(ps_mpp$Section == "Sub")]
ps_pa_uwp_votes = ps_pa$UWP[which(ps_pa$Section == "Sub")]


ps_bgs_slp_votes = ps_bgs$SLP[which(ps_bgs$Section == "Sub")]
ps_cp_slp_votes = ps_cp$SLP[which(ps_cp$Section == "Sub")]
ps_ess_slp_votes = ps_ess$SLP[which(ps_ess$Section == "Sub")]
ps_mcs_slp_votes = ps_mcs$SLP[which(ps_mcs$Section == "Sub")]
ps_mpc_slp_votes = ps_mpc$SLP[which(ps_mpc$Section == "Sub")]
ps_mpp_slp_votes = ps_mpp$SLP[which(ps_mpp$Section == "Sub")]
ps_pa_slp_votes = ps_pa$SLP[which(ps_pa$Section == "Sub")]


trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("BOCAGE GOVERNMENT SCHOOL", 
        "CADET PAVILLION", 
        "ENTREPOT SECONDARY SCHOOL", 
        "MARCHAND COMBINED SCHOOL", 
        "MARCHAND PARISH CENTRE",
        "MINDOO PHILLIP PARK",
        "POLICE AUDITORIUM"), 
  y = c(ps_bgs_uwp_votes, 
        ps_cp_uwp_votes, 
        ps_ess_uwp_votes, 
        ps_mcs_uwp_votes, 
        ps_mpc_uwp_votes,
        ps_mpp_uwp_votes,
        ps_pa_uwp_votes), 
  marker = list(color = "yellow")
)

trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("BOCAGE GOVERNMENT SCHOOL", 
        "CADET PAVILLION", 
        "ENTREPOT SECONDARY SCHOOL", 
        "MARCHAND COMBINED SCHOOL", 
        "MARCHAND PARISH CENTRE",
        "MINDOO PHILLIP PARK",
        "POLICE AUDITORIUM"), 
  y = c(ps_bgs_slp_votes, 
        ps_cp_slp_votes, 
        ps_ess_slp_votes, 
        ps_mcs_slp_votes, 
        ps_mpc_slp_votes,
        ps_mpp_slp_votes,
        ps_pa_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_ce_graph = plot_ly()
ps_ce_graph = add_trace(ps_ce_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_ce_graph = add_trace(ps_ce_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_ce_graph = layout(ps_ce_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


###############################################################################
# Graph for Castries North
###############################################################################
cn_summary = read.csv(file = "Castries_North/Castries_North.csv")

cn_party = c("SLP", "UWP") 
cn_votes = c(cn_summary$SLP, cn_summary$UWP) 
cn_results = data.frame(cn_party, cn_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
cn_results$party = with(cn_results, reorder(cn_party, -cn_votes))

# Bar graph showing election results in Castries North District.
cn_graph = ggplot(cn_results, aes(x = cn_party, y = cn_votes, fill = cn_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

cn_graph = ggplotly(cn_graph, tooltip = c("cn_votes"))

###############################################################################
# Graph to show results by polling station in Castries North District
###############################################################################
ps_aps = read.csv(file = "Castries_North/ANGLICAN_PRIMARY_SCHOOL.csv")
ps_ccs = read.csv(file = "Castries_North/CASTRIES_COMPREHENSIVE_SCHOOL.csv")
ps_mddcs = read.csv(file = "Castries_North/MORNE_DU_DON_COMBINED_SCHOOL.csv")
ps_sjcps = read.csv(file = "Castries_North/ST_JOSEPHS_CONVENT_PRE_SCHOOL.csv")
ps_vbss = read.csv(file = "Castries_North/VIDE_BOUTEILLE_SECONDARY_SCHOOL.csv")


ps_aps_uwp_votes = ps_aps$UWP[which(ps_aps$Section == "Sub")]
ps_ccs_uwp_votes = ps_ccs$UWP[which(ps_ccs$Section == "Sub")]
ps_mddcs_uwp_votes = ps_mddcs$UWP[which(ps_mddcs$Section == "Sub")]
ps_sjcps_uwp_votes = ps_sjcps$UWP[which(ps_sjcps$Section == "Sub")]
ps_vbss_uwp_votes = ps_vbss$UWP[which(ps_vbss$Section == "Sub")]


ps_aps_slp_votes = ps_aps$SLP[which(ps_aps$Section == "Sub")]
ps_ccs_slp_votes = ps_ccs$SLP[which(ps_ccs$Section == "Sub")]
ps_mddcs_slp_votes = ps_mddcs$SLP[which(ps_mddcs$Section == "Sub")]
ps_sjcps_slp_votes = ps_sjcps$SLP[which(ps_sjcps$Section == "Sub")]
ps_vbss_slp_votes = ps_vbss$SLP[which(ps_vbss$Section == "Sub")]


trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("ANGLICAN PRIMARY SCHOOL", 
        "CASTRIES COMPREHENSIVE SCHOOL", 
        "MORNE DU DON COMBINED SCHOOL",
        "ST JOSEPHS CONVENT PRE SCHOOL",
        "VIDE BOUTEILLE SECONDARY SCHOOL"), 
  y = c(ps_aps_uwp_votes, 
        ps_ccs_uwp_votes, 
        ps_mddcs_uwp_votes, 
        ps_sjcps_uwp_votes,
        ps_vbss_uwp_votes), 
  marker = list(color = "yellow")
)

trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("ANGLICAN PRIMARY SCHOOL", 
        "CASTRIES COMPREHENSIVE SCHOOL", 
        "MORNE DU DON COMBINED SCHOOL",
        "ST JOSEPHS CONVENT PRE SCHOOL",
        "VIDE BOUTEILLE SECONDARY SCHOOL"), 
  y = c(ps_aps_slp_votes, 
        ps_ccs_slp_votes, 
        ps_mddcs_slp_votes,
        ps_sjcps_slp_votes,
        ps_vbss_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_cn_graph = plot_ly()
ps_cn_graph = add_trace(ps_cn_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_cn_graph = add_trace(ps_cn_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_cn_graph = layout(ps_cn_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


###############################################################################
# Graph for Castries South
###############################################################################
cs_summary = read.csv(file = "Castries_South/Castries_South.csv")

cs_party = c("SLP", "UWP") 
cs_votes = c(cs_summary$SLP, cs_summary$UWP) 
cs_results = data.frame(cs_party, cs_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
cs_results$party = with(cs_results, reorder(cs_party, -cs_votes))

# Bar graph showing election results in Castries South District.
cs_graph = ggplot(cs_results, aes(x = cs_party, y = cs_votes, fill = cs_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

cs_graph = ggplotly(cs_graph, tooltip = c("cs_votes"))

###############################################################################
# Graph to show results by polling station in Castries South District
###############################################################################
ps_ccs = read.csv(file = "Castries_South/CICERON_COMBINED_SCHOOL.csv")
ps_lcmhs = read.csv(file = "Castries_South/LA_CROIX_MAINGOT_HESS_SCHOOL.csv")
ps_sft = read.csv(file = "Castries_South/SLASPA_FERRY_TERMINAL.csv")


ps_ccs_uwp_votes = ps_ccs$UWP[which(ps_ccs$Section == "Sub")]
ps_lcmhs_uwp_votes = ps_lcmhs$UWP[which(ps_lcmhs$Section == "Sub")]
ps_sft_uwp_votes = ps_sft$UWP[which(ps_sft$Section == "Sub")]


ps_ccs_slp_votes = ps_ccs$SLP[which(ps_ccs$Section == "Sub")]
ps_lcmhs_slp_votes = ps_lcmhs$SLP[which(ps_lcmhs$Section == "Sub")]
ps_sft_slp_votes = ps_sft$SLP[which(ps_sft$Section == "Sub")]


trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("CICERON COMBINED SCHOOL", 
        "LA CROIX MAINGOT HESS SCHOOL", 
        "SLASPA FERRY TERMINAL"), 
  y = c(ps_ccs_uwp_votes, 
        ps_lcmhs_uwp_votes, 
        ps_sft_uwp_votes), 
  marker = list(color = "yellow")
)

trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("CICERON COMBINED SCHOOL", 
        "LA CROIX MAINGOT HESS SCHOOL", 
        "SLASPA FERRY TERMINAL"), 
  y = c(ps_ccs_slp_votes, 
        ps_lcmhs_slp_votes, 
        ps_sft_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_cs_graph = plot_ly()
ps_cs_graph = add_trace(ps_cs_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_cs_graph = add_trace(ps_cs_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_cs_graph = layout(ps_cs_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


###############################################################################
# Graph for Castries South East
###############################################################################
cse_summary = read.csv(file = "Castries_South_East/Castries_South_East.csv")

cse_party = c("SLP", "UWP") 
cse_votes = c(cse_summary$SLP, cse_summary$UWP) 
cse_results = data.frame(cse_party, cse_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
cse_results$party = with(cse_results, reorder(cse_party, -cse_votes))

# Bar graph showing election results in Castries South East District.
cse_graph = ggplot(cse_results, aes(x = cse_party, y = cse_votes, fill = cse_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

cse_graph = ggplotly(cse_graph, tooltip = c("cse_votes"))

###############################################################################
# Graph to show results by polling station in Castries South East District
###############################################################################
ps_bcc = read.csv(file = "Castries_South_East/BELAIR_COMMUNITY_CENTRE.csv")
ps_bis = read.csv(file = "Castries_South_East/BEXON_INFANT_SCHOOL.csv")
ps_brs = read.csv(file = "Castries_South_East/BEXON_PRIMARY_SCHOOL.csv")
ps_lcmhs = read.csv(file = "Castries_South_East/LA_CROIX_MAINGOT_HESS_SCHOOL.csv")
ps_ocs = read.csv(file = "Castries_South_East/ODSAN_COMBINED_SCHOOL.csv")
ps_rpss = read.csv(file = "Castries_South_East/RAVINE_POISSON_SDA_SCHOOL.csv")
ps_salcc = read.csv(file = "Castries_South_East/SIR_ARTHUR_LEWIS_COMMUNITY_COLLEGE.csv")
ps_trcs = read.csv(file = "Castries_South_East/TI_ROCHER_COMBINED_SCHOOL.csv")

ps_bcc_uwp_votes = ps_bcc$UWP[which(ps_bcc$Section == "A-Z")]
ps_bis_uwp_votes = ps_bis$UWP[which(ps_bis$Section == "Sub")]
ps_brs_uwp_votes = ps_brs$UWP[which(ps_brs$Section == "Sub")]
ps_lcmhs_uwp_votes = ps_lcmhs$UWP[which(ps_lcmhs$Section == "Sub")]
ps_ocs_uwp_votes = ps_ocs$UWP[which(ps_ocs$Section == "Sub")]
ps_rpss_uwp_votes = ps_rpss$UWP[which(ps_rpss$Section == "Sub")]
ps_salcc_uwp_votes = ps_salcc$UWP[which(ps_salcc$Section == "Sub")]
ps_trcs_uwp_votes = ps_trcs$UWP[which(ps_trcs$Section == "Sub")]

ps_bcc_slp_votes = ps_bcc$SLP[which(ps_bcc$Section == "A-Z")]
ps_bis_slp_votes = ps_bis$SLP[which(ps_bis$Section == "Sub")]
ps_brs_slp_votes = ps_brs$SLP[which(ps_brs$Section == "Sub")]
ps_lcmhs_slp_votes = ps_lcmhs$SLP[which(ps_lcmhs$Section == "Sub")]
ps_ocs_slp_votes = ps_ocs$SLP[which(ps_ocs$Section == "Sub")]
ps_rpss_slp_votes = ps_rpss$SLP[which(ps_rpss$Section == "Sub")]
ps_salcc_slp_votes = ps_salcc$SLP[which(ps_salcc$Section == "Sub")]
ps_trcs_slp_votes = ps_trcs$SLP[which(ps_trcs$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("BELAIR COMMUNITY CENTRE", 
        "BEXON INFANT SCHOOL", 
        "BEXON PRIMARY SCHOOL",
        "LA CROIX MAINGOT HESS SCHOOL",
        "ODSAN COMBINED SCHOOL",
        "RAVINE POISSON SDA SCHOOL",
        "SIR ARTHUR LEWIS COMMUNITY COLLEGE",
        "TI ROCHER COMBINED SCHOOL"), 
  y = c(ps_bcc_uwp_votes, 
        ps_bis_uwp_votes, 
        ps_brs_uwp_votes,
        ps_lcmhs_uwp_votes,
        ps_ocs_uwp_votes,
        ps_rpss_uwp_votes,
        ps_salcc_uwp_votes,
        ps_trcs_uwp_votes), 
  marker = list(color = "yellow")
)

trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("BELAIR COMMUNITY CENTRE", 
        "BEXON INFANT SCHOOL", 
        "BEXON PRIMARY SCHOOL",
        "LA CROIX MAINGOT HESS SCHOOL",
        "ODSAN COMBINED SCHOOL",
        "RAVINE POISSON SDA SCHOOL",
        "SIR ARTHUR LEWIS COMMUNITY COLLEGE",
        "TI ROCHER COMBINED SCHOOL"), 
  y = c(ps_bcc_slp_votes, 
        ps_bis_slp_votes, 
        ps_brs_slp_votes,
        ps_lcmhs_slp_votes,
        ps_ocs_slp_votes,
        ps_rpss_slp_votes,
        ps_salcc_slp_votes,
        ps_trcs_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_cse_graph = plot_ly()
ps_cse_graph = add_trace(ps_cse_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_cse_graph = add_trace(ps_cse_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_cse_graph = layout(ps_cse_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


###############################################################################
# Graph for Choiseul
###############################################################################
c_summary = read.csv(file = "Choiseul/Choiseul.csv")

c_party = c("SLP", "UWP") 
c_votes = c(c_summary$SLP, c_summary$UWP) 
c_results = data.frame(c_party, c_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
c_results$party = with(c_results, reorder(c_party, -c_votes))

# Bar graph showing election results in Choiseul District.
c_graph = ggplot(c_results, aes(x = c_party, y = c_votes, fill = c_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

c_graph = ggplotly(c_graph, tooltip = c("c_votes"))

###############################################################################
# Graph to show results by polling station in Choiseul District
###############################################################################
ps_css = read.csv(file = "Choiseul/CHOISEUL_SECONDARY_SCHOOL.csv")
ps_dcc = read.csv(file = "Choiseul/DACRETIN_COMMUNITY_CENTRE.csv")
ps_dcs = read.csv(file = "Choiseul/DELCER_COMBINED_SCHOOL.csv")
ps_ducs = read.csv(file = "Choiseul/DUGARD_COMBINED_SCHOOL.csv")
ps_hobj = read.csv(file = "Choiseul/HANG_OUT_BAR_JETRINE.csv")
ps_mis = read.csv(file = "Choiseul/MONGOUGE_INFANT_SCHOOL.csv")
ps_pcs = read.csv(file = "Choiseul/PIAYE_COMBINED_SCHOOL.csv")
ps_rcs = read.csv(file = "Choiseul/ROBLOT_COMBINED_SCHOOL.csv")
ps_scs = read.csv(file = "Choiseul/SALTIBUS_COMBINED_SCHOOL.csv")

ps_css_uwp_votes = ps_css$UWP[which(ps_css$Section == "Sub")]
ps_dcc_uwp_votes = ps_dcc$UWP[which(ps_dcc$Section == "Sub")]
ps_dcs_uwp_votes = ps_dcs$UWP[which(ps_dcs$Section == "Sub")]
ps_ducs_uwp_votes = ps_ducs$UWP[which(ps_ducs$Section == "A-Z")]
ps_hobj_uwp_votes = ps_hobj$UWP[which(ps_hobj$Section == "Sub")]
ps_mis_uwp_votes = ps_mis$UWP[which(ps_mis$Section == "Sub")]
ps_pcs_uwp_votes = ps_pcs$UWP[which(ps_pcs$Section == "Sub")]
ps_rcs_uwp_votes = ps_rcs$UWP[which(ps_rcs$Section == "Sub")]
ps_scs_uwp_votes = ps_scs$UWP[which(ps_scs$Section == "Sub")]

ps_css_slp_votes = ps_css$SLP[which(ps_css$Section == "Sub")]
ps_dcc_slp_votes = ps_dcc$SLP[which(ps_dcc$Section == "Sub")]
ps_dcs_slp_votes = ps_dcs$SLP[which(ps_dcs$Section == "Sub")]
ps_ducs_slp_votes = ps_ducs$SLP[which(ps_ducs$Section == "A-Z")]
ps_hobj_slp_votes = ps_hobj$SLP[which(ps_hobj$Section == "Sub")]
ps_mis_slp_votes = ps_mis$SLP[which(ps_mis$Section == "Sub")]
ps_pcs_slp_votes = ps_pcs$SLP[which(ps_pcs$Section == "Sub")]
ps_rcs_slp_votes = ps_rcs$SLP[which(ps_rcs$Section == "Sub")]
ps_scs_slp_votes = ps_scs$SLP[which(ps_scs$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("CHOISEUL SECONDARY SCHOOL", 
        "DACRETIN COMMUNITY CENTRE", 
        "DELCER COMBINED SCHOOL",
        "DUGARD COMBINED SCHOOL",
        "HANG OUT BAR JETRINE",
        "MONGOUGE INFANT SCHOOL",
        "PIAYE COMBINED SCHOOL",
        "ROBLOT COMBINED SCHOOL",
        "SALTIBUS COMBINED SCHOOL"), 
  y = c(ps_css_uwp_votes, 
        ps_dcc_uwp_votes, 
        ps_dcs_uwp_votes,
        ps_ducs_uwp_votes,
        ps_hobj_uwp_votes,
        ps_mis_uwp_votes,
        ps_pcs_uwp_votes,
        ps_rcs_uwp_votes,
        ps_scs_uwp_votes), 
  marker = list(color = "yellow")
)

trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("CHOISEUL SECONDARY SCHOOL", 
        "DACRETIN COMMUNITY CENTRE", 
        "DELCER COMBINED SCHOOL",
        "DUGARD COMBINED SCHOOL",
        "HANG OUT BAR JETRINE",
        "MONGOUGE INFANT SCHOOL",
        "PIAYE COMBINED SCHOOL",
        "ROBLOT COMBINED SCHOOL",
        "SALTIBUS COMBINED SCHOOL"), 
  y = c(ps_css_slp_votes, 
        ps_dcc_slp_votes, 
        ps_dcs_slp_votes,
        ps_ducs_slp_votes,
        ps_hobj_slp_votes,
        ps_mis_slp_votes,
        ps_pcs_slp_votes,
        ps_trcs_slp_votes,
        ps_scs_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_c_graph = plot_ly()
ps_c_graph = add_trace(ps_c_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_c_graph = add_trace(ps_c_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_c_graph = layout(ps_c_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)

###############################################################################
# Graph for Dennery North
###############################################################################
dn_summary = read.csv(file = "Dennery_North/Dennery_North.csv")

dn_party = c("SLP", "UWP") 
dn_votes = c(dn_summary$SLP, dn_summary$UWP) 
dn_results = data.frame(dn_party, dn_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
dn_results$party = with(dn_results, reorder(dn_party, -dn_votes))

# Bar graph showing election results in Dennery North District.
dn_graph = ggplot(dn_results, aes(x = dn_party, y = dn_votes, fill = dn_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

dn_graph = ggplotly(dn_graph, tooltip = c("dn_votes"))

###############################################################################
# Graph to show results by polling station in Dennery North District
###############################################################################
ps_alcs = read.csv(file = "Dennery_North/AU_LEON_COMBINED_SCHOOL.csv")
ps_drcs = read.csv(file = "Dennery_North/DERNIERE_RIVIERE_COMBINED_SCHOOL.csv")
ps_grss = read.csv(file = "Dennery_North/GRANDE_RIVIERE_SECONDARY_SCHOOL.csv")
ps_lrcs = read.csv(file = "Dennery_North/LA_RESSOURCE_COMBINED_SCHOOL.csv")
ps_ris = read.csv(file = "Dennery_North/RICHFOND_INFANT_SCHOOL.csv")

ps_alcs_uwp_votes = ps_alcs$UWP[which(ps_alcs$Section == "Sub")]
ps_drcs_uwp_votes = ps_drcs$UWP[which(ps_drcs$Section == "Sub")]
ps_grss_uwp_votes = ps_grss$UWP[which(ps_grss$Section == "Sub")]
ps_lrcs_uwp_votes = ps_lrcs$UWP[which(ps_lrcs$Section == "Sub")]
ps_ris_uwp_votes = ps_ris$UWP[which(ps_ris$Section == "Sub")]

ps_alcs_slp_votes = ps_alcs$SLP[which(ps_alcs$Section == "Sub")]
ps_drcs_slp_votes = ps_drcs$SLP[which(ps_drcs$Section == "Sub")]
ps_grss_slp_votes = ps_grss$SLP[which(ps_grss$Section == "Sub")]
ps_lrcs_slp_votes = ps_lrcs$SLP[which(ps_lrcs$Section == "Sub")]
ps_ris_slp_votes = ps_ris$SLP[which(ps_ris$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("AU LEON COMBINED SCHOOL", 
        "DERNIERE RIVIERE COMBINED SCHOOL", 
        "GRANDE RIVIERE SECONDARY SCHOOL",
        "LA RESSOURCE COMBINED SCHOOL",
        "RICHFOND INFANT SCHOOL"), 
  y = c(ps_alcs_uwp_votes, 
        ps_drcs_uwp_votes, 
        ps_grss_uwp_votes,
        ps_lrcs_uwp_votes,
        ps_ris_uwp_votes), 
  marker = list(color = "yellow")
)

trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("AU LEON COMBINED SCHOOL", 
        "DERNIERE RIVIERE COMBINED SCHOOL", 
        "GRANDE RIVIERE SECONDARY SCHOOL",
        "LA RESSOURCE COMBINED SCHOOL",
        "RICHFOND INFANT SCHOOL"), 
  y = c(ps_alcs_slp_votes, 
        ps_drcs_slp_votes, 
        ps_grss_slp_votes,
        ps_lrcs_slp_votes,
        ps_ris_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  height = 607, 
  autosize = TRUE
)
ps_dn_graph = plot_ly()
ps_dn_graph = add_trace(ps_dn_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_dn_graph = add_trace(ps_dn_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_dn_graph = layout(ps_dn_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)

###############################################################################
# Graph for Dennery South
###############################################################################
ds_summary = read.csv(file = "Dennery_South/Dennery_South.csv")

ds_party = c("INDEPENDENT", "SLP", "UWP") 
ds_votes = c(ds_summary$INDEP, ds_summary$SLP, ds_summary$UWP) 
ds_results = data.frame(ds_party, ds_votes)

color = c("blue", "red", "yellow")

# Reorder the factor levels before sending to ggplot.
ds_results$party = with(ds_results, reorder(ds_party, -ds_votes))

# Bar graph showing election results in Central Castries District.
ds_graph = ggplot(ds_results, aes(x = ds_party, y = ds_votes, fill = ds_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

ds_graph = ggplotly(ds_graph, tooltip = c("ds_votes"))

###############################################################################
# Graph to show results by polling station in Dennery South District
###############################################################################
ps_dis = read.csv(file = "Dennery_South/DENNERY_INFANT_SCHOOL.csv")
ps_dss = read.csv(file = "Dennery_South/DENNERY_SECONDARY_SCHOOL.csv")
ps_lcdcc = read.csv(file = "Dennery_South/LA_CAYE_DAY_CARE_CENTRE.csv")

ps_dis_uwp_votes = ps_dis$UWP[which(ps_dis$Section == "Sub")]
ps_dss_uwp_votes = ps_dss$UWP[which(ps_dss$Section == "Sub")]
ps_lcdcc_uwp_votes = ps_lcdcc$UWP[which(ps_lcdcc$Section == "Sub")]


ps_dis_slp_votes = ps_dis$SLP[which(ps_dis$Section == "Sub")]
ps_dss_slp_votes = ps_dss$SLP[which(ps_dss$Section == "Sub")]
ps_lcdcc_slp_votes = ps_lcdcc$SLP[which(ps_lcdcc$Section == "Sub")]

ps_dis_indep_votes = ps_dis$INDEP[which(ps_dis$Section == "Sub")]
ps_dss_indep_votes = ps_dss$INDEP[which(ps_dss$Section == "Sub")]
ps_lcdcc_indep_votes = ps_lcdcc$INDEP[which(ps_lcdcc$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("DENNERY INFANT SCHOOL", 
        "DENNERY SECONDARY SCHOOL", 
        "LA CAYE DAY CARE CENTRE"), 
  y = c(ps_dis_uwp_votes, 
        ps_dss_uwp_votes, 
        ps_lcdcc_uwp_votes), 
  marker = list(color = "yellow")
)
trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("DENNERY INFANT SCHOOL", 
        "DENNERY SECONDARY SCHOOL", 
        "LA CAYE DAY CARE CENTRE"), 
  y = c(ps_dis_slp_votes, 
        ps_dss_slp_votes, 
        ps_lcdcc_slp_votes), 
  marker = list(color = "red")
)
trace3 = list(
  uid = "IND", 
  name = "INDEPENDENT", 
  type = "bar", 
  x = c("DENNERY INFANT SCHOOL", 
        "DENNERY SECONDARY SCHOOL", 
        "LA CAYE DAY CARE CENTRE"), 
  y = c(ps_dis_indep_votes, 
        ps_dss_indep_votes, 
        ps_lcdcc_indep_votes), 
  marker = list(color = "blue")
)

data = list(trace1, trace2, trace3)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_ds_graph = plot_ly()
ps_ds_graph = add_trace(ps_ds_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_ds_graph = add_trace(ps_ds_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_ds_graph = add_trace(ps_ds_graph, uid=trace3$uid, name=trace3$name, type=trace3$type, x=trace3$x, y=trace3$y, marker=trace3$marker)
ps_ds_graph = layout(ps_ds_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)

###############################################################################
# Graph for Gros-Islet
###############################################################################
gi_summary = read.csv(file = "Gros_Islet/Gros_Islet.csv")

gi_party = c("LPM", "SLP", "UWP") 
gi_votes = c(gi_summary$LPM, gi_summary$SLP, gi_summary$UWP) 
gi_results = data.frame(gi_party, gi_votes)

color = c("green", "red", "yellow")

# Reorder the factor levels before sending to ggplot.
gi_results$party = with(gi_results, reorder(gi_party, -gi_votes))

# Bar graph showing election results in Gros-Islet District.
gi_graph = ggplot(gi_results, aes(x = gi_party, y = gi_votes, fill = gi_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

gi_graph = ggplotly(gi_graph, tooltip = c("gi_votes"))

###############################################################################
# Graph to show results by polling station in Gros-Islet District
###############################################################################
ps_css = read.csv(file = "Gros_Islet/CORINTH_SECONDARY_SCHOOL.csv")
ps_dss = read.csv(file = "Gros_Islet/DARREN_SAMMY_STADIUM.csv")
ps_grps = read.csv(file = "Gros_Islet/GRANDE_RIVIERE_PRIMARY_SCHOOL.csv")
ps_gips = read.csv(file = "Gros_Islet/GROS_ISLET_PRIMARY_SCHOOL.csv")
ps_giss = read.csv(file = "Gros_Islet/GROS_ISLET_SECONDARY_SCHOOL.csv")
ps_jc1 = read.csv(file = "Gros_Islet/JOHNSONS_CENTRE_1.csv")
ps_jc2 = read.csv(file = "Gros_Islet/JOHNSONS_CENTRE_2.csv")
ps_mcs = read.csv(file = "Gros_Islet/MONCHY_COMBINED_SCHOOL.csv")

ps_css_uwp_votes = ps_css$UWP[which(ps_css$Section == "Sub")]
ps_dss_uwp_votes = ps_dss$UWP[which(ps_dss$Section == "Sub")]
ps_grps_uwp_votes = ps_grps$UWP[which(ps_grps$Section == "Sub")]
ps_gips_uwp_votes = ps_gips$UWP[which(ps_gips$Section == "Sub")]
ps_giss_uwp_votes = ps_giss$UWP[which(ps_giss$Section == "Sub")]
ps_jc1_uwp_votes = ps_jc1$UWP[which(ps_jc1$Section == "Sub")]
ps_jc2_uwp_votes = ps_jc2$UWP[which(ps_jc2$Section == "A-Z")]
ps_mcs_uwp_votes = ps_mcs$UWP[which(ps_mcs$Section == "Sub")]

ps_css_slp_votes = ps_css$SLP[which(ps_css$Section == "Sub")]
ps_dss_slp_votes = ps_dss$SLP[which(ps_dss$Section == "Sub")]
ps_grps_slp_votes = ps_grps$SLP[which(ps_grps$Section == "Sub")]
ps_gips_slp_votes = ps_gips$SLP[which(ps_gips$Section == "Sub")]
ps_giss_slp_votes = ps_giss$SLP[which(ps_giss$Section == "Sub")]
ps_jc1_slp_votes = ps_jc1$SLP[which(ps_jc1$Section == "Sub")]
ps_jc2_slp_votes = ps_jc2$SLP[which(ps_jc2$Section == "A-Z")]
ps_mcs_slp_votes = ps_mcs$SLP[which(ps_mcs$Section == "Sub")]

ps_css_lpm_votes = ps_css$LPM[which(ps_css$Section == "Sub")]
ps_dss_lpm_votes = ps_dss$LPM[which(ps_dss$Section == "Sub")]
ps_grps_lpm_votes = ps_grps$LPM[which(ps_grps$Section == "Sub")]
ps_gips_lpm_votes = ps_gips$LPM[which(ps_gips$Section == "Sub")]
ps_giss_lpm_votes = ps_giss$LPM[which(ps_giss$Section == "Sub")]
ps_jc1_lpm_votes = ps_jc1$LPM[which(ps_jc1$Section == "Sub")]
ps_jc2_lpm_votes = ps_jc2$LPM[which(ps_jc2$Section == "A-Z")]
ps_mcs_lpm_votes = ps_mcs$LPM[which(ps_mcs$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("CORINTH SECONDARY SCHOOL", 
        "DARREN SAMMY STADIUM", 
        "GRANDE RIVIERE PRIMARY SCHOOL",
        "GROS ISLET PRIMARY SCHOOL",
        "GROS ISLET SECONDARY SCHOOL",
        "JOHNSONS CENTRE 1",
        "JOHNSONS CENTRE 2",
        "MONCHY COMBINED SCHOOL"), 
  y = c(ps_css_uwp_votes, 
        ps_dss_uwp_votes, 
        ps_grps_uwp_votes,
        ps_gips_uwp_votes,
        ps_giss_uwp_votes,
        ps_jc1_uwp_votes,
        ps_jc2_uwp_votes,
        ps_mcs_uwp_votes), 
  marker = list(color = "yellow")
)
trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("CORINTH SECONDARY SCHOOL", 
        "DARREN SAMMY STADIUM", 
        "GRANDE RIVIERE PRIMARY SCHOOL",
        "GROS ISLET PRIMARY SCHOOL",
        "GROS ISLET SECONDARY SCHOOL",
        "JOHNSONS CENTRE 1",
        "JOHNSONS CENTRE 2",
        "MONCHY COMBINED SCHOOL"), 
  y = c(ps_css_slp_votes, 
        ps_dss_slp_votes, 
        ps_grps_slp_votes,
        ps_gips_slp_votes,
        ps_giss_slp_votes,
        ps_jc1_slp_votes,
        ps_jc2_slp_votes,
        ps_mcs_slp_votes), 
  marker = list(color = "red")
)
trace3 = list(
  uid = "LPM", 
  name = "LPM", 
  type = "bar", 
  x = c("CORINTH SECONDARY SCHOOL", 
        "DARREN SAMMY STADIUM", 
        "GRANDE RIVIERE PRIMARY SCHOOL",
        "GROS ISLET PRIMARY SCHOOL",
        "GROS ISLET SECONDARY SCHOOL",
        "JOHNSONS CENTRE 1",
        "JOHNSONS CENTRE 2",
        "MONCHY COMBINED SCHOOL"), 
  y = c(ps_css_lpm_votes, 
        ps_dss_lpm_votes, 
        ps_grps_lpm_votes,
        ps_gips_lpm_votes,
        ps_giss_lpm_votes,
        ps_jc1_lpm_votes,
        ps_jc2_lpm_votes,
        ps_mcs_lpm_votes), 
  marker = list(color = "green")
)

data = list(trace1, trace2, trace3)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ),
  autosize = TRUE
)
ps_gi_graph = plot_ly()
ps_gi_graph = add_trace(ps_gi_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_gi_graph = add_trace(ps_gi_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_gi_graph = add_trace(ps_gi_graph, uid=trace3$uid, name=trace3$name, type=trace3$type, x=trace3$x, y=trace3$y, marker=trace3$marker)
ps_gi_graph = layout(ps_gi_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)

###############################################################################
# Graph for Laborie
###############################################################################
l_summary = read.csv(file = "Laborie/Laborie.csv")

l_party = c("SLP", "UWP") 
l_votes = c(l_summary$SLP, l_summary$UWP) 
l_results = data.frame(l_party, l_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
l_results$party = with(l_results, reorder(l_party, -l_votes))

# Bar graph showing election results in Laborie District.
l_graph = ggplot(l_results, aes(x = l_party, y = l_votes, fill = l_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

l_graph = ggplotly(l_graph, tooltip = c("l_votes"))

###############################################################################
# Graph to show results by polling station in Laborie District
###############################################################################
ps_acs = read.csv(file = "Laborie/AUGIER_COMBINED_SCHOOL.csv")
ps_lgcs = read.csv(file = "Laborie/LA_GRACE_COMBINED_SCHOOL.csv")
ps_lbs = read.csv(file = "Laborie/LABORIE_BOYS_SCHOOL.csv")
ps_lgps = read.csv(file = "Laborie/LABORIE_GIRLS_PRIMARY_SCHOOL.csv")
ps_tdh = read.csv(file = "Laborie/TEASERS_DANCE_HALL.csv")

ps_acs_uwp_votes = ps_acs$UWP[which(ps_acs$Section == "Sub")]
ps_lgcs_uwp_votes = ps_lgcs$UWP[which(ps_lgcs$Section == "Sub")]
ps_lbs_uwp_votes = ps_lbs$UWP[which(ps_lbs$Section == "Sub")]
ps_lgps_uwp_votes = ps_lgps$UWP[which(ps_lgps$Section == "Sub")]
ps_tdh_uwp_votes = ps_tdh$UWP[which(ps_tdh$Section == "Sub")]

ps_acs_slp_votes = ps_acs$SLP[which(ps_acs$Section == "Sub")]
ps_lgcs_slp_votes = ps_lgcs$SLP[which(ps_lgcs$Section == "Sub")]
ps_lbs_slp_votes = ps_lbs$SLP[which(ps_lbs$Section == "Sub")]
ps_lgps_slp_votes = ps_lgps$SLP[which(ps_lgps$Section == "Sub")]
ps_tdh_slp_votes = ps_tdh$SLP[which(ps_tdh$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("AUGIER COMBINED SCHOOL", 
        "LA_GRACE COMBINED SCHOOL", 
        "LABORIE BOYS SCHOOL",
        "LABORIE GIRLS PRIMARY SCHOOL",
        "TEASERS DANCE HALL"), 
  y = c(ps_acs_uwp_votes, 
        ps_lgcs_uwp_votes, 
        ps_lbs_uwp_votes,
        ps_lgps_uwp_votes,
        ps_tdh_uwp_votes), 
  marker = list(color = "yellow")
)

trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("AUGIER COMBINED SCHOOL", 
        "LA_GRACE COMBINED SCHOOL", 
        "LABORIE BOYS SCHOOL",
        "LABORIE GIRLS PRIMARY SCHOOL",
        "TEASERS DANCE HALL"), 
  y = c(ps_acs_slp_votes, 
        ps_lgcs_slp_votes, 
        ps_lbs_slp_votes,
        ps_lgps_slp_votes,
        ps_tdh_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_l_graph = plot_ly()
ps_l_graph = add_trace(ps_l_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_l_graph = add_trace(ps_l_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_l_graph = layout(ps_l_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


###############################################################################
# Graph for Micoud North
###############################################################################
mn_summary = read.csv(file = "Micoud_North/Micoud_North.csv")

mn_party = c("INDEP-1", "INDEP-2", "SLP", "UWP") 
mn_votes = c(mn_summary$INDEP_1, mn_summary$INDEP_2, mn_summary$SLP, mn_summary$UWP) 
mn_results = data.frame(mn_party, mn_votes)

color = c("blue", "lightblue", "red", "yellow")

# Reorder the factor levels before sending to ggplot.
mn_results$party = with(mn_results, reorder(mn_party, -mn_votes))

# Bar graph showing election results in Micoud North District.
mn_graph = ggplot(mn_results, aes(x = mn_party, y = mn_votes, fill = mn_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

mn_graph = ggplotly(mn_graph, tooltip = c("mn_votes"))

###############################################################################
# Graph to show results by polling station in Micoud North District
###############################################################################
ps_mis = read.csv(file = "Micoud_North/MICOUD_INFANT_SCHOOL.csv")
ps_mss = read.csv(file = "Micoud_North/MICOUD_SECONDARY_SCHOOL.csv")
ps_mrcs = read.csv(file = "Micoud_North/MON_REPOS_COMBINED_SCHOOL.csv")
ps_pcs = read.csv(file = "Micoud_North/PATIENCE_COMBINED_SCHOOL.csv")
ps_pcc = read.csv(file = "Micoud_North/PRASLIN_COMMUNITY_CENTRE.csv")

ps_mis_uwp_votes = ps_mis$UWP[which(ps_mis$Section == "Sub")]
ps_mss_uwp_votes = ps_mss$UWP[which(ps_mss$Section == "Sub")]
ps_mrcs_uwp_votes = ps_mrcs$UWP[which(ps_mrcs$Section == "Sub")]
ps_pcs_uwp_votes = ps_pcs$UWP[which(ps_pcs$Section == "Sub")]
ps_pcc_uwp_votes = ps_pcc$UWP[which(ps_pcc$Section == "A-Z")]

ps_mis_slp_votes = ps_mis$SLP[which(ps_mis$Section == "Sub")]
ps_mss_slp_votes = ps_mss$SLP[which(ps_mss$Section == "Sub")]
ps_mrcs_slp_votes = ps_mrcs$SLP[which(ps_mrcs$Section == "Sub")]
ps_pcs_slp_votes = ps_pcs$SLP[which(ps_pcs$Section == "Sub")]
ps_pcc_slp_votes = ps_pcc$SLP[which(ps_pcc$Section == "A-Z")]

ps_mis_indep_1_votes = ps_mis$INDEP_1[which(ps_mis$Section == "Sub")]
ps_mss_indep_1_votes = ps_mss$INDEP_1[which(ps_mss$Section == "Sub")]
ps_mrcs_indep_1_votes = ps_mrcs$INDEP_1[which(ps_mrcs$Section == "Sub")]
ps_pcs_indep_1_votes = ps_pcs$INDEP_1[which(ps_pcs$Section == "Sub")]
ps_pcc_indep_1_votes = ps_pcc$INDEP_1[which(ps_pcc$Section == "A-Z")]

ps_mis_indep_2_votes = ps_mis$INDEP_2[which(ps_mis$Section == "Sub")]
ps_mss_indep_2_votes = ps_mss$INDEP_2[which(ps_mss$Section == "Sub")]
ps_mrcs_indep_2_votes = ps_mrcs$INDEP_2[which(ps_mrcs$Section == "Sub")]
ps_pcs_indep_2_votes = ps_pcs$INDEP_2[which(ps_pcs$Section == "Sub")]
ps_pcc_indep_2_votes = ps_pcc$INDEP_2[which(ps_pcc$Section == "A-Z")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("MICOUD INFANT SCHOOL", 
        "MICOUD SECONDARY SCHOOL", 
        "MON REPOS COMBINED SCHOOL",
        "PATIENCE COMBINED SCHOOL",
        "PRASLIN COMMUNITY CENTRE"), 
  y = c(ps_mis_uwp_votes, 
        ps_mss_uwp_votes, 
        ps_mrcs_uwp_votes,
        ps_pcs_uwp_votes,
        ps_pcc_uwp_votes), 
  marker = list(color = "yellow")
)
trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("MICOUD INFANT SCHOOL", 
        "MICOUD SECONDARY SCHOOL", 
        "MON REPOS COMBINED SCHOOL",
        "PATIENCE COMBINED SCHOOL",
        "PRASLIN COMMUNITY CENTRE"), 
  y = c(ps_mis_slp_votes, 
        ps_mss_slp_votes, 
        ps_mrcs_slp_votes,
        ps_pcs_slp_votes,
        ps_pcc_slp_votes), 
  marker = list(color = "red")
)
trace3 = list(
  uid = "IND1", 
  name = "INDEP-1", 
  type = "bar", 
  x = c("MICOUD INFANT SCHOOL", 
        "MICOUD SECONDARY SCHOOL", 
        "MON REPOS COMBINED SCHOOL",
        "PATIENCE COMBINED SCHOOL",
        "PRASLIN COMMUNITY CENTRE"), 
  y = c(ps_mis_indep_1_votes, 
        ps_mss_indep_1_votes, 
        ps_mrcs_indep_1_votes,
        ps_pcs_indep_1_votes,
        ps_pcc_indep_1_votes), 
  marker = list(color = "blue")
)
trace4 = list(
  uid = "IND2", 
  name = "INDEP-2", 
  type = "bar", 
  x = c("MICOUD INFANT SCHOOL", 
        "MICOUD SECONDARY SCHOOL", 
        "MON REPOS COMBINED SCHOOL",
        "PATIENCE COMBINED SCHOOL",
        "PRASLIN COMMUNITY CENTRE"), 
  y = c(ps_mis_indep_2_votes, 
        ps_mss_indep_2_votes, 
        ps_mrcs_indep_2_votes,
        ps_pcs_indep_2_votes,
        ps_pcc_indep_2_votes), 
  marker = list(color = "lightblue")
)

data = list(trace1, trace2, trace3, trace4)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_mn_graph = plot_ly()
ps_mn_graph = add_trace(ps_mn_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_mn_graph = add_trace(ps_mn_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_mn_graph = add_trace(ps_mn_graph, uid=trace3$uid, name=trace3$name, type=trace3$type, x=trace3$x, y=trace3$y, marker=trace3$marker)
ps_mn_graph = add_trace(ps_mn_graph, uid=trace4$uid, name=trace4$name, type=trace4$type, x=trace4$x, y=trace4$y, marker=trace4$marker)
ps_mn_graph = layout(ps_mn_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)

###############################################################################
# Graph for Micoud South
###############################################################################
ms_summary = read.csv(file = "Micoud_South/Micoud_South.csv")

ms_party = c("SLP", "UWP") 
ms_votes = c(ms_summary$SLP, ms_summary$UWP) 
ms_results = data.frame(ms_party, ms_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
ms_results$party = with(ms_results, reorder(ms_party, -ms_votes))

# Bar graph showing election results in Micoud South District.
ms_graph = ggplot(ms_results, aes(x = ms_party, y = ms_votes, fill = ms_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

ms_graph = ggplotly(ms_graph, tooltip = c("ms_votes"))

###############################################################################
# Graph to show results by polling station in Micoud South District
###############################################################################
ps_agss = read.csv(file = "Micoud_South/ANSE_GER_SECONDARY_SCHOOL.csv")
ps_bcs = read.csv(file = "Micoud_South/BLANCHARD_COMBINED_SCHOOL.csv")
ps_dcs = read.csv(file = "Micoud_South/DESRUISSEAUX_COMBINED_SCHOOL.csv")
ps_dcc = read.csv(file = "Micoud_South/DUGARD_COMMUNITY_CENTRE.csv")
ps_lcvcc = read.csv(file = "Micoud_South/LA_COUR_VILLE_COMMUNITY_CENTRE.csv")
ps_trcc = read.csv(file = "Micoud_South/TI_ROCHER_COMMUNITY_CENTRE.csv")

ps_agss_uwp_votes = ps_agss$UWP[which(ps_agss$Section == "Sub")]
ps_bcs_uwp_votes = ps_bcs$UWP[which(ps_bcs$Section == "Sub")]
ps_dcs_uwp_votes = ps_dcs$UWP[which(ps_dcs$Section == "Sub")]
ps_dcc_uwp_votes = ps_dcc$UWP[which(ps_dcc$Section == "Sub")]
ps_lcvcc_uwp_votes = ps_lcvcc$UWP[which(ps_lcvcc$Section == "A-Z")]
ps_trcc_uwp_votes = ps_trcc$UWP[which(ps_trcc$Section == "Sub")]

ps_agss_slp_votes = ps_agss$SLP[which(ps_agss$Section == "Sub")]
ps_bcs_slp_votes = ps_bcs$SLP[which(ps_bcs$Section == "Sub")]
ps_dcs_slp_votes = ps_dcs$SLP[which(ps_dcs$Section == "Sub")]
ps_dcc_slp_votes = ps_dcc$SLP[which(ps_dcc$Section == "Sub")]
ps_lcvcc_slp_votes = ps_lcvcc$SLP[which(ps_lcvcc$Section == "A-Z")]
ps_trcc_slp_votes = ps_trcc$SLP[which(ps_trcc$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("ANSE GER SECONDARY SCHOOL", 
        "BLANCHARD COMBINED SCHOOL", 
        "DESRUISSEAUX COMBINED SCHOOL",
        "DUGARD COMMUNITY CENTRE",
        "LA COUR VILLE COMMUNITY CENTRE",
        "TI ROCHER COMMUNITY CENTRE"), 
  y = c(ps_agss_uwp_votes, 
        ps_bcs_uwp_votes, 
        ps_dcs_uwp_votes,
        ps_dcc_uwp_votes,
        ps_lcvcc_uwp_votes,
        ps_trcc_uwp_votes), 
  marker = list(color = "yellow")
)

trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("ANSE GER SECONDARY SCHOOL", 
        "BLANCHARD COMBINED SCHOOL", 
        "DESRUISSEAUX COMBINED SCHOOL",
        "DUGARD COMMUNITY CENTRE",
        "LA COUR VILLE COMMUNITY CENTRE",
        "TI ROCHER COMMUNITY CENTRE"), 
  y = c(ps_agss_slp_votes, 
        ps_bcs_slp_votes, 
        ps_dcs_slp_votes,
        ps_dcc_slp_votes,
        ps_lcvcc_slp_votes,
        ps_trcc_uwp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_ms_graph = plot_ly()
ps_ms_graph = add_trace(ps_ms_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_ms_graph = add_trace(ps_ms_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_ms_graph = layout(ps_ms_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


###############################################################################
# Graph for Soufriere
###############################################################################
s_summary = read.csv(file = "Soufriere/Soufriere.csv")

s_party = c("LG", "SLP", "UWP") 
s_votes = c(s_summary$LG, s_summary$SLP, s_summary$UWP) 
s_results = data.frame(s_party, s_votes)

color = c("purple", "red", "yellow")

# Reorder the factor levels before sending to ggplot.
s_results$party = with(s_results, reorder(s_party, -s_votes))

# Bar graph showing election results in Soufriere District.
s_graph = ggplot(s_results, aes(x = s_party, y = s_votes, fill = s_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

s_graph = ggplotly(s_graph, tooltip = c("s_votes"))

###############################################################################
# Graph to show results by polling station in Soufriere District
###############################################################################
ps_bcs = read.csv(file = "Soufriere/BOUTON_COMBINED_SCHOOL.csv")
ps_ecs = read.csv(file = "Soufriere/ETANGS_COMBINED_SCHOOL.csv")
ps_fsjpsa = read.csv(file = "Soufriere/FOND_ST_JACQUES_PRIMARY_SCHOOL_A.csv")
ps_fsjpsb = read.csv(file = "Soufriere/FOND_ST_JACQUES_PRIMARY_SCHOOL_B.csv")
ps_sis = read.csv(file = "Soufriere/SOUFRIERE_INFANT_SCHOOL.csv")
ps_sps = read.csv(file = "Soufriere/SOUFRIERE_PRIMARY_SCHOOL.csv")
ps_sih = read.csv(file = "Soufriere/ST_ISIDORES_HALL.csv")

ps_bcs_uwp_votes = ps_bcs$UWP[which(ps_bcs$Section == "A-Z")]
ps_ecs_uwp_votes = ps_ecs$UWP[which(ps_ecs$Section == "Sub")]
ps_fsjpsa_uwp_votes = ps_fsjpsa$UWP[which(ps_fsjpsa$Section == "Sub")]
ps_fsjpsb_uwp_votes = ps_fsjpsb$UWP[which(ps_fsjpsb$Section == "Sub")]
ps_sis_uwp_votes = ps_sis$UWP[which(ps_sis$Section == "Sub")]
ps_sps_uwp_votes = ps_sps$UWP[which(ps_sps$Section == "Sub")]
ps_sih_uwp_votes = ps_sih$UWP[which(ps_sih$Section == "Sub")]


ps_bcs_slp_votes = ps_bcs$SLP[which(ps_bcs$Section == "A-Z")]
ps_ecs_slp_votes = ps_ecs$SLP[which(ps_ecs$Section == "Sub")]
ps_fsjpsa_slp_votes = ps_fsjpsa$SLP[which(ps_fsjpsa$Section == "Sub")]
ps_fsjpsb_slp_votes = ps_fsjpsb$SLP[which(ps_fsjpsb$Section == "Sub")]
ps_sis_slp_votes = ps_sis$SLP[which(ps_sis$Section == "Sub")]
ps_sps_slp_votes = ps_sps$SLP[which(ps_sps$Section == "Sub")]
ps_sih_slp_votes = ps_sih$SLP[which(ps_sih$Section == "Sub")]

ps_bcs_lg_votes = ps_bcs$LG[which(ps_bcs$Section == "A-Z")]
ps_ecs_lg_votes = ps_ecs$LG[which(ps_ecs$Section == "Sub")]
ps_fsjpsa_lg_votes = ps_fsjpsa$LG[which(ps_fsjpsa$Section == "Sub")]
ps_fsjpsb_lg_votes = ps_fsjpsb$LG[which(ps_fsjpsb$Section == "Sub")]
ps_sis_lg_votes = ps_sis$LG[which(ps_sis$Section == "Sub")]
ps_sps_lg_votes = ps_sps$LG[which(ps_sps$Section == "Sub")]
ps_sih_lg_votes = ps_sih$LG[which(ps_sih$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("BOUTON COMBINED SCHOOL", 
        "ETANGS COMBINED SCHOOL", 
        "FOND ST JACQUES PRIMARY SCHOOL A",
        "FOND ST JACQUES PRIMARY SCHOOL B",
        "SOUFRIERE INFANT SCHOOL",
        "SOUFRIERE PRIMARY SCHOOL",
        "ST ISIDORES HALL"), 
  y = c(ps_bcs_uwp_votes, 
        ps_ecs_uwp_votes, 
        ps_fsjpsa_uwp_votes,
        ps_fsjpsb_uwp_votes,
        ps_sis_uwp_votes,
        ps_sps_uwp_votes,
        ps_sih_uwp_votes), 
  marker = list(color = "yellow")
)
trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("BOUTON COMBINED SCHOOL", 
        "ETANGS COMBINED SCHOOL", 
        "FOND ST JACQUES PRIMARY SCHOOL A",
        "FOND ST JACQUES PRIMARY SCHOOL B",
        "SOUFRIERE INFANT SCHOOL",
        "SOUFRIERE PRIMARY SCHOOL",
        "ST ISIDORES HALL"), 
  y = c(ps_bcs_slp_votes, 
        ps_ecs_slp_votes, 
        ps_fsjpsa_slp_votes,
        ps_fsjpsb_slp_votes,
        ps_sis_slp_votes,
        ps_sps_slp_votes,
        ps_sih_slp_votes), 
  marker = list(color = "red")
)
trace3 = list(
  uid = "LG", 
  name = "LG", 
  type = "bar", 
  x = c("BOUTON COMBINED SCHOOL", 
        "ETANGS COMBINED SCHOOL", 
        "FOND ST JACQUES PRIMARY SCHOOL A",
        "FOND ST JACQUES PRIMARY SCHOOL B",
        "SOUFRIERE INFANT SCHOOL",
        "SOUFRIERE PRIMARY SCHOOL",
        "ST ISIDORES HALL"), 
  y = c(ps_bcs_lg_votes, 
        ps_ecs_lg_votes, 
        ps_fsjpsa_lg_votes,
        ps_fsjpsb_lg_votes,
        ps_sis_lg_votes,
        ps_sps_lg_votes,
        ps_sih_lg_votes), 
  marker = list(color = "purple")
)

data = list(trace1, trace2, trace3)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ),
  autosize = TRUE
)
ps_s_graph = plot_ly()
ps_s_graph = add_trace(ps_s_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_s_graph = add_trace(ps_s_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_s_graph = add_trace(ps_s_graph, uid=trace3$uid, name=trace3$name, type=trace3$type, x=trace3$x, y=trace3$y, marker=trace3$marker)
ps_s_graph = layout(ps_s_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


###############################################################################
# Graph for Vieux-Fort North
###############################################################################
vfn_summary = read.csv(file = "Vieux_Fort_North/Vieux_Fort_North.csv")

vfn_party = c("SLP", "UWP") 
vfn_votes = c(vfn_summary$SLP, vfn_summary$UWP) 
vfn_results = data.frame(vfn_party, vfn_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
vfn_results$party = with(vfn_results, reorder(vfn_party, -vfn_votes))

# Bar graph showing election results in Vieux-Fort North District.
vfn_graph = ggplot(vfn_results, aes(x = vfn_party, y = vfn_votes, fill = vfn_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

vfn_graph = ggplotly(vfn_graph, tooltip = c("vfn_votes"))

###############################################################################
# Graph to show results by polling station in Vieux-Fort North District
#############################################################################
ps_bvcs = read.csv(file = "Vieux_Fort_North/BELLE_VUE_COMBINED_SCHOOL.csv")
ps_gcs = read.csv(file = "Vieux_Fort_North/GRACE_COMBINED_SCHOOL.csv")
ps_pcs = read.csv(file = "Vieux_Fort_North/PIERROT_COMBINED_SCHOOL.csv")
ps_vcs = read.csv(file = "Vieux_Fort_North/VIGIER_COMBINED_SCHOOL.csv")

ps_bvcs_uwp_votes = ps_bvcs$UWP[which(ps_bvcs$Section == "Sub")]
ps_gcs_uwp_votes = ps_gcs$UWP[which(ps_gcs$Section == "Sub")]
ps_pcs_uwp_votes = ps_pcs$UWP[which(ps_pcs$Section == "Sub")]
ps_vcs_uwp_votes = ps_vcs$UWP[1]

ps_bvcs_slp_votes = ps_bvcs$SLP[which(ps_bvcs$Section == "Sub")]
ps_gcs_slp_votes = ps_gcs$SLP[which(ps_gcs$Section == "Sub")]
ps_pcs_slp_votes = ps_pcs$SLP[which(ps_pcs$Section == "Sub")]
ps_vcs_slp_votes = ps_vcs$SLP[1]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("BELLE VUE COMBINED SCHOOL", 
        "GRACE COMBINED SCHOOL", 
        "PIERROT COMBINED SCHOOL",
        "VIGIER COMBINED SCHOOL"), 
  y = c(ps_bvcs_uwp_votes, 
        ps_gcs_uwp_votes, 
        ps_pcs_uwp_votes,
        ps_vcs_uwp_votes), 
  marker = list(color = "yellow")
)

trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("BELLE VUE COMBINED SCHOOL", 
        "GRACE COMBINED SCHOOL", 
        "PIERROT COMBINED SCHOOL",
        "VIGIER COMBINED SCHOOL"), 
  y = c(ps_bvcs_slp_votes, 
        ps_gcs_slp_votes, 
        ps_pcs_slp_votes,
        ps_vcs_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_vfn_graph = plot_ly()
ps_vfn_graph = add_trace(ps_vfn_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_vfn_graph = add_trace(ps_vfn_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_vfn_graph = layout(ps_vfn_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


###############################################################################
# Graph for Vieux-Fort South
###############################################################################
vfs_summary = read.csv(file = "Vieux_Fort_South/Vieux_Fort_South.csv")

vfs_party = c("SLP", "UWP") 
vfs_votes = c(vfs_summary$SLP, vfs_summary$UWP) 
vfs_results = data.frame(vfs_party, vfs_votes)

color = c("red", "yellow")

# Reorder the factor levels before sending to ggplot.
vfs_results$party = with(vfs_results, reorder(vfs_party, -vfs_votes))

# Bar graph showing election results in Vieux-Fort South District.
vfs_graph = ggplot(vfs_results, aes(x = vfn_party, y = vfs_votes, fill = vfs_party)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Party") + ylab("Votes") + 
  scale_fill_manual(values = color) + 
  theme_minimal()

vfs_graph = ggplotly(vfs_graph, tooltip = c("vfs_votes"))

###############################################################################
# Graph to show results by polling station in Vieux-Fort South District
#############################################################################
ps_pvcs = read.csv(file = "Vieux_Fort_South/PLAIN_VIEW_COMBINED_SCHOOL.csv")
ps_vfis = read.csv(file = "Vieux_Fort_South/VIEUX_FORT_INFANT_SCHOOL.csv")
ps_vfps = read.csv(file = "Vieux_Fort_South/VIEUX_FORT_PRIMARY_SCHOOL.csv")
ps_vfscb = read.csv(file = "Vieux_Fort_South/VIEUX_FORT_SECONDARY_CAMPUS_B.csv")

ps_pvcs_uwp_votes = ps_pvcs$UWP[which(ps_pvcs$Section == "Sub")]
ps_vfis_uwp_votes = ps_vfis$UWP[which(ps_vfis$Section == "Sub")]
ps_vfps_uwp_votes = ps_vfps$UWP[which(ps_vfps$Section == "Sub")]
ps_vfscb_uwp_votes = ps_vfscb$UWP[which(ps_vfscb$Section == "Sub")]

ps_pvcs_slp_votes = ps_pvcs$SLP[which(ps_pvcs$Section == "Sub")]
ps_vfis_slp_votes = ps_vfis$SLP[which(ps_vfis$Section == "Sub")]
ps_vfps_slp_votes = ps_vfps$SLP[which(ps_vfps$Section == "Sub")]
ps_vfscb_slp_votes = ps_vfscb$SLP[which(ps_vfscb$Section == "Sub")]

trace1 = list(
  uid = "uwp", 
  name = "UWP", 
  type = "bar", 
  x = c("PLAIN VIEW COMBINED SCHOOL", 
        "VIEUX FORT INFANT SCHOOL", 
        "VIEUX FORT PRIMARY SCHOOL",
        "VIEUX FORT SECONDARY CAMPUS B"), 
  y = c(ps_pvcs_uwp_votes, 
        ps_vfis_uwp_votes, 
        ps_vfps_uwp_votes,
        ps_vfscb_uwp_votes), 
  marker = list(color = "yellow")
)

trace2 = list(
  uid = "slp", 
  name = "SLP", 
  type = "bar", 
  x = c("PLAIN VIEW COMBINED SCHOOL", 
        "VIEUX FORT INFANT SCHOOL", 
        "VIEUX FORT PRIMARY SCHOOL",
        "VIEUX FORT SECONDARY CAMPUS B"), 
  y = c(ps_pvcs_slp_votes, 
        ps_vfis_slp_votes, 
        ps_vfps_slp_votes,
        ps_vfscb_slp_votes), 
  marker = list(color = "red")
)

data = list(trace1, trace2)
layout = list(
  xaxis = list(
    type = "category", 
    title = "Polling Stations", 
    autorange = TRUE
  ), 
  yaxis = list(
    title = "Votes", 
    autorange = TRUE
  ), 
  autosize = TRUE
)
ps_vfs_graph = plot_ly()
ps_vfs_graph = add_trace(ps_vfs_graph, uid=trace1$uid, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker)
ps_vfs_graph = add_trace(ps_vfs_graph, uid=trace2$uid, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, marker=trace2$marker)
ps_vfs_graph = layout(ps_vfs_graph, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, autosize=layout$autosize)


function(input, output) { 
  output$generalResults = renderPlotly({generalResults})
  output$police = renderPlotly({police})
  output$rejectedVotes = renderPlotly({r_votes_graph})
  output$anseLaRayeCanaries = renderPlotly({alrc_graph})
  output$anseLaRayeCanariesPS = renderPlotly({ps_alrc_graph})
  output$babonneau = renderPlotly({b_graph})
  output$babonneauPS = renderPlotly({ps_b_graph})
  output$castriesCentral = renderPlotly({cc_graph})
  output$castriesCentralPS = renderPlotly({ps_cc_graph})
  output$castriesEast = renderPlotly({ce_graph})
  output$castriesEastPS = renderPlotly({ps_ce_graph})
  output$castriesNorth = renderPlotly({cn_graph})
  output$castriesNorthPS = renderPlotly({ps_cn_graph})
  output$castriesSouth = renderPlotly({cs_graph})
  output$castriesSouthPS = renderPlotly({ps_cs_graph})
  output$castriesSouthEast = renderPlotly({cse_graph})
  output$castriesSouthEastPS = renderPlotly({ps_cse_graph})
  output$choiseul = renderPlotly({c_graph})
  output$choiseulPS = renderPlotly({ps_c_graph})
  output$denneryNorth = renderPlotly({dn_graph})
  output$denneryNorthPS = renderPlotly({ps_dn_graph})
  output$dennerySouth = renderPlotly({ds_graph})
  output$dennerySouthPS = renderPlotly({ps_ds_graph})
  output$grosIslet = renderPlotly({gi_graph})
  output$grosIsletPS = renderPlotly({ps_gi_graph})
  output$laborie = renderPlotly({l_graph})
  output$laboriePS = renderPlotly({ps_l_graph})
  output$micoudNorth = renderPlotly({mn_graph})
  output$micoudNorthPS = renderPlotly({ps_mn_graph})
  output$micoudSouth = renderPlotly({ms_graph})
  output$micoudSouthPS = renderPlotly({ps_ms_graph})
  output$soufriere = renderPlotly({s_graph})
  output$soufrierePS = renderPlotly({ps_s_graph})
  output$vieuxFortNorth = renderPlotly({vfn_graph})
  output$vieuxFortNorthPS = renderPlotly({ps_vfn_graph})
  output$vieuxFortSouth = renderPlotly({vfs_graph})
  output$vieuxFortSouthPS = renderPlotly({ps_vfs_graph})
  
  # # Get number of rows from dataset
  # records = nrow(candidatesDF)
  # 
  # # Each row will have max. 3 items
  # fluidRows = ceiling(nrow(candidatesDF) / 3)
  # fluidRows = c(1:fluidRows)
  # 
  # count = 0
  # offset = 3
  
  #output$candidateList = renderUI({
    # for (row in fluidRows) {
    #   fluidRow({
    #     if (records < offset){
    #       offset = records
    #     }
    #     for (column in 1:offset) {
    #       count = count + 1
    # 
    #       # Convert the names to a more legible format
    #       name = explode(candidatesDF[count, "Candidate"], sep = ", ")
    #       name = paste0(name[2], ' ', name[1])
    #       name = capitalizeStrings(name, all.words = TRUE, lower.back = TRUE)
    # 
    #       # Convert the names to the img name format
    #       imgName = explode(name, sep = " ")
    #       imgName = tolower(implode(imgName, "_"))
    #       imgUrl = paste0("img/", imgName, ".png")
    # 
    #       # Create a user card on each iteration.
    #       bs4UserCard(
    #         title = name,
    #         subtitle = candidatesDF[count, "Party"],
    #         type = NULL,
    #         width = 4,
    #         src = imgUrl,
    #         status = "danger",
    #         closable = TRUE,
    #         elevation = 4,
    #         imageElevation = 4,
    #         fluidRow(
    #           column(
    #             width = 4,
    #             descriptionBlock(header = "District",
    #                              text = capitalizeStrings(candidatesDF[count, "District"],
    #                                                       all.words = TRUE, lower.back = TRUE ))
    #           ),
    #           column(
    #             width = 4,
    #             descriptionBlock(header = "Votes",
    #                              text = candidatesDF[count, "Votes"])
    #           ),
    #           column(
    #             width = 4,
    #             descriptionBlock(header = "Result",
    #                              text = candidatesDF[count, "Result"], right_border = FALSE)
    #           )
    #         )
    #       )
    #       records = records - 1
    #     }
    #   })
    # }
  #})
} 