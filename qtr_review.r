# Denver Metro TB Control Program
# Quarterly Strategic Area Review
# Fourth Quarter 2012

options(stringsAsFactors = FALSE)

require(ggplot2)
require(plyr)



## Clinic and Outreach Visits

source("fun/query_visits.r")

visits <- query_visits(start_date = as.Date("2009-01-01"),
                       stop_date = as.Date("2012-12-31"))

visits$plot_qtr <- with(visits, paste(visit_yr, " Q", visit_qtr, sep = ""))

locagg <- count(subset(visits, location %in% c("Clinic", "Outreach")),
                vars = c("plot_qtr", "location"))

qtragg <- count(subset(visits, location %in% c("Clinic", "Outreach")),
                vars = "plot_qtr")

qtragg$location <- "All Visits"

visitagg <- rbind(locagg, qtragg)

ggplot(visitagg, aes(x = plot_qtr, y = freq, 
                     group = location, color = location)) +
    geom_point(size = 3) +
    geom_line(size = 1.3) +
    expand_limits(y = 0) +
    scale_color_discrete("Visit Location") +
    labs(x = "Visit Date", 
         y = "Number of Visits",
         title = "Patient Visits by Location") +
    theme_bw()





## Screening for Latent TB Infection






## Reported TB Cases by Quarter and County of Residence

source("fun/query_actives.r")


actives <- query_actives(start_date = as.Date("2011-01-01"),
                         stop_date = as.Date("2012-12-31"))


actives$plot_qtr <- with(actives, paste(yr_id, " Q", qtr_id, sep = ""))

actives$plot_group <- NA
actives$plot_group[actives$report_county %in% "Denver"] <- "Denver"
actives$plot_group[actives$metro_case %in% "True" & 
                 !actives$report_county %in% "Denver"] <- "Other Metro Counties"

ggplot(subset(actives, !is.na(plot_group)),
       aes(x = plot_qtr, fill = plot_group)) +
    geom_bar(color = "black") +
    scale_fill_manual("Patient's Residence", values = c("#ADDD8E", "#31A354")) +
    labs(x = "Quarter Identified (earliest of date treatment started or date case reported)", 
         y = "Number of cases",
         title = "Metro TB Cases by Residence") +
    theme_bw()





## Active TB Treatment Initiation - Reported Cases vs. Suspects




## Treatment Initiation - Active and Latent




## Active Treatment Completion (Overall)




## Active Treatment Completion (expected to complete in 12 months)



## LTBI Treatment Completion (expected to complete in 4 months)




## LTBI Treatment Completion (expected to complete in 9 months)




