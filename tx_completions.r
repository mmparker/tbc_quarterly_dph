


# Strings ain't factors
options(stringsAsFactors = FALSE)



library(RODBC)



# Load up the treatment plans
plus <- odbcConnect("tbdbplus64")


plans <- sqlQuery(plus, "

    SELECT person_id,
           treat_plan_type,
		   ltbi_drug,
           treat_plan_date,
           treat_plan_end,
           reason_stopped,
           completion_status,
           n_tx_completed,
		   days_to_end
    FROM Tx_Plan_View
    WHERE treat_plan_type IN ('LTBI', 'Active')
        AND plan_author IN (
            SELECT staff_name
            FROM Def_staff
            WHERE affiliation = 'Denver Metro TB Clinic'
        )
        AND n_tx_completed IS NOT NULL

")


odbcClose(plus)


# Add a starting year and quarter to each plan
plans$start_year <- format(plans$treat_plan_date,
                           format = "%Y")

plans$start_month <- format(plans$treat_plan_date,
                            format = "%m")

plans$start_qtr <- (as.numeric(plans$start_month) + 2) %/% 3


# Get the most recent plan from each person
latestplan <- ddply(plans, .var = "person_id", .fun = function(x) {

    x.sorted <- x[order(x$treat_plan_date), ]

    x.latest <- x.sorted[nrow(x.sorted), ]

})



# Summarize by year
dcast(latestplan, start_year ~ completion_status, fun.agg = length)


# By year and quarter



x <- subset(latestplan, 
            treat_plan_date >= as.POSIXct("2011-03-01") &
            treat_plan_date < as.POSIXct("2013-04-01")
)


ddply(x, .var = c("treat_plan_type", "start_year"), .fun = summarise,
      n = length(completion_status),
      n.complete = sum(completion_status %in% "Completed"),
      prop.complete = round(n.complete / n * 100, 2),
      n.not = sum(completion_status %in% "Not Completed"),
      prop.not = round(n.not / n * 100, 2),
      n.ongoing = sum(completion_status %in% "Ongoing"),
      prop.ongoing = round(n.ongoing / n * 100, 2),
      n.other = sum(completion_status %in% "Other"),
      prop.other = round(n.other / n * 100, 2),
      n.unk = sum(completion_status %in% "Unknown"),
      prop.unk = round(n.unk / n * 100, 2)
)


# Report out ongoing LTBI plans more than 9 months old
ongoing.270 <- subset(latestplan,
    subset = completion_status %in% "Ongoing" &
             treat_plan_type %in% "LTBI" &
             (Sys.Date() - as.Date(treat_plan_date) > 270),
    select = c("person_id", "ltbi_drug", "treat_plan_date", "reason_stopped",
               "n_tx_completed")
)

write.csv(arrange(ongoing.270, treat_plan_date),
          file = file.path("output", "LTBI treatments more than 9 months.csv"),
          row.names = FALSE)



# Report out Unknown plans
unknown <- subset(latestplan,
    subset = completion_status %in% "Unknown",
    select = c("person_id", "ltbi_drug", "treat_plan_date", "reason_stopped",
               "n_tx_completed")
)

write.csv(arrange(unknown, treat_plan_date),
          file = file.path("output", "LTBI treatments more than 9 months.csv"),
          row.names = FALSE)


