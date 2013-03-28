


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


# Get only the first plan of each type for each person
firstplan <- plans[!duplicated(plans[ , c("person_id", "treat_plan_type")]), ]


# How many plans were started in each year?
dcast(plans, start_year ~ treat_plan_type, fun.agg = length)
dcast(firstplan, start_year ~ treat_plan_type, fun.agg = length)

# Quarter?
dcast(plans, start_year + start_qtr ~ treat_plan_type, fun.agg = length)
dcast(firstplan, start_year + start_qtr ~ treat_plan_type, fun.agg = length)

