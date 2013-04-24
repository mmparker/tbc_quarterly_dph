


# This function provides a standardized method for querying 
# treatment plan status
# including plan type, start date, status, end date (if completed),



query_tx_plans <- function(start_date,
                           stop_date = Sys.Date(),
                           odbc = "tbdbplus64") {

    # TODO: argument validation

    require(RODBC)

    dbconnect <- odbcConnect(odbc)

    plans <- sqlQuery(dbconnect, paste(
        "SELECT person_id,
                treat_plan,
                treat_plan_type,
                ltbi_drug,
                plan_author,
                author_affiliation,
                treat_plan_date,
                plan_status,
                reason_stopped,
                completion_status,
                treat_plan_end,
                n_tx_completed,
                days_to_end
         FROM Tx_Plan_View
         WHERE author_affiliation = 'Denver Metro TB Clinic'
            AND treat_plan_date BETWEEN #", 
            start_date, 
            "# AND #", 
            stop_date, 
            "#",
            "ORDER BY person_id, treat_plan_date DESC",
        sep = "")
    )

    odbcClose(dbconnect)

    
    # Select the most recent plan for each person
    latest_plan <- plans[!duplicated(plans$person_id), ]



    # Add month-, quarter-, and year-of-plan variables for ease of aggregation
    latest_plan$plan_mon <- as.character(
        format(latest_plan$treat_plan_date, format = "%m")
    )

    latest_plan$plan_yr <- as.character(
        format(latest_plan$treat_plan_date, format = "%Y")
    )

    latest_plan$plan_qtr <- (as.numeric(latest_plan$plan_mon) + 2) %/% 3


    latest_plan

}
