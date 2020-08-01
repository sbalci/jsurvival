
# This file is a generated template, your changes will not be overwritten

timeintervalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "timeintervalClass",
    inherit = timeintervalBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            mydata <- self$data

            time <- self$options$time
            event <- self$options$event
            expl <- self$options$expl



            expl <- unlist(expl)

            # Time Interval ----

            # mydata$int <- lubridate::interval(
            #     lubridate::ymd(mydata$SurgeryDate),
            #     lubridate::ymd(mydata$LastFollowUpDate)
            # )
            # mydata$OverallTime <- lubridate::time_length(mydata$int, "month")
            # mydata$OverallTime <- round(mydata$OverallTime, digits = 1)




            myoveralltime <- self$data[[time]]
            myoutcome <- self$data[[event]]


            thefactor <- self$data[[expl]]


            # Denemeler ----
            # denemeler <- list(
            #     names(thefactor),
            #     typeof(thefactor),
            #     class(thefactor),
            #     length(thefactor),
            #     jmvcore::constructFormula(terms = thefactor),
            #     jmvcore::composeTerms(thefactor),
            #     jmvcore::composeFormula(thefactor)
            # )
            # self$results$text1$setContent(denemeler)



            # Multiexplanatory ----

            # km_fit <- survfit(Surv(OverallTime, Outcome) ~ LVI + PNI, data = deneme)

            # km_fit <- survival::survfit(survival::Surv(myoveralltime, myoutcome) ~ thefactor, data = mydata)


            time1 <- jmvcore::constructFormula(terms = self$options$time)

            time1 <- jmvcore::toNumeric(time1)

            event1 <- jmvcore::constructFormula(terms = self$options$event)

            # expl1 <- as.vector(self$options$expl)

            # expl1 <- jmvcore::constructFormula(terms = self$options$expl)


            # myformula <- jmvcore::composeFormula(lht = paste("survival::Surv(", time1, ",", event1, ")"),
            #                                      rht = list(expl1))

            # myformula <- paste("survival::Surv(", time1, ",", event1, ") ~ ", expl1)

            # myformula <- as.formula(myformula)

            # myformula <- as.formula(expl1)

            # km_fit <- survival::survfit(myformula, data = mydata)


            # km_fit_median_df <- summary(km_fit)
            # results1html <- as.data.frame(km_fit_median_df$table)
            # self$results$text1html$setContent(results1html)


            denemeler <- list(
                head(thefactor)
            )

            self$results$text1$setContent(denemeler)



        })
)
