
# This file is a generated template, your changes will not be overwritten

survivalcontClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "survivalcontClass",
    inherit = survivalcontBase,
    private = list(

        .todo = function() {

            # Initial Message ----

            if ( is.null(self$options$outcome) ||

                 (is.null(self$options$elapsedtime) && !(self$options$tint))

                 || is.null(self$options$contexpl)

            ) {

                todo <- glue::glue("
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you calculate median survivals and 1,3,5-yr survivals for a given fisk factor.
                <br><br>
                Explanatory variable can be categorical (ordinal or nominal), or continuous
                (options under continuous variable collapsebox).
                <br><br>
                Select outcome level from Outcome variable.
                <br><br>
                Outcome Level: if patient is dead or event (recurrence) occured.
                <br><br>
                Survival time should be numeric and continuous.
                <br><br>
                This function uses survival, survminer, and finalfit packages. Please cite jamovi and the packages as given below.
                <br><hr>
                <br>
                See details for survival <a href = 'https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf'>here</a>."
                )

                html <- self$results$todo
                html$setContent(todo)
                return()

            }




        }
        ,

        .errors = function() {
        # Common Errors, Warnings ----

        if (nrow(self$data) == 0)
            stop('Data contains no (complete) rows')

        },

        .cleandata = function() {



            # Common Definitions ----

            contin <- c("integer", "numeric", "double")

            # Read Data ----

            mydata <- self$data

            # Read Arguments ----

            elapsedtime <- self$options$elapsedtime
            outcome <- self$options$outcome
            contexpl <- self$options$contexpl
            outcomeLevel <- self$options$outcomeLevel
            tint <- self$options$tint

            # Define Outcome ----

            multievent <- self$options$multievent

            outcome1 <- self$options$outcome
            outcome1 <- self$data[[outcome1]]


            if (!multievent) {


                if (inherits(outcome1, contin)) {

                    if (
                        !((length(unique(outcome1[!is.na(outcome1)])) == 2) && (sum(unique(outcome1[!is.na(outcome1)])) == 1) )
                    ) {
                        stop('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')

                    }

                    mydata[["myoutcome"]] <- mydata[[self$options$outcome]]


                } else if (inherits(outcome1, "factor")) {


                    # mydata[[self$options$outcome]] <-
                    #     ifelse(test = outcome1 == outcomeLevel,
                    #            yes = 1,
                    #            no = 0)



                    mydata[["myoutcome"]] <-
                        ifelse(test = outcome1 == outcomeLevel,
                               yes = 1,
                               no = 0)



                } else {

                    stop('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0. If you are using a factor as an outcome, please check the levels and content.')

                }

            } else if (multievent) {


                analysistype <- self$options$analysistype

                dod <- self$options$dod
                dooc <- self$options$dooc
                awd <- self$options$awd
                awod <- self$options$awod



                if (analysistype == 'overall') {

                    # (Alive) <=> (Dead of Disease & Dead of Other Causes)


                    mydata[["myoutcome"]] <- NA_integer_

                    mydata[["myoutcome"]][outcome1 == awd] <- 0
                    mydata[["myoutcome"]][outcome1 == awod] <- 0
                    mydata[["myoutcome"]][outcome1 == dod] <- 1
                    mydata[["myoutcome"]][outcome1 == dooc] <- 1



                } else if (analysistype == 'cause') {

                    # (Alive & Dead of Other Causes) <=> (Dead of Disease)


                    mydata[["myoutcome"]] <- NA_integer_

                    mydata[["myoutcome"]][outcome1 == awd] <- 0
                    mydata[["myoutcome"]][outcome1 == awod] <- 0
                    mydata[["myoutcome"]][outcome1 == dod] <- 1
                    mydata[["myoutcome"]][outcome1 == dooc] <- 0

                } else if (analysistype == 'compete') {

                    # Alive <=> Dead of Disease accounting for Dead of Other Causes



                    mydata[["myoutcome"]] <- NA_integer_

                    mydata[["myoutcome"]][outcome1 == awd] <- 0
                    mydata[["myoutcome"]][outcome1 == awod] <- 0
                    mydata[["myoutcome"]][outcome1 == dod] <- 1
                    mydata[["myoutcome"]][outcome1 == dooc] <- 2

                }

            }


            # Define Survival Time ----


            if (!tint) {

            ## Use precalculated time ----

                mydata[[self$options$elapsedtime]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])

                mydata[["mytime"]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])


            } else if (tint) {

            ## Calculate Time Interval ----

                dxdate <- self$options$dxdate
                fudate <- self$options$fudate
                timetypedata <- self$options$timetypedata

                stopifnot(
                    inherits(mydata[[dxdate]], c("POSIXct","POSIXt", "POSIXlt"))
                )

                stopifnot(
                    inherits(mydata[[fudate]], c("POSIXct","POSIXt", "POSIXlt"))
                )


                if (timetypedata == "ymdhms") {
                    mydata[["start"]] <- lubridate::ymd_hms(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::ymd_hms(mydata[[fudate]])
                }
                if (timetypedata == "ymd") {
                    mydata[["start"]] <- lubridate::ymd(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::ymd(mydata[[fudate]])
                }
                if (timetypedata == "ydm") {
                    mydata[["start"]] <- lubridate::ydm(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::ydm(mydata[[fudate]])
                }
                if (timetypedata == "mdy") {
                    mydata[["start"]] <- lubridate::mdy(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::mdy(mydata[[fudate]])
                }
                if (timetypedata == "myd") {
                    mydata[["start"]] <- lubridate::myd(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::myd(mydata[[fudate]])
                }
                if (timetypedata == "dmy") {
                    mydata[["start"]] <- lubridate::dmy(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::dmy(mydata[[fudate]])
                }
                if (timetypedata == "dym") {
                    mydata[["start"]] <- lubridate::dym(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::dym(mydata[[fudate]])
                }


                timetypeoutput <- jmvcore::constructFormula(terms = self$options$timetypeoutput)


                mydata <- mydata %>%
                    dplyr::mutate(
                        interval = lubridate::interval(start, end)
                    )

                stopifnot(lubridate::is.interval(mydata[["interval"]]))

                mydata <- mydata %>%
                    dplyr::mutate(
                        mytime = lubridate::time_length(interval, timetypeoutput)
                    )
            }


            # Define Explanatory Factor ----

            mydata[["myfactor"]] <- mydata[[contexpl]]

            # Define Data For Analysis

            # naOmit ----

            mydata <- jmvcore::naOmit(mydata)


            # Send cleaned mydata to other functions  ----


            return(
                list(
                "mydata" = mydata
            )
            )


        }

        ,
        .run = function() {

            if ( is.null(self$options$outcome) ||

                 (is.null(self$options$elapsedtime) && !(self$options$tint))

                 || is.null(self$options$contexpl)

            ) {
                private$.todo()
            }

                # private$.erors()

            # View mydata ----
            cleaneddata <- private$.cleandata()
            mydata <- cleaneddata$mydata
            self$results$mydataview$setContent(head(mydata, n = 30))

        }

        )
)
