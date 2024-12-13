#' @title Multivariable Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

multisurvivalClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "multisurvivalClass",
        inherit = multisurvivalBase,
        private = list(
            # init ----
            .init = function() {
                explanatory_len <- length(self$options$explanatory)

                contexpl_len <- length(self$options$contexpl)

                self$results$plot8$setSize(explanatory_len * 400, contexpl_len * 300)

            }

            # getData ----
            ,
            .getData = function() {
                # Check if data exists and has content
                if (is.null(self$data) || nrow(self$data) == 0) {
                    stop('Data contains no (complete) rows')
                }

                # Get the data
                mydata <- self$data

                # Check if data has names
                if (is.null(names(mydata))) {
                    stop('Data must have column names')
                }

                # Add row names if missing
                if (is.null(rownames(mydata))) {
                    mydata$row_names <- seq_len(nrow(mydata))
                } else {
                    mydata$row_names <- rownames(mydata)
                }

                # Get original names
                original_names <- names(mydata)

                # Check if original names exist
                if (length(original_names) == 0) {
                    stop('Data must have column names')
                }

                # Create labels vector
                labels <- stats::setNames(original_names, original_names)

                # Clean names safely
                mydata_cleaned <- try({
                    janitor::clean_names(mydata)
                }, silent = TRUE)

                # mydata <- mydata %>% janitor::clean_names()


                if (inherits(mydata_cleaned, "try-error")) {
                    stop('Error cleaning variable names. Please check column names.')
                }


                # Create corrected labels
                corrected_labels <- stats::setNames(original_names, names(mydata_cleaned))

                # Apply labels
                mydata_labelled <- try({
                    labelled::set_variable_labels(.data = mydata_cleaned, .labels = corrected_labels)
                }, silent = TRUE)

                # mydata <- labelled::set_variable_labels(
                #     .data = mydata,
                #     .labels = corrected_labels
                # )


                if (inherits(mydata_labelled, "try-error")) {
                    stop('Error setting variable labels')
                }


                # Get all labels
                all_labels <- labelled::var_label(mydata_labelled)

                # all_labels <- labelled::var_label(mydata)


                # Get variable names from labels
                mytime <- try({
                    names(all_labels)[all_labels == self$options$elapsedtime]
                }, silent = TRUE)

                # mytime <-
                #     names(all_labels)[all_labels == self$options$elapsedtime]

                myoutcome <- try({
                    names(all_labels)[all_labels == self$options$outcome]
                }, silent = TRUE)

                # myoutcome <-
                #     names(all_labels)[all_labels == self$options$outcome]


                mydxdate <- try({
                    names(all_labels)[all_labels == self$options$dxdate]
                }, silent = TRUE)

                # mydxdate <-
                #     names(all_labels)[all_labels == self$options$dxdate]


                myfudate <- try({
                    names(all_labels)[all_labels == self$options$fudate]
                }, silent = TRUE)

                # myfudate <-
                #     names(all_labels)[all_labels == self$options$fudate]



                labels_explanatory <- self$options$explanatory

                myexplanatory <-
                    names(all_labels)[match(labels_explanatory, all_labels)]

                labels_contexpl <- self$options$contexpl

                mycontexpl <-
                    names(all_labels)[match(labels_contexpl, all_labels)]


                # Get adjexplanatory only if it exists and ac option is TRUE
                adjexplanatory <- NULL
                if (!is.null(self$options$adjexplanatory) &&
                    self$options$ac) {
                    adjexplanatory <- names(all_labels)[all_labels == self$options$adjexplanatory]
                }



                # Check if required variables were found
                if (length(mytime) == 0 &&
                    !is.null(self$options$elapsedtime)) {
                    stop('Could not find elapsed time variable')
                }
                if (length(myoutcome) == 0 &&
                    !is.null(self$options$outcome)) {
                    stop('Could not find outcome variable')
                }

                # Return results
                return(
                    list(
                        "mydata_labelled" = mydata_labelled,
                        "mytime_labelled" = mytime,
                        "myoutcome_labelled" = myoutcome,
                        "mydxdate_labelled" = mydxdate,
                        "myfudate_labelled" = myfudate,
                        "mycontexpl_labelled" = mycontexpl,
                        "myexplanatory_labelled" = myexplanatory,
                        "adjexplanatory_labelled" = adjexplanatory

                    )
                )



            }

            # todo ----
            ,
            .todo = function() {
                # todo ----

                todo <- glue::glue(
                    "
                    <br>Welcome to ClinicoPath
                    <br><br>
                        This tool will help you perform a multivariable survival analysis.
                    <br><br>
                        Explanatory variables can be categorical (ordinal or nominal) or continuous.
                    <br><br>
                    Select outcome level from Outcome variable.
                    <br><br>
                    Outcome Level: if patient is dead or event (recurrence) occured. You may also use advanced outcome options depending on your analysis type.
                    <br><br>
                        Survival time should be numeric, continuous, and in months. You may also use dates to calculate survival time in advanced elapsed time options.
                    <br><br>
                        This function uses finalfit, survival, survminer and ggstatsplot packages. Please cite jamovi and the packages as given below.
                    <br><br>
                    "
                )
                # https://finalfit.org/articles/all_tables_examples.html#cox-proportional-hazards-model-survival-time-to-event


                html <- self$results$todo
                html$setContent(todo)
                return()

            }





            # Define Survival Time ----
            ,
            .definemytime = function() {
                ## Read Labelled Data ----

                labelled_data <- private$.getData()

                mydata <- labelled_data$mydata_labelled
                mytime_labelled <- labelled_data$mytime_labelled
                mydxdate_labelled <- labelled_data$mydxdate_labelled
                myfudate_labelled <- labelled_data$myfudate_labelled

                tint <- self$options$tint


                if (!tint) {
                    ### Precalculated Time ----

                    mydata[["mytime"]] <-
                        jmvcore::toNumeric(mydata[[mytime_labelled]])


                } else if (tint) {
                    ### Time Interval ----

                    dxdate <- mydxdate_labelled
                    fudate <- myfudate_labelled
                    timetypedata <- self$options$timetypedata


                    # # Define a mapping from timetypedata to lubridate functions
                    # lubridate_functions <- list(
                    #     ymdhms = lubridate::ymd_hms,
                    #     ymd = lubridate::ymd,
                    #     ydm = lubridate::ydm,
                    #     mdy = lubridate::mdy,
                    #     myd = lubridate::myd,
                    #     dmy = lubridate::dmy,
                    #     dym = lubridate::dym
                    # )
                    # # Apply the appropriate lubridate function based on timetypedata
                    # if (timetypedata %in% names(lubridate_functions)) {
                    #     func <- lubridate_functions[[timetypedata]]
                    #     mydata[["start"]] <- func(mydata[[dxdate]])
                    #     mydata[["end"]] <- func(mydata[[fudate]])
                    # }


                    if (timetypedata == "ymdhms") {
                        mydata[["start"]] <- lubridate::ymd_hms(mydata[[dxdate]])
                        mydata[["end"]] <-
                            lubridate::ymd_hms(mydata[[fudate]])
                    }
                    if (timetypedata == "ymd") {
                        mydata[["start"]] <- lubridate::ymd(mydata[[dxdate]])
                        mydata[["end"]] <-
                            lubridate::ymd(mydata[[fudate]])
                    }
                    if (timetypedata == "ydm") {
                        mydata[["start"]] <- lubridate::ydm(mydata[[dxdate]])
                        mydata[["end"]] <-
                            lubridate::ydm(mydata[[fudate]])
                    }
                    if (timetypedata == "mdy") {
                        mydata[["start"]] <- lubridate::mdy(mydata[[dxdate]])
                        mydata[["end"]] <-
                            lubridate::mdy(mydata[[fudate]])
                    }
                    if (timetypedata == "myd") {
                        mydata[["start"]] <- lubridate::myd(mydata[[dxdate]])
                        mydata[["end"]] <-
                            lubridate::myd(mydata[[fudate]])
                    }
                    if (timetypedata == "dmy") {
                        mydata[["start"]] <- lubridate::dmy(mydata[[dxdate]])
                        mydata[["end"]] <-
                            lubridate::dmy(mydata[[fudate]])
                    }
                    if (timetypedata == "dym") {
                        mydata[["start"]] <- lubridate::dym(mydata[[dxdate]])
                        mydata[["end"]] <-
                            lubridate::dym(mydata[[fudate]])
                    }


                    if (sum(!is.na(mydata[["start"]])) == 0 ||
                        sum(!is.na(mydata[["end"]])) == 0)  {
                        stop(
                            paste0(
                                "Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: ",
                                self$options$timetypedata
                            )
                        )
                    }

                    timetypeoutput <-
                        jmvcore::constructFormula(terms = self$options$timetypeoutput)


                    mydata <- mydata %>%
                        dplyr::mutate(interval = lubridate::interval(start, end))



                    mydata <- mydata %>%
                        dplyr::mutate(mytime = lubridate::time_length(interval, timetypeoutput))

                }


                df_time <- mydata %>% jmvcore::select(c("row_names", "mytime"))

                return(df_time)


            }

            # Define Outcome ----
            ,
            .definemyoutcome = function() {
                labelled_data <- private$.getData()

                mydata <- labelled_data$mydata_labelled
                myoutcome_labelled <- labelled_data$myoutcome_labelled


                contin <- c("integer", "numeric", "double")

                outcomeLevel <- self$options$outcomeLevel
                multievent <- self$options$multievent

                outcome1 <- mydata[[myoutcome_labelled]]

                if (!multievent) {
                    if (inherits(outcome1, contin)) {
                        if (!((length(unique(
                            outcome1[!is.na(outcome1)]
                        )) == 2) && (sum(unique(
                            outcome1[!is.na(outcome1)]
                        )) == 1))) {
                            stop(
                                'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.'
                            )

                        }

                        mydata[["myoutcome"]] <- mydata[[myoutcome_labelled]]
                        # mydata[[self$options$outcome]]

                    } else if (inherits(outcome1, "factor")) {
                        mydata[["myoutcome"]] <-
                            ifelse(
                                test = outcome1 == outcomeLevel,
                                yes = 1,
                                no = 0
                            )

                    } else {
                        stop(
                            'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0. If you are using a factor as an outcome, please check the levels and content.'
                        )

                    }

                } else if (multievent) {
                    analysistype <- self$options$analysistype

                    dod <- self$options$dod
                    dooc <- self$options$dooc
                    awd <- self$options$awd
                    awod <- self$options$awod

                    if (analysistype == 'overall') {
                        # Overall ----
                        # (Alive) <=> (Dead of Disease & Dead of Other Causes)


                        mydata[["myoutcome"]] <- NA_integer_

                        mydata[["myoutcome"]][outcome1 == awd] <- 0
                        mydata[["myoutcome"]][outcome1 == awod] <- 0
                        mydata[["myoutcome"]][outcome1 == dod] <- 1
                        mydata[["myoutcome"]][outcome1 == dooc] <- 1



                    } else if (analysistype == 'cause') {
                        # Cause Specific ----
                        # (Alive & Dead of Other Causes) <=> (Dead of Disease)


                        mydata[["myoutcome"]] <- NA_integer_

                        mydata[["myoutcome"]][outcome1 == awd] <- 0
                        mydata[["myoutcome"]][outcome1 == awod] <- 0
                        mydata[["myoutcome"]][outcome1 == dod] <- 1
                        mydata[["myoutcome"]][outcome1 == dooc] <- 0

                    } else if (analysistype == 'compete') {
                        # Competing Risks ----
                        # Alive <=> Dead of Disease accounting for Dead of Other Causes

                        # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#part_3:_competing_risks


                        mydata[["myoutcome"]] <- NA_integer_

                        mydata[["myoutcome"]][outcome1 == awd] <- 0
                        mydata[["myoutcome"]][outcome1 == awod] <- 0
                        mydata[["myoutcome"]][outcome1 == dod] <- 1
                        mydata[["myoutcome"]][outcome1 == dooc] <- 2

                    }

                }

                df_outcome <- mydata %>% jmvcore::select(c("row_names", "myoutcome"))

                return(df_outcome)

            }


            # Define Factor ----
            ,

            .definemyfactor = function() {
                labelled_data <- private$.getData()

                mydata_labelled <- labelled_data$mydata_labelled
                myexplanatory_labelled <- labelled_data$myexplanatory_labelled
                mycontexpl_labelled <- labelled_data$mycontexpl_labelled

                mydata <- mydata_labelled

                df_factor <- mydata %>%
                    jmvcore::select(c("row_names", myexplanatory_labelled, mycontexpl_labelled))



                return(df_factor)

            }

            # Clean Data ----
            ,
            .cleandata = function() {
                ## Common Definitions ----

                contin <- c("integer", "numeric", "double")

                ## Read Data ----

                labelled_data <- private$.getData()

                mydata_labelled        <- labelled_data$mydata_labelled
                mytime_labelled        <- labelled_data$mytime_labelled
                myoutcome_labelled     <- labelled_data$myoutcome_labelled
                mydxdate_labelled      <- labelled_data$mydxdate_labelled
                myfudate_labelled      <- labelled_data$myfudate_labelled
                myexplanatory_labelled <- labelled_data$myexplanatory_labelled
                mycontexpl_labelled    <- labelled_data$mycontexpl_labelled
                adjexplanatory_labelled <- labelled_data$adjexplanatory_labelled


                time <- private$.definemytime()
                outcome <- private$.definemyoutcome()
                factor <- private$.definemyfactor()

                ## Clean Data ----
                cleanData <- dplyr::left_join(time, outcome, by = "row_names") %>%
                    dplyr::left_join(factor, by = "row_names")

                ## Landmark ----

                # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#landmark_method

                if (self$options$uselandmark) {
                    landmark <- jmvcore::toNumeric(self$options$landmark)

                    cleanData <- cleanData %>%
                        dplyr::filter(mytime >= landmark) %>%
                        dplyr::mutate(mytime = mytime - landmark)
                }

                ## Names cleanData ----

                if (self$options$tint) {
                    name1time <- "CalculatedTime"
                }

                if (!self$options$tint &&
                    !is.null(self$options$elapsedtime)) {
                    name1time <- mytime_labelled
                }

                name2outcome <- myoutcome_labelled

                if (self$options$multievent) {
                    name2outcome <- "CalculatedOutcome"
                }

                name3expl <- NULL

                if (!is.null(self$options$explanatory)) {
                    name3expl <- myexplanatory_labelled
                }


                name3contexpl <- NULL

                if (!is.null(self$options$contexpl)) {
                    name3contexpl <- mycontexpl_labelled
                }

                # Add adjexplanatory name if present
                adjexplanatory_name <- NULL
                if (!is.null(adjexplanatory_labelled)) {
                    adjexplanatory_name <- adjexplanatory_labelled
                }


                # naOmit ----

                cleanData <- jmvcore::naOmit(cleanData)




                ## Add Calculated Time to Data ----

                if (self$options$tint &&
                    self$options$calculatedtime &&
                    self$results$calculatedtime$isNotFilled()) {
                    self$results$calculatedtime$setRowNums(cleanData$row_names)
                    self$results$calculatedtime$setValues(cleanData$mytime)
                }




                ## Add Redefined Outcome to Data ----

                if (self$options$multievent  &&
                    self$options$outcomeredefined &&
                    self$results$outcomeredefined$isNotFilled()) {
                    self$results$outcomeredefined$setRowNums(cleanData$row_names)
                    self$results$outcomeredefined$setValues(cleanData$myoutcome)
                }





                # Return Data ----

                return(
                    list(
                        "name1time" = name1time,
                        "name2outcome" = name2outcome,
                        "name3contexpl" = name3contexpl,
                        "name3expl" = name3expl,
                        "adjexplanatory_name" = adjexplanatory_name,

                        "cleanData" = cleanData,
                        "mytime_labelled" = mytime_labelled,
                        "myoutcome_labelled" = myoutcome_labelled,
                        "mydxdate_labelled" = mydxdate_labelled,
                        "myfudate_labelled" = myfudate_labelled,
                        "myexplanatory_labelled" = myexplanatory_labelled,
                        "mycontexpl_labelled" = mycontexpl_labelled,
                        "adjexplanatory_labelled" = adjexplanatory_labelled

                    )
                )

            }



            # run  ----
            ,
            .run = function() {
                # Errors, Warnings ----

                ## No variable todo ----

                ## Define subconditions ----

                subcondition1a <- !is.null(self$options$outcome)
                subcondition1b1 <- self$options$multievent
                subcondition1b2 <- !is.null(self$options$dod)
                subcondition1b3 <- !is.null(self$options$dooc)
                # subcondition1b4 <- !is.null(self$options$awd)
                # subcondition1b5 <- !is.null(self$options$awod)
                subcondition2a <- !is.null(self$options$elapsedtime)
                subcondition2b1 <- self$options$tint
                subcondition2b2 <- !is.null(self$options$dxdate)
                subcondition2b3 <- !is.null(self$options$fudate)
                condition3a <- !is.null(self$options$contexpl)
                condition3b <- !is.null(self$options$explanatory)

                condition1 <- subcondition1a &&
                    !subcondition1b1 ||
                    subcondition1b1 &&
                    subcondition1b2 || subcondition1b1 && subcondition1b3

                condition2 <- subcondition2b1 &&
                    subcondition2b2 &&
                    subcondition2b3 ||
                    subcondition2a &&
                    !subcondition2b1 && !subcondition2b2 && !subcondition2b3


                condition3 <- condition3a || condition3b

                not_continue_analysis <- !(condition1 &&
                                               condition2 && condition3)


                if (not_continue_analysis) {
                    private$.todo()
                    self$results$text$setVisible(FALSE)
                    self$results$text2$setVisible(FALSE)
                    self$results$plot$setVisible(FALSE)
                    self$results$plot3$setVisible(FALSE)
                    self$results$plot8$setVisible(FALSE)
                    self$results$todo$setVisible(TRUE)
                    return()
                } else {
                    self$results$todo$setVisible(FALSE)
                }


                ## Stop if Empty Data ----

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                ## mydata ----

                cleaneddata <- private$.cleandata()

                name1time <- cleaneddata$name1time
                name2outcome <- cleaneddata$name2outcome
                name3contexpl <- cleaneddata$name3contexpl
                name3expl <- cleaneddata$name3expl
                adjexplanatory_name <- cleaneddata$adjexplanatory_name

                mydata <- cleanData <- cleaneddata$cleanData

                mytime_labelled <- cleaneddata$mytime_labelled
                myoutcome_labelled <- cleaneddata$myoutcome_labelled
                mydxdate_labelled <- cleaneddata$mydxdate_labelled
                myfudate_labelled <- cleaneddata$myfudate_labelled
                myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
                mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
                adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled



                ## run Cox function ----

                private$.final_fit()


                ## generate cox model ----

                if (self$options$ph_cox ||
                    self$options$calculateRiskScore || self$options$ac) {
                    private$.cox_model()
                }

                ## run coxph ----

                if (self$options$ph_cox) {
                    private$.cox_ph()
                }





                # Prepare Data For Plots ----

                image <- self$results$plot
                image$setState(cleaneddata)

                image3 <- self$results$plot3
                image3$setState(cleaneddata)


                # image4 <- self$results$plot4
                # image4$setState(cleaneddata)

                imageKM <- self$results$plotKM
                imageKM$setState(cleaneddata)

                # image7 <- self$results$plot7
                # image7$setState(cleaneddata)

                image_plot_adj <- self$results$plot_adj
                image_plot_adj$setState(cleaneddata)


                # View plot data ----
                self$results$mydataview_plot_adj$setContent(list(head(cleaneddata)))



            }



            # finalfit  ----
            ,
            .final_fit = function() {
                cleaneddata <- private$.cleandata()

                name1time <- cleaneddata$name1time
                name2outcome <- cleaneddata$name2outcome
                name3contexpl <- cleaneddata$name3contexpl
                name3expl <- cleaneddata$name3expl
                adjexplanatory_name <- cleaneddata$adjexplanatory_name

                mydata <- cleanData <- cleaneddata$cleanData

                mytime_labelled <- cleaneddata$mytime_labelled
                myoutcome_labelled <- cleaneddata$myoutcome_labelled
                mydxdate_labelled <- cleaneddata$mydxdate_labelled
                myfudate_labelled <- cleaneddata$myfudate_labelled
                myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
                mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
                adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled


                ## prepare formula ----

                myexplanatory <- NULL

                if (!is.null(self$options$explanatory)) {
                    myexplanatory <- as.vector(myexplanatory_labelled)
                }

                mycontexpl <- NULL
                if (!is.null(self$options$contexpl)) {
                    mycontexpl <- as.vector(mycontexpl_labelled)
                }


                formula2 <- c(myexplanatory, mycontexpl)

                myformula <-
                    paste("Surv( mytime, myoutcome ) ~ ", paste(formula2, collapse = " + "))

                myformula <- as.formula(myformula)

                self$results$mydataview$setContent(
                    list(
                        mydata = head(mydata, n = 30),
                        myformula = myformula,
                        myexplanatory = myexplanatory,
                        mycontexpl = mycontexpl,
                        formula2 = formula2
                    )
                )




                ## finalfit Multivariable table ----

                finalfit::finalfit(.data = mydata,
                                   formula = myformula,
                                   # dependent = myformula,
                                   # explanatory = formula2,

                                   metrics = TRUE) -> tMultivariable


                text2 <- glue::glue("
                                    <br>
                                    <b>Model Metrics:</b>
                                    ",
                                    unlist(tMultivariable[[2]]),
                                    "
                                    <br>
                                    ")

                if (self$options$uselandmark) {
                    landmark <- jmvcore::toNumeric(self$options$landmark)

                    text2 <- glue::glue(
                        text2,
                        "Landmark time used as: ",
                        landmark,
                        " ",
                        self$options$timetypeoutput,
                        "."
                    )
                }

                self$results$text2$setContent(text2)



                results1 <- knitr::kable(
                    tMultivariable[[1]],
                    row.names = FALSE,
                    align = c('l', 'l', 'r', 'r', 'r', 'r'),
                    format = "html"
                )

                self$results$text$setContent(results1)

            }

            # cox model  ----
            ,
            .cox_model = function() {
                cleaneddata <- private$.cleandata()

                name1time <- cleaneddata$name1time
                name2outcome <- cleaneddata$name2outcome
                name3contexpl <- cleaneddata$name3contexpl
                name3expl <- cleaneddata$name3expl
                adjexplanatory_name <- cleaneddata$adjexplanatory_name

                mydata <- cleanData <- cleaneddata$cleanData

                mytime_labelled <- cleaneddata$mytime_labelled
                myoutcome_labelled <- cleaneddata$myoutcome_labelled
                mydxdate_labelled <- cleaneddata$mydxdate_labelled
                myfudate_labelled <- cleaneddata$myfudate_labelled
                myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
                mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
                adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled


                myexplanatory <- NULL
                if (!is.null(self$options$explanatory)) {
                    myexplanatory <- as.vector(myexplanatory_labelled)
                }

                mycontexpl <- NULL
                if (!is.null(self$options$contexpl)) {
                    mycontexpl <- as.vector(mycontexpl_labelled)
                }

                formula2 <- c(myexplanatory, mycontexpl)


                LHT <- "survival::Surv(mytime, myoutcome)"

                RHT <- formula2

                RHT <- paste(RHT, collapse = " + ")

                coxformula <- paste0(LHT, " ~ ", RHT)

                coxformula <- as.formula(coxformula)

                cox_model <- survival::coxph(coxformula, data = mydata)


                return(cox_model)

            }


            # coxph Proportional Hazards Assumption  ----
            ,
            .cox_ph = function() {
                cleaneddata <- private$.cleandata()

                name1time <- cleaneddata$name1time
                name2outcome <- cleaneddata$name2outcome
                name3contexpl <- cleaneddata$name3contexpl
                name3expl <- cleaneddata$name3expl
                adjexplanatory_name <- cleaneddata$adjexplanatory_name

                mydata <- cleanData <- cleaneddata$cleanData

                mytime_labelled <- cleaneddata$mytime_labelled
                myoutcome_labelled <- cleaneddata$myoutcome_labelled
                mydxdate_labelled <- cleaneddata$mydxdate_labelled
                myfudate_labelled <- cleaneddata$myfudate_labelled
                myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
                mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
                adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled


                cox_model <- private$.cox_model()

                zph <- survival::cox.zph(cox_model)

                self$results$cox_ph$setContent(print(zph))

                image8 <- self$results$plot8
                image8$setState(zph)

            }




            # hr_plot ----
            ,
            .plot = function(image, ggtheme, theme, ...) {
                if (!self$options$hr) {
                    return()
                }

                if (!(self$options$sty == "t1")) {
                    return()
                }

                plotData <- image$state

                if (is.null(plotData)) {
                    return()
                }

                name1time <- plotData$name1time
                name2outcome <- plotData$name2outcome
                name3contexpl <- plotData$name3contexpl
                name3expl <- plotData$name3expl

                mydata <- cleanData <- plotData$cleanData

                mytime_labelled <- plotData$mytime_labelled
                myoutcome_labelled <- plotData$myoutcome_labelled
                mydxdate_labelled <- plotData$mydxdate_labelled
                myfudate_labelled <- plotData$myfudate_labelled
                myexplanatory_labelled <- plotData$myexplanatory_labelled
                mycontexpl_labelled <- plotData$mycontexpl_labelled


                ### prepare formula ----

                myexplanatory <- NULL
                if (!is.null(self$options$explanatory)) {
                    myexplanatory <- as.vector(myexplanatory_labelled)
                }

                mycontexpl <- NULL
                if (!is.null(self$options$contexpl)) {
                    mycontexpl <- as.vector(mycontexpl_labelled)
                }

                formula2 <- c(myexplanatory, mycontexpl)

                myformula <-
                    paste0('Surv( mytime, myoutcome )')


                # hr_plot ----
                # https://finalfit.org/reference/hr_plot.html

                plot <-
                    finalfit::hr_plot(
                        .data = mydata,
                        dependent = myformula,
                        explanatory = formula2,
                        dependent_label = "Survival",
                        table_text_size = 4,
                        title_text_size = 14,
                        plot_opts = list(
                            ggplot2::xlab("HR, 95% CI"),
                            ggplot2::theme(axis.title =
                                               ggplot2::element_text(size = 12))
                        )
                    )


                # print plot ----

                print(plot)
                TRUE

            }






            # Forest plot ----
            ,
            .plot3 = function(image3, ggtheme, theme, ...) {
                if (!self$options$hr) {
                    return()
                }

                if (!(self$options$sty == "t3")) {
                    return()
                }

                plotData <- image3$state

                if (is.null(plotData)) {
                    return()
                }

                name1time <- plotData$name1time
                name2outcome <- plotData$name2outcome
                name3contexpl <- plotData$name3contexpl
                name3expl <- plotData$name3expl

                mydata <- cleanData <- plotData$cleanData

                mytime_labelled <- plotData$mytime_labelled
                myoutcome_labelled <- plotData$myoutcome_labelled
                mydxdate_labelled <- plotData$mydxdate_labelled
                myfudate_labelled <- plotData$myfudate_labelled
                myexplanatory_labelled <- plotData$myexplanatory_labelled
                mycontexpl_labelled <- plotData$mycontexpl_labelled


                ### prepare formula ----

                myexplanatory <- NULL
                if (!is.null(self$options$explanatory)) {
                    myexplanatory <- as.vector(myexplanatory_labelled)
                }

                mycontexpl <- NULL
                if (!is.null(self$options$contexpl)) {
                    mycontexpl <- as.vector(mycontexpl_labelled)
                }

                formula2 <- c(myexplanatory, mycontexpl)

                myformula <-
                    paste("survival::Surv(mytime, myoutcome) ~ ",
                          paste(formula2, collapse = " + "))



                myformula <- as.formula(myformula)

                mod <-
                    survival::coxph(formula = myformula, data = mydata)


                # ggforest ----

                plot3 <- survminer::ggforest(model = mod, data = mydata)


                # print plot ----

                print(plot3)
                TRUE

            }


            # cox.zph ----
            ,
            .plot8 = function(image8, ggtheme, theme, ...) {
                ph_cox <- self$options$ph_cox

                if (!ph_cox)
                    return()

                zph <- image8$state

                if (is.null(zph)) {
                    return()
                }

                # plot8 <- plot(zph)

                plot8 <- survminer::ggcoxzph(zph)

                print(plot8)
                TRUE

            }


            # Kaplan-Meier ----
            ,


            .plotKM = function(imageKM, ggtheme, theme, ...) {
                plotData <- imageKM$state

                if (is.null(plotData)) {
                    return()
                }

                name1time <- plotData$name1time
                name2outcome <- plotData$name2outcome
                name3contexpl <- plotData$name3contexpl
                name3expl <- plotData$name3expl

                mydata <- cleanData <- plotData$cleanData

                mytime_labelled <- plotData$mytime_labelled
                myoutcome_labelled <- plotData$myoutcome_labelled
                mydxdate_labelled <- plotData$mydxdate_labelled
                myfudate_labelled <- plotData$myfudate_labelled
                myexplanatory_labelled <- plotData$myexplanatory_labelled
                mycontexpl_labelled <- plotData$mycontexpl_labelled


                ### prepare formula ----

                myexplanatory <- NULL
                if (!is.null(self$options$explanatory)) {
                    myexplanatory <- as.vector(myexplanatory_labelled)
                }


                # myformula <-
                #     paste("survival::Surv(mytime, myoutcome) ~ ",
                #           paste(myexplanatory, collapse = " + "))
                #
                #
                # myformula <- as.formula(myformula)
                #


                thefactor <- jmvcore::constructFormula(terms = myexplanatory)

                if (length(self$options$explanatory) > 2)
                    stop("Kaplan-Meier function allows maximum of 2 explanatory variables")

                if (!is.null(self$options$contexpl))
                    stop("Kaplan-Meier function does not use continuous explanatory variables.")

                title2 <- as.character(thefactor)

                plotKM <- mydata %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = 'survival::Surv(mytime, myoutcome)',
                        explanatory = thefactor,
                        xlab = paste0('Time (', self$options$timetypeoutput, ')'),
                        pval = self$options$pplot,
                        pval.method	= self$options$pplot,
                        legend = 'none',
                        break.time.by = self$options$byplot,
                        xlim = c(0, self$options$endplot),
                        title = paste0("Survival curves for ", title2),
                        subtitle = "Based on Kaplan-Meier estimates",
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95,
                        censored = self$options$censored

                    )

                # plot <- plot + ggtheme

                print(plotKM)
                TRUE



            }












            ,
            # Risk Score Methods ----
            .calculateRiskScore = function(cox_model, mydata) {
                # Calculate risk scores
                risk_scores <- predict(cox_model, type = "risk")

                # Add risk scores to data
                mydata$risk_score <- risk_scores

                # Create risk groups using quantiles
                mydata$risk_group <- cut(
                    mydata$risk_score,
                    breaks = quantile(mydata$risk_score, probs = seq(0, 1, by = 0.25)),
                    labels = c(
                        "Low Risk",
                        "Intermediate-Low Risk",
                        "Intermediate-High Risk",
                        "High Risk"
                    ),
                    include.lowest = TRUE
                )

                # Calculate summary statistics
                risk_summary <- data.frame(
                    group = levels(mydata$risk_group),
                    n_patients = as.numeric(table(mydata$risk_group)),
                    events = tapply(mydata$myoutcome, mydata$risk_group, sum),
                    median_score = tapply(mydata$risk_score, mydata$risk_group, median)
                )

                risk_summary$percent <- (risk_summary$n_patients / sum(risk_summary$n_patients)) * 100

                # Fill risk score table
                riskScoreTable <- self$results$riskScoreTable

                for (i in seq_len(nrow(risk_summary))) {
                    riskScoreTable$addRow(
                        rowKey = i,
                        values = list(
                            group = risk_summary$group[i],
                            n_patients = risk_summary$n_patients[i],
                            percent = risk_summary$percent[i],
                            median_score = risk_summary$median_score[i],
                            events = risk_summary$events[i]
                        )
                    )
                }

                # Create metrics summary
                c_index <- survival::concordance(cox_model)$concordance

                metrics_html <- glue::glue(
                    "
        <br>
        <b>Risk Score Model Performance:</b><br>
        Harrell's C-index: {format(c_index, digits=3)}<br>
        <br>
        Number of patients in risk groups:<br>
        Low Risk: {risk_summary$n_patients[1]} ({format(risk_summary$percent[1], digits=1)}%)<br>
        Intermediate-Low: {risk_summary$n_patients[2]} ({format(risk_summary$percent[2], digits=1)}%)<br>
        Intermediate-High: {risk_summary$n_patients[3]} ({format(risk_summary$percent[3], digits=1)}%)<br>
        High Risk: {risk_summary$n_patients[4]} ({format(risk_summary$percent[4], digits=1)}%)<br>
    "
                )

                self$results$riskScoreMetrics$setContent(metrics_html)

                return(mydata)
            }

            ,
            .plotRiskGroups = function(image, ggtheme, theme, ...) {
                # Check if risk score calculation is enabled
                if (!self$options$calculateRiskScore ||
                    !self$options$plotRiskGroups) {
                    return()
                }

                # Get data from image state
                riskData <- image$state$riskData
                if (is.null(riskData)) {
                    return()
                }

                # Keep only needed columns
                plotData <- data.frame(
                    time = riskData$mytime,
                    status = riskData$myoutcome,
                    group = riskData$risk_group
                )

                # Create survival object and fit
                fit <- survival::survfit(survival::Surv(time, status) ~ group, data = plotData)

                # Create plot
                plot <- survminer::ggsurvplot(
                    fit = fit,
                    data = plotData,
                    pval = TRUE,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    risk.table.height = 0.3,
                    risk.table.y.text.col = TRUE,
                    risk.table.y.text = FALSE,
                    ncensor.plot = TRUE,
                    ncensor.plot.height = 0.25,
                    xlab = paste0("Time (", self$options$timetypeoutput, ")"),
                    ylab = "Survival probability",
                    title = "Survival by Risk Group",
                    subtitle = "Based on Cox model risk score quartiles",
                    legend.title = "Risk Group",
                    palette = "Set2",
                    ggtheme = ggplot2::theme_bw() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = 14, face = "bold"),
                            plot.subtitle = ggplot2::element_text(size = 12),
                            axis.title = ggplot2::element_text(size = 12),
                            axis.text = ggplot2::element_text(size = 10),
                            legend.text = ggplot2::element_text(size = 10)
                        )
                )

                print(plot)
                TRUE
            }




            ,
            # Adjusted survival ----

            .plot_adj = function(image_plot_adj, ggtheme, theme, ...) {
                if (!self$options$ac)
                    return()

                plotData <- image_plot_adj$state

                if (is.null(plotData)) {
                    return()
                }

                name1time <- plotData$name1time
                name2outcome <- plotData$name2outcome
                name3contexpl <- plotData$name3contexpl
                name3expl <- plotData$name3expl
                adjexplanatory_name <- plotData$adjexplanatory_name

                mydata <- cleanData <- plotData$cleanData

                mytime_labelled <- plotData$mytime_labelled
                myoutcome_labelled <- plotData$myoutcome_labelled
                mydxdate_labelled <- plotData$mydxdate_labelled
                myfudate_labelled <- plotData$myfudate_labelled
                myexplanatory_labelled <- plotData$myexplanatory_labelled
                mycontexpl_labelled <- plotData$mycontexpl_labelled
                adjexplanatory_labelled <- plotData$adjexplanatory_labelled


                if (is.null(plotData$adjexplanatory_name)) {
                    stop('Please select a variable for adjusted curves')
                }


                ### prepare formula ----

                myexplanatory <- NULL
                if (!is.null(self$options$explanatory)) {
                    myexplanatory <- as.vector(myexplanatory_labelled)
                }

                mycontexpl <- NULL
                if (!is.null(self$options$contexpl)) {
                    mycontexpl <- as.vector(mycontexpl_labelled)
                }

                formula2 <- c(myexplanatory, mycontexpl)

                myformula <-
                    paste("survival::Surv(mytime, myoutcome) ~ ",
                          paste(formula2, collapse = " + "))

                myformula <- as.formula(myformula)

                # Fit model
                cox_model <- survival::coxph(myformula, data = mydata)

                # Create adjusted curves
                plot <- survminer::ggadjustedcurves(
                    fit = cox_model,
                    data = mydata,
                    variable = adjexplanatory_name,
                    method = "average",
                    conf.int = self$options$ci95,
                    risk.table = self$options$risktable,
                    xlab = paste0('Time (', self$options$timetypeoutput, ')'),
                    title = paste0(
                        "Adjusted Survival Curves for ",
                        self$options$adjexplanatory
                    )
                )

                print(plot)
                TRUE
            }




        )
    )
