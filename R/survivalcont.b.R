
# This file is a generated template, your changes will not be overwritten

survivalcontClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "survivalcontClass",
    inherit = survivalcontBase,
    private = list(

        .todo = function() {

            # Initial Message ----

            if ( is.null(self$options$outcome) ||

                 (is.null(self$options$elapsedtime) && !(self$options$tint))

                 || (is.null(self$options$explanatory) && is.null(self$options$contexpl))

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




            if (length(self$options$explanatory) > 1) {


                todo <- glue::glue("
                                   <br>More than one explanatory variable.
                                   <br>
                                   <hr>")
                html <- self$results$todo
                html$setContent(todo)

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

                mydata[[self$options$elapsedtime]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])

                mydata[["mytime"]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])


            } else if (tint) {

                # Time Interval ----

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


            # Send mydata ----


            return(
                "mydata" = mydata
            )



            # # Continious Explanatory ----
            #
            #
            # if ( !is.null(self$options$contexpl) ) {
            #
            #
            #     todo <- glue::glue("
            #                                            <br>
            #                                            Continious Explanatory
            #                                            <br>
            #                                            <hr>")
            #     html <- self$results$todo
            #     html$setContent(todo)
            #
            #
            #     # Disable other tables
            #     self$results$medianSummary$setVisible(FALSE)
            #     self$results$medianTable$setVisible(FALSE)
            #     self$results$survTableSummary$setVisible(FALSE)
            #     self$results$survTable$setVisible(FALSE)
            #     self$results$pairwiseSummary$setVisible(FALSE)
            #     self$results$pairwiseTable$setVisible(FALSE)
            #
            #
            #
            #
            #
            #     # Continious Cox Regression ----
            #
            #
            #     formula2 <- as.vector(self$options$contexpl)
            #
            #     sas <- self$options$sas
            #
            #     if (sas) {
            #         formula2 <- 1
            #     }
            #
            #     myformula <- paste("Surv(", "mytime", "," , "myoutcome", ")")
            #
            #     finalfit::finalfit(.data = mydata,
            #                        dependent = myformula,
            #                        explanatory = formula2,
            #
            #                        metrics = TRUE
            #     ) -> tCox
            #
            #
            #     tCoxtext2 <- glue::glue("
            #                     <br>
            #                     <b>Model Metrics:</b>
            #                       ",
            #                     unlist(
            #                         tCox[[2]]
            #                     ),
            #                     "
            #                     <br>
            #                     ")
            #
            #
            #     self$results$tCoxtext2$setContent(tCoxtext2)
            #
            #
            #
            #
            #     tCox_df <- tibble::as_tibble(tCox[[1]], .name_repair = "minimal") %>%
            #         janitor::clean_names(dat = ., case = "snake")
            #
            #
            #     # Continious Cox-Regression Table ----
            #
            #     coxTable <- self$results$coxTable
            #
            #     data_frame <- tCox_df
            #
            #     names(data_frame) <- c(
            #         "Explanatory",
            #         "Levels",
            #         "all",
            #         "HR_univariable",
            #         "HR_multivariable"
            #     )
            #
            #     for(i in seq_along(data_frame[,1,drop=T])) {
            #         coxTable$addRow(rowKey = i, values = c(data_frame[i,]))
            #     }
            #
            #
            #     # Continious coxTable explanation ----
            #
            #
            #     tCox_df <- tibble::as_tibble(tCox[[1]], .name_repair = "minimal") %>%
            #         janitor::clean_names(dat = ., case = "snake")
            #
            #     names(tCox_df) <- names(data_frame) <- c(
            #         "Explanatory",
            #         "Levels",
            #         "all",
            #         "HR_univariable",
            #         "HR_multivariable"
            #     )
            #
            #
            #     # https://stackoverflow.com/questions/38470355/r-fill-empty-cell-with-value-of-last-non-empty-cell
            #
            #     while(length(ind <- which(tCox_df$Explanatory == "")) > 0){
            #         tCox_df$Explanatory[ind] <- tCox_df$Explanatory[ind - 1]
            #     }
            #
            #     # https://stackoverflow.com/questions/51180290/mutate-by-group-in-r
            #
            #     tCox_df %>%
            #         dplyr::group_by(Explanatory) %>%
            #         dplyr::mutate(firstlevel = first(Levels)) %>%
            #         dplyr::mutate(
            #             coxdescription = glue::glue(
            #                 "When {Explanatory} increases 1 unit, the hazard increases {HR_multivariable} times."
            #             )
            #         ) %>%
            #         dplyr::filter(HR_univariable != '-') %>%
            #         dplyr::pull(coxdescription) -> coxSummary
            #
            #
            #
            #     coxSummary <- unlist(coxSummary)
            #     self$results$coxSummary$setContent(coxSummary)
            #
            #
            #
            #
            #     # Continuous Optimal Cut-off ----
            #
            #     # https://rpkgs.datanovia.com/survminer/reference/surv_cutpoint.html
            #
            #     findcut <- self$options$findcut
            #
            #     if (findcut) {
            #
            #         res.cut <- survminer::surv_cutpoint(
            #             mydata,
            #             time = "mytime",
            #             event = "myoutcome",
            #             self$options$contexpl,
            #             minprop = 0.1,
            #             progressbar = TRUE
            #         )
            #
            #         # res.cut$Age
            #         # res.cut$data
            #         # res.cut$minprop
            #         # res.cut$cutpoint
            #
            #
            #         # Cut-off Table ----
            #
            #         rescut_summary <- summary(res.cut)
            #
            #         # self$results$rescutTable$setContent(rescut_summary)
            #
            #
            #         rescutTable <- self$results$rescutTable
            #
            #         rescutTable$setTitle(paste0(self$options$contexpl))
            #
            #
            #         data_frame <- rescut_summary
            #         for (i in seq_along(data_frame[,1,drop = T])) {
            #             rescutTable$addRow(rowKey = i, values = c(data_frame[i,]))
            #         }
            #
            #
            #         # categorisation ----
            #
            #         res.cat <- survminer::surv_categorize(res.cut)
            #
            #
            #         # View mydata ----
            #
            #         # self$results$mydataview$setContent(head(res.cat, 20))
            #
            #         # Prepare Data For Continuous Explanatory Plots ----
            #
            #         plotData4 <- res.cut
            #
            #         image4 <- self$results$plot4
            #         image4$setState(plotData4)
            #
            #         plotData5 <- res.cat
            #
            #         image5 <- self$results$plot5
            #         image5$setState(plotData5)
            #
            #
            #     }
            #
            #
            #
            #
            #     return()
            #
            #
            #
            # }
            #
            #
            #
            #
            #
            #


        }

        # ,
        # .mydata = function(results) {
        #
        #
        #
        #
        # }

        ,
        .run = function() {

            # private$.todo()
            # private$.erors()
            # private$.mydata()

            results <- private$.cleandata()

            mydata <- results$mydata

            self$results$mydataview$setContent(

                head(mydata, n = 30))


        }


        ,
        .plot4 = function(image4, ggtheme, theme, ...) {  # <-- the plot4 function ----


            findcut <- self$options$findcut

            if (!findcut)
                return()

            # if (nrow(self$data) == 0)
            #     stop('Data contains no (complete) rows')

            if ( !is.null(self$options$explanatory) && !is.null(self$options$contexpl)) {

                stop("If you want to use continuous and categorical variables together as explanatory variables, please use Multivariate Survival Analysis function in jsurvival module.")

            }


            # if (is.null(self$options$contexpl) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
            #     return()

            plotData <- image4$state

            res.cut <- plotData

            plot4 <- plot(res.cut, self$options$contexpl, palette = "npg")


            print(plot4)
            TRUE
        }


        ,
        .plot5 = function(image5, ggtheme, theme, ...) {  # <-- the plot5 function ----


            findcut <- self$options$findcut

            if (!findcut)
                return()

            # if (nrow(self$data) == 0)
            #     stop('Data contains no (complete) rows')

            if ( !is.null(self$options$explanatory) && !is.null(self$options$contexpl)) {

                stop("If you want to use continuous and categorical variables together as explanatory variables, please use Multivariate Survival Analysis function in jsurvival module.")

            }


            # if (is.null(self$options$contexpl) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
            #     return()

            plotData <- image5$state

            res.cat <- plotData


            contfactor <- jmvcore::constructFormula(terms = self$options$contexpl)


            sas <- self$options$sas

            if (sas) {
                contfactor <- 1
            }



            # contfactor <- as.formula(contfactor)

            myformula <- paste0("survival::Surv(mytime, myoutcome) ~ ", contfactor)

            myformula <- as.formula(myformula)

            fit <- survminer::surv_fit(formula = myformula,
                                       data = res.cat
            )

            plot5 <- survminer::ggsurvplot(fit,
                                           data = res.cat,
                                           risk.table = self$options$risktable,
                                           conf.int = self$options$ci95)


            print(plot5)
            TRUE
        }


        )
)
