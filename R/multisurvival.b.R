#' @title Multivariate Survival Analysis
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

multisurvivalClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "multisurvivalClass",
        inherit = multisurvivalBase,
        private = list(
            .todo = function() {
                # If no variable selected Initial Message ----

                if (is.null(self$options$explanatory) ||
                    is.null(self$options$outcome) ||
                    is.null(self$options$elapsedtime))
                {
                    # TODO ----

                    todo <- glue::glue(
                        "
                    <br>Welcome to ClinicoPath
                    <br><br>
                        This tool will help you perform a multivariate survival analysis.
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

                } else {
                    if (nrow(self$data) == 0)
                        stop('Data contains no (complete) rows')
                }

            }
            ,


            .cleandata = function() {
                # Common Definitions ----

                contin <- c("integer", "numeric", "double")

                # Read Data ----

                mydata <- self$data

                # Read Arguments ----

                elapsedtime <- self$options$elapsedtime
                outcome <- self$options$outcome
                explanatory <- self$options$explanatory
                outcomeLevel <- self$options$outcomeLevel
                tint <- self$options$tint

                # Define Outcome ----

                multievent <- self$options$multievent

                outcome1 <- self$options$outcome
                outcome1 <- self$data[[outcome1]]


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

                        mydata[["myoutcome"]] <-
                            mydata[[self$options$outcome]]


                    } else if (inherits(outcome1, "factor")) {
                        # mydata[[self$options$outcome]] <-
                        #     ifelse(test = outcome1 == outcomeLevel,
                        #            yes = 1,
                        #            no = 0)



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

                    mydata[[self$options$elapsedtime]] <-
                        jmvcore::toNumeric(mydata[[self$options$elapsedtime]])

                    mydata[["mytime"]] <-
                        jmvcore::toNumeric(mydata[[self$options$elapsedtime]])


                } else if (tint) {
                    ## Calculate Time Interval ----

                    dxdate <- self$options$dxdate
                    fudate <- self$options$fudate
                    timetypedata <- self$options$timetypedata

                    stopifnot(inherits(mydata[[dxdate]], c("POSIXct", "POSIXt", "POSIXlt")))

                    stopifnot(inherits(mydata[[fudate]], c("POSIXct", "POSIXt", "POSIXlt")))


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


                    timetypeoutput <-
                        jmvcore::constructFormula(terms = self$options$timetypeoutput)


                    mydata <- mydata %>%
                        dplyr::mutate(interval = lubridate::interval(start, end))

                    stopifnot(lubridate::is.interval(mydata[["interval"]]))

                    mydata <- mydata %>%
                        dplyr::mutate(mytime = lubridate::time_length(interval, timetypeoutput))
                }




                # Define Data For Analysis

                myfactors <- as.vector(self$options$explanatory)

                mydata <- jmvcore::select(df = mydata, columnNames = c("mytime", "myoutcome", myfactors))


                # naOmit ----

                mydata <- jmvcore::naOmit(mydata)


                # Send cleaned mydata to other functions  ----


                return(list("mydata" = mydata))


            }


            ,
            .run = function() {



                # Errors ----
                if ( is.null(self$options$outcome) ||

                     (is.null(self$options$elapsedtime) && !(self$options$tint))

                     || is.null(self$options$explanatory)

                ) {
                    private$.todo()
                    return()
                }

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Calculate mydata ----

                cleaneddata <- private$.cleandata()

                mydata <- cleaneddata$mydata


                # Cox ----

                private$.cox(mydata)


            },



            .cox = function(mydata) {


                # prepare formula ----

                formula2 <- as.vector(self$options$explanatory)

                # formulaL <-
                #     jmvcore::constructFormula(terms = self$options$elapsedtime)
                #
                # formulaL <- jmvcore::toNumeric(formulaL)
                #
                # formulaL <-
                #     jmvcore::constructFormula(terms = self$options$elapsedtime)

                # formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

                # formulaR <- jmvcore::toNumeric(formulaR)


                myformula <-
                    paste("Surv(mytime, myoutcome)")

                # resultsdeneme2 <- list(
                #     formula2,
                #     formulaL,
                #     # formulaR,
                #     myformula
                # )

                # self$results$text3$setContent(resultsdeneme2)

                # finalfit multivariate table ----

                finalfit::finalfit(
                    .data = mydata,
                    dependent = myformula,
                    explanatory = formula2,

                    metrics = TRUE
                ) -> tMultivariate


                text2 <- glue::glue("
                                <br>
                                <b>Model Metrics:</b>
                                  ",
                                unlist(tMultivariate[[2]]),
                                "
                                <br>
                                ")


                self$results$text2$setContent(text2)



                results1 <- knitr::kable(
                    tMultivariate[[1]],
                    row.names = FALSE,
                    align = c('l', 'l', 'r', 'r', 'r', 'r'),
                    format = "html"
                )

                self$results$text$setContent(results1)


                # Reduced model ----
                # If you are using a backwards selection approach or similar, a reduced model can be directly specified and compared. The full model can be kept or dropped.

                # explanatory_multi = c("age", "thickness", "ulcer")
                # melanoma %>%
                #     finalfit(dependent_os, explanatory, explanatory_multi, keep_models = TRUE) %>%
                #     mykable()


                # Testing for proportional hazards ----
                # An assumption of CPH regression is that the hazard (think risk) associated with a particular variable does not change over time. For example, is the magnitude of the increase in risk of death associated with tumour ulceration the same in the early post-operative period as it is in later years?
                #
                #     The cox.zph() function from the survival package allows us to test this assumption for each variable. The plot of scaled Schoenfeld residuals should be a horizontal line. The included hypothesis test identifies whether the gradient differs from zero for each variable. No variable significantly differs from zero at the 5% significance level.

                # explanatory = c("age", "sex", "thickness", "ulcer", "year")
                # melanoma %>%
                #     coxphmulti(dependent_os, explanatory) %>%
                #     cox.zph() %>%
                #     {zph_result <<- .} %>%
                #     plot(var=5)

                # zph_result
                # #>               rho  chisq      p
                # #> age        0.1633 2.4544 0.1172
                # #> sexMale   -0.0781 0.4473 0.5036
                # #> thickness -0.1493 1.3492 0.2454
                # #> ulcerYes  -0.2044 2.8256 0.0928
                # #> year       0.0195 0.0284 0.8663
                # #> GLOBAL         NA 8.4695 0.1322


                # Stratified models ----
                # One approach to dealing with a violation of the proportional hazards assumption is to stratify by that variable. Including a strata() term will result in a separate baseline hazard function being fit for each level in the stratification variable. It will be no longer possible to make direct inference on the effect associated with that variable.
                #
                # This can be incorporated directly into the explanatory variable list.

                # explanatory= c("age", "sex", "ulcer", "thickness", "strata(year)")
                # melanoma %>%
                #     finalfit(dependent_os, explanatory) %>%
                #     mykable()




            }



            ,
            .plot = function(image, ggtheme, theme, ...) {
                # <-- the plot function ----

                # plotData <- image$state

                if (is.null(self$options$explanatory) ||
                    (length(self$options$outcome) + length(self$options$elapsedtime) < 2))
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')



                # prepare data ----

                mydata <- self$data

                outcomeLevel <- self$options$outcomeLevel



                contin <- c("integer", "numeric", "double")


                outcome1 <- self$options$outcome

                outcome1 <- self$data[[outcome1]]

                if (inherits(outcome1, contin)) {
                    if (!any(outcome1 != 0, na.rm = TRUE) ||
                        !any(outcome1 != 1, na.rm = TRUE)) {
                        stop(
                            'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.'
                        )

                    }

                    mydata[["Outcome"]] <-
                        mydata[[self$options$outcome]]

                } else if (inherits(outcome1, "factor")) {
                    outcomeLevel <- self$options$outcomeLevel

                    mydata[["Outcome"]] <-
                        ifelse(test = mydata[[self$options$outcome]] == outcomeLevel,
                               yes = 1,
                               no = 0)



                }

                # prepare formula ----

                formula2 <-
                    jmvcore::constructFormula(terms = self$options$explanatory)

                # formula2 <- as.vector(self$options$explanatory)

                formulaL <-
                    jmvcore::constructFormula(terms = self$options$elapsedtime)

                formulaL <- jmvcore::toNumeric(formulaL)

                formulaL <-
                    jmvcore::constructFormula(terms = self$options$elapsedtime)

                myformula <-
                    paste("survival::Surv(", formulaL, ",", "Outcome", ")")


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





            # ,
            # .plot2 = function(image, ggtheme, theme, ...) {  # <-- the plot function
            #
            # # plotData <- image$state
            #
            #     if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
            #         return()
            #
            # if (nrow(self$data) == 0)
            #     stop('Data contains no (complete) rows')
            #
            # # Check if outcome variable is suitable or stop
            # myoutcome2 <- self$options$outcome
            # myoutcome2 <- self$data[[myoutcome2]]
            # myoutcome2 <- na.omit(myoutcome2)
            #
            # if (class(myoutcome2) == "factor")
            #     stop("Please use a continuous variable for outcome.")
            #
            # if (any(myoutcome2 != 0 & myoutcome2 != 1))
            #     stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')
            #
            # # https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html#cox-proportional-hazards-regression-model-coxph
            # # fit a stratified model
            #
            # mydata <- self$data
            #
            # formulaL <- jmvcore::constructFormula(terms = self$options$elapsedtime)
            #
            # formulaL <- jmvcore::toNumeric(formulaL)
            #
            # formulaR <- jmvcore::constructFormula(terms = self$options$outcome)
            #
            # formulaR <- jmvcore::toNumeric(formulaR)
            #
            # formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)
            #
            # formula3 <- paste("survival::Surv(", formulaL, ",", formulaR, ") ~ ", formula2)
            #
            # formula3 <- as.formula(formula3)
            #
            # mod <-
            #     survival::coxph(
            #         formula = formula3,
            #         data = mydata
            #     )
            #
            # # plot
            # plot2 <- ggstatsplot::ggcoefstats(
            #     x = mod,
            #     exponentiate = TRUE,
            #     title = "Cox proportional hazards regression model"
            # )
            #
            # print(plot2)
            # TRUE
            #
            # }




            ,
            .plot3 = function(image, ggtheme, theme, ...) {
                # <-- the plot function ----

                # plotData <- image$state

                if (is.null(self$options$explanatory) ||
                    is.null(self$options$outcome) ||
                    is.null(self$options$elapsedtime))
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')




                # prepare data ----

                mydata <- self$data

                outcomeLevel <- self$options$outcomeLevel


                contin <- c("integer", "numeric", "double")


                outcome1 <- self$options$outcome

                outcome1 <- self$data[[outcome1]]

                if (inherits(outcome1, contin)) {
                    if (!any(outcome1 != 0, na.rm = TRUE) ||
                        !any(outcome1 != 1, na.rm = TRUE)) {
                        stop(
                            'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.'
                        )

                    }

                    mydata[["Outcome"]] <-
                        mydata[[self$options$outcome]]

                } else if (inherits(outcome1, "factor")) {
                    outcomeLevel <- self$options$outcomeLevel

                    mydata[["Outcome"]] <-
                        ifelse(test = mydata[[self$options$outcome]] == outcomeLevel,
                               yes = 1,
                               no = 0)



                }



                # prepare formula ----

                formula2 <-
                    jmvcore::constructFormula(terms = self$options$explanatory)

                # formula2 <- as.vector(self$options$explanatory)

                formulaL <-
                    jmvcore::constructFormula(terms = self$options$elapsedtime)

                formulaL <- jmvcore::toNumeric(formulaL)

                formulaL <-
                    jmvcore::constructFormula(terms = self$options$elapsedtime)

                formula2 <-
                    jmvcore::constructFormula(terms = self$options$explanatory)

                formula3 <-
                    paste("survival::Surv(", formulaL, ",", "Outcome", ") ~ ", formula2)

                formula3 <- as.formula(formula3)

                mod <-
                    survival::coxph(formula = formula3,
                                    data = mydata)

                # plot

                # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf


                # The function ggforest() from the survminer package creates a forest plot for a Cox regression model fit. Hazard ratio estimates along with confiden- ce intervals and p-values are plotter for each variable.

                # lung$age <- ifelse(lung$age > 70, ">70","<= 70")
                # fit <- coxph( Surv(time, status) ~ sex + ph.ecog + age, data = lung)
                # ggforest(fit)


                # ggforest ----
                plot3 <- survminer::ggforest(model = mod,
                                             data = mydata)


                # print plot ----

                print(plot3)
                TRUE

            }


            ,
            .plot4 = function(image, ggtheme, theme, ...) {
                # <-- the plot function ----

                # plotData <- image$state

                if (is.null(self$options$explanatory) ||
                    is.null(self$options$outcome) ||
                    is.null(self$options$elapsedtime) ||
                    is.null(self$options$adjexplanatory))
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


                # prepare data ----

                mydata <- self$data

                outcomeLevel <- self$options$outcomeLevel



                contin <- c("integer", "numeric", "double")


                outcome1 <- self$options$outcome

                outcome1 <- self$data[[outcome1]]

                if (inherits(outcome1, contin)) {
                    if (!any(outcome1 != 0, na.rm = TRUE) ||
                        !any(outcome1 != 1, na.rm = TRUE)) {
                        stop(
                            'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.'
                        )

                    }

                    mydata[["Outcome"]] <- mydata[[self$options$outcome]]

                } else if (inherits(outcome1, "factor")) {
                    outcomeLevel <- self$options$outcomeLevel

                    mydata[["Outcome"]] <-
                        ifelse(test = mydata[[self$options$outcome]] == outcomeLevel,
                               yes = 1,
                               no = 0)



                }


                # prepare formula ----

                formula2 <-
                    jmvcore::constructFormula(terms = self$options$explanatory)

                # formula2 <- as.vector(self$options$explanatory)

                formulaL <-
                    jmvcore::constructFormula(terms = self$options$elapsedtime)

                formulaL <- jmvcore::toNumeric(formulaL)

                formulaL <-
                    jmvcore::constructFormula(terms = self$options$elapsedtime)

                formula2 <-
                    jmvcore::constructFormula(terms = self$options$explanatory)

                formula3 <-
                    paste("survival::Surv(", formulaL, ",", "Outcome", ") ~ ", formula2)

                formula3 <- as.formula(formula3)

                # get fitted model ----

                mod <-
                    survival::coxph(formula = formula3,
                                    data = mydata)


                # plot

                # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf


                # The function ggadjustedcurves() from the survminer package plots Adjusted Survival Curves for Cox Proportional Hazards Model. Adjusted Survival Curves show how a selected factor influences survival estimated from a Cox model.
                # Note that these curves differ from Kaplan Meier estimates since they present expected ssurvival based on given Cox model.

                # lung$sex <- ifelse(lung$sex == 1, "Male", "Female")

                # fit <- coxph(Surv(time, status) ~ sex + ph.ecog + age +
                #                  strata(sex), data = lung)
                # ggcoxadjustedcurves(fit, data=lung)


                # Note that it is not necessary to include the grouping factor in the Cox model. Survival curves are estimated from Cox model for each group defined by the factor independently.

                # lung$age3 <- cut(lung$age,
                #                  c(35,55,65,85))


                # ggcoxadjustedcurves(fit, data=lung,
                #                     variable=”lage3”)


                # select adjexplanatory ----

                adjexplanatory <- self$options$adjexplanatory

                adjexplanatory <-
                    jmvcore::composeTerm(components = adjexplanatory)


                # ggadjustedcurves ----

                plot4 <- survminer::ggadjustedcurves(fit = mod,
                                                     data = mydata,
                                                     variable = adjexplanatory
                                                     # method = ,
                                                     # fun =

                                                     )


                                                     # print plot -----

                                                     print(plot4)
                                                     TRUE

                                                     }




            )
        )
