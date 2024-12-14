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

        if (explanatory_len > 0 || contexpl_len > 0) {
          self$results$plot8$setSize((explanatory_len + contexpl_len) * 400,
                                     (explanatory_len + contexpl_len) * 300)
        } else {
          self$results$plot8$setVisible(FALSE)
        }




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


        mystratvar_labelled <- NULL
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
        # Add this to get stratification variables
        labels_stratvar <- self$options$stratvar
        mystratvar_labelled <- names(all_labels)[match(labels_stratvar, all_labels)]
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
            "adjexplanatory_labelled" = adjexplanatory,
            "mystratvar_labelled" = mystratvar_labelled


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


        Stratification Variables: Use these when the proportional hazards assumption
        is violated for certain variables. The model will create separate baseline
        hazard functions for each level of the stratification variables, but won't
        estimate their direct effects.
        <br><br>
        Consider using stratification when:
        <br>- A variable fails the proportional hazards test
        <br>- You need to control for a variable's effect but don't need to
        estimate its hazard ratio
        <br>- There are natural differences in baseline risk across groups

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
            if (!((length(unique(outcome1[!is.na(outcome1)])) == 2) &&
                  (sum(unique(outcome1[!is.na(outcome1)])) == 1))) {
              stop(
                'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.'
              )

            }

            mydata[["myoutcome"]] <- mydata[[myoutcome_labelled]]
            # mydata[[self$options$outcome]]

          } else if (inherits(outcome1, "factor")) {
            mydata[["myoutcome"]] <-
              ifelse(test = outcome1 == outcomeLevel,
                     yes = 1,
                     no = 0)

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
        adjexplanatory_labelled <- labelled_data$adjexplanatory_labelled

        mydata <- mydata_labelled

        df_factor <- mydata %>%
          jmvcore::select(unique(
            c(
              "row_names",
              myexplanatory_labelled,
              adjexplanatory_labelled,
              mycontexpl_labelled
            )
          ))

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


        # self$results$mydataview$setContent(
        #   list(
        #     "name1time" = name1time,
        #     "name2outcome" = name2outcome,
        #     "name3contexpl" = name3contexpl,
        #     "name3expl" = name3expl,
        #     "adjexplanatory_name" = adjexplanatory_name,
        #
        #     "cleanData" = cleanData,
        #     "mytime_labelled" = mytime_labelled,
        #     "myoutcome_labelled" = myoutcome_labelled,
        #     "mydxdate_labelled" = mydxdate_labelled,
        #     "myfudate_labelled" = myfudate_labelled,
        #     "myexplanatory_labelled" = myexplanatory_labelled,
        #     "mycontexpl_labelled" = mycontexpl_labelled,
        #     "adjexplanatory_labelled" = adjexplanatory_labelled
        #
        #   )
        # )



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
          subcondition1b2 ||
          subcondition1b1 && subcondition1b3

        condition2 <- subcondition2b1 &&
          subcondition2b2 &&
          subcondition2b3 ||
          subcondition2a &&
          !subcondition2b1 &&
          !subcondition2b2 && !subcondition2b3


        condition3 <- condition3a || condition3b

        not_continue_analysis <- !(condition1 &&
                                     condition2 &&
                                     condition3)


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
            self$options$calculateRiskScore ||
            self$options$ac) {
          cox_model <- private$.cox_model()
        }

        ## run coxph ----

        if (self$options$ph_cox) {
          private$.cox_ph(cox_model)
        }


        ## Calculate Risk Score ----

        if (self$options$calculateRiskScore) {
          riskData <- private$.calculateRiskScore(cox_model, mydata)

        }


        ## Compare models ----

        if (self$options$compare_models) {
          private$.compare_models()
        }


        ## Adjusted survival ----

        if (self$options$ac) {
          private$.runAdjustedSurvival()
        }



        ## run Cox function .fitModelWithSelection ----

        if (self$options$use_modelSelection) {
          private$.final_fit2()
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


        if (self$options$calculateRiskScore) {
          image_riskGroupPlot <- self$results$riskGroupPlot
          image_riskGroupPlot$setState(riskData)

        }

        # View plot data ----
        # self$results$mydataview_plot_adj$setContent(list(head(cleaneddata)))



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

        # self$results$mydataview$setContent(
        #     list(
        #         mydata = head(mydata, n = 30),
        #         myformula = myformula,
        #         myexplanatory = myexplanatory,
        #         mycontexpl = mycontexpl,
        #         formula2 = formula2
        #     )
        # )




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

          text2 <- glue::glue(text2,
                              "Landmark time used as: ",
                              landmark,
                              " ",
                              self$options$timetypeoutput,
                              ".")
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



        # Add stratification variables
        mystratvar <- NULL
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          mystratvar <- as.vector(cleaneddata$mystratvar_labelled)
          # Create strata terms
          mystratvar <- paste0("strata(", mystratvar, ")")
        }



        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(mycontexpl_labelled)
        }


        formula2 <- c(myexplanatory, mycontexpl, mystratvar)



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
      .cox_ph = function(cox_model) {
        # cleaneddata <- private$.cleandata()
        #
        # name1time <- cleaneddata$name1time
        # name2outcome <- cleaneddata$name2outcome
        # name3contexpl <- cleaneddata$name3contexpl
        # name3expl <- cleaneddata$name3expl
        # adjexplanatory_name <- cleaneddata$adjexplanatory_name
        #
        # mydata <- cleanData <- cleaneddata$cleanData
        #
        # mytime_labelled <- cleaneddata$mytime_labelled
        # myoutcome_labelled <- cleaneddata$myoutcome_labelled
        # mydxdate_labelled <- cleaneddata$mydxdate_labelled
        # myfudate_labelled <- cleaneddata$myfudate_labelled
        # myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
        # mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
        # adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled
        #
        #
        # cox_model <- private$.cox_model()

        zph <- survival::cox.zph(cox_model)




        # Add suggestions for stratification
        significant_violations <- which(zph$table[,"p"] < 0.05)
        if (length(significant_violations) > 0) {
          violation_vars <- rownames(zph$table)[significant_violations]
          suggestion <- glue::glue(
            "<br><br>Note: The proportional hazards assumption appears to be
            violated for: {paste(violation_vars, collapse=', ')}.
            Consider using these as stratification variables instead of
            covariates."
          )

          self$results$cox_ph$setContent(
            paste(print(zph), suggestion)
          )
        }



        # Display test results
        self$results$cox_ph$setContent(print(zph))






        # Only create plots if there are variables to plot
        if (!is.null(zph$y)) {
          # Pass zph object to plot function
          image8 <- self$results$plot8
          image8$setState(zph)
        } else {
          # If no variables to plot, hide the plot
          self$results$plot8$setVisible(FALSE)
        }

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


      # cox.zph plot8 ----
      ,
      .plot8 = function(image8, ggtheme, theme, ...) {
        if (!self$options$ph_cox)
          return()

        zph <- image8$state

        if (is.null(zph)) {
          return()
        }

        # Check if there are variables to plot
        if (is.null(zph$y)) {
          return()
        }

        # Create plot using survminer
        plot8 <- survminer::ggcoxzph(zph)

        print(plot8)
        TRUE

      }


      # Kaplan-Meier ----
      ,


      .plotKM = function(imageKM, ggtheme, theme, ...) {
        # Check conditions and show message if not met
        if (length(self$options$explanatory) > 2) {
          text_warning <- "Kaplan-Meier plot requires 2 categorical explanatory variables. You have selected more than 2 variables."
          grid::grid.newpage()
          grid::grid.text(text_warning, 0.5, 0.5)
          return(TRUE)
        }

        if (!is.null(self$options$contexpl)) {
          text_warning <- "Kaplan-Meier plot cannot be created with continuous explanatory variables. Please select only categorical variables."
          grid::grid.newpage()
          grid::grid.text(text_warning, 0.5, 0.5)
          return(TRUE)
        }

        if (length(self$options$explanatory) < 2) {
          text_warning <- "Please select 2 categorical explanatory variables to create the Kaplan-Meier plot."
          grid::grid.newpage()
          grid::grid.text(text_warning, 0.5, 0.5)
          return(TRUE)
        }


        # if (length(self$options$explanatory) > 2)
        #     stop("Kaplan-Meier function allows maximum of 2 explanatory variables")
        #
        # if (!is.null(self$options$contexpl))
        #     stop("Kaplan-Meier function does not use continuous explanatory variables.")





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

      ## Calculate Risk Score ----

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


        ### Add risk scores to output if requested ----
        if (self$options$addRiskScore &&
            self$results$addRiskScore$isNotFilled()) {
          self$results$addRiskScore$setRowNums(mydata$row_names)
          self$results$addRiskScore$setValues(mydata$risk_score)
        }


        ### Add risk group to output if requested ----
        if (self$options$addRiskGroup &&
            self$results$addRiskGroup$isNotFilled()) {
          self$results$addRiskGroup$setRowNums(mydata$row_names)
          self$results$addRiskGroup$setValues(mydata$risk_group)
        }






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

      ## Plot Risk Groups ----
      ,
      .plotRiskGroups = function(image_riskGroupPlot, ggtheme, theme, ...) {
        # Check if risk score calculation is enabled
        if (!self$options$calculateRiskScore ||
            !self$options$plotRiskGroups) {
          return()
        }

        # Get data from image state
        riskData <- image_riskGroupPlot$state
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
      # Compare Models ----
      .compare_models = function() {
        # Get clean data
        cleaneddata <- private$.cleandata()
        mydata <- cleaneddata$cleanData

        # Get full model variables
        full_explanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          full_explanatory <- as.vector(cleaneddata$myexplanatory_labelled)
        }

        full_contexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          full_contexpl <- as.vector(cleaneddata$mycontexpl_labelled)
        }

        # Get reduced model variables
        reduced_explanatory <- NULL
        if (!is.null(self$options$reduced_explanatory)) {
          reduced_explanatory <- names(labelled::var_label(mydata))[match(self$options$reduced_explanatory,
                                                                          labelled::var_label(mydata))]
        }

        # Create formulas
        full_formula <- c(full_explanatory, full_contexpl)

        # Run finalfit with model comparison
        comparison <- finalfit::finalfit(
          .data = mydata,
          dependent = 'survival::Surv(mytime, myoutcome)',
          explanatory = full_formula,
          explanatory_multi = reduced_explanatory,
          keep_models = TRUE
        )

        # Create comparison table
        html_comparison <- knitr::kable(comparison[[1]], format = 'html', caption = "Full vs Reduced Model Comparison")

        # Add metrics
        metrics_html <- glue::glue(
          "
        <br>
        <b>Model Comparison Metrics:</b><br>
        Full model AIC: {comparison[[2]]$AIC.full}<br>
        Reduced model AIC: {comparison[[2]]$AIC.reduced}<br>
        Likelihood ratio test p-value: {comparison[[2]]$lrtest.pvalue}
    "
        )

        # Set results
        self$results$model_comparison$setContent(html_comparison)
        self$results$reduced_model_metrics$setContent(metrics_html)
      }

      ,
      # Helper function to validate timepoints input ----

      .validateTimepoints = function(timepoints_str) {
        if (is.null(timepoints_str) || nchar(timepoints_str) == 0) {
          return(NULL)
        }

        tryCatch({
          # Split and clean the timepoints string
          pts <- trimws(unlist(strsplit(timepoints_str, ",")))
          pts <- as.numeric(pts)
          pts <- sort(unique(pts[!is.na(pts)]))

          if (length(pts) == 0) {
            stop("No valid timepoints found")
          }
          if (any(pts < 0)) {
            stop("Timepoints must be positive")
          }

          return(pts)
        }, error = function(e) {
          stop(paste("Invalid timepoints format:", e$message))
        })
      }

      ,
      # Calculate adjusted survival statistics ----

      .calculateAdjustedStats = function(data,
                                         cox_model,
                                         adj_var,
                                         timepoints,
                                         method = "average") {
        # Input validation
        if (!adj_var %in% names(data)) {
          stop("Adjustment variable not found in data")
        }

        # Get unique levels of adjustment variable
        levels <- sort(unique(data[[adj_var]]))
        if (length(levels) < 2) {
          stop("Adjustment variable must have at least 2 levels")
        }

        # Initialize results list
        results <- list()

        # Calculate adjusted survival for each level
        for (level in levels) {
          # Create modified dataset with current level
          newdata <- data
          newdata[[adj_var]] <- level

          # Calculate predicted survival
          pred_surv <- tryCatch({
            survival::survfit(cox_model, newdata = newdata)
          }, error = function(e) {
            warning(paste(
              "Error calculating survival for level:",
              level,
              "-",
              e$message
            ))
            return(NULL)
          })

          if (is.null(pred_surv))
            next

          # Extract survival estimates
          level_stats <- data.frame(
            time = pred_surv$time,
            survival = pred_surv$surv,
            std.err = pred_surv$std.err,
            lower = pred_surv$lower,
            upper = pred_surv$upper,
            n.risk = pred_surv$n.risk
          )

          # Calculate statistics at specified timepoints
          timepoint_stats <- lapply(timepoints, function(t) {
            idx <- which.min(abs(level_stats$time - t))
            if (length(idx) == 0 ||
                t > max(level_stats$time)) {
              return(NULL)
            }

            list(
              timepoint = t,
              survival = level_stats$survival[idx],
              se = level_stats$std.err[idx],
              ci_lower = level_stats$lower[idx],
              ci_upper = level_stats$upper[idx],
              n_risk = level_stats$n.risk[idx]
            )
          })

          # Store results
          timepoint_stats <- Filter(Negate(is.null), timepoint_stats)
          if (length(timepoint_stats) > 0) {
            results[[as.character(level)]] <- list(full_curve = level_stats, timepoints = timepoint_stats)
          }
        }

        # Add metadata
        attr(results, "timepoints") <- timepoints
        attr(results, "levels") <- levels
        attr(results, "variable") <- adj_var
        attr(results, "method") <- method

        return(results)
      }

      ,
      # Generate summary table from adjusted statistics ----

      .generateAdjustedSummary = function(adj_stats) {
        # Input validation
        if (is.null(adj_stats) || length(adj_stats) == 0) {
          warning("No adjusted statistics provided")
          return(data.frame())
        }

        # Initialize summary dataframe
        summary_df <- data.frame(
          Level = character(),
          Timepoint = numeric(),
          Survival = numeric(),
          SE = numeric(),
          CI_Lower = numeric(),
          CI_Upper = numeric(),
          N_at_Risk = integer(),
          stringsAsFactors = FALSE
        )

        # Process each level's statistics
        for (level in names(adj_stats)) {
          level_data <- adj_stats[[level]]

          if (is.null(level_data$timepoints)) {
            warning(sprintf("No timepoint data available for level: %s", level))
            next
          }

          # Extract timepoint statistics
          for (tp_stat in level_data$timepoints) {
            # Validate numeric values before rounding
            surv_val <- if (is.numeric(tp_stat$survival))
              round(tp_stat$survival, 3)
            else
              NA
            se_val <- if (is.numeric(tp_stat$se))
              round(tp_stat$se, 3)
            else
              NA
            ci_lower_val <- if (is.numeric(tp_stat$ci_lower))
              round(tp_stat$ci_lower, 3)
            else
              NA
            ci_upper_val <- if (is.numeric(tp_stat$ci_upper))
              round(tp_stat$ci_upper, 3)
            else
              NA
            n_risk_val <- if (is.numeric(tp_stat$n_risk))
              as.integer(tp_stat$n_risk)
            else
              NA

            # Create row with validated values
            row <- data.frame(
              Level = as.character(level),
              Timepoint = as.numeric(tp_stat$timepoint),
              Survival = surv_val,
              SE = se_val,
              CI_Lower = ci_lower_val,
              CI_Upper = ci_upper_val,
              N_at_Risk = n_risk_val,
              stringsAsFactors = FALSE
            )

            # Only add row if essential values are present
            if (!is.na(row$Timepoint) &&
                !is.na(row$Survival)) {
              summary_df <- rbind(summary_df, row)
            }
          }
        }

        # Sort and add metadata if we have results
        if (nrow(summary_df) > 0) {
          summary_df <- summary_df[order(summary_df$Level, summary_df$Timepoint), ]

          # Calculate median survival times
          median_stats <- lapply(adj_stats, function(x) {
            if (is.null(x$full_curve))
              return(NA)

            full_curve <- x$full_curve
            if (!is.numeric(full_curve$survival))
              return(NA)

            median_idx <- which.min(abs(full_curve$survival - 0.5))
            if (length(median_idx) > 0) {
              return(full_curve$time[median_idx])
            }
            return(NA)
          })

          attr(summary_df, "median_survival") <- median_stats
          attr(summary_df, "analysis_info") <- list(
            variable = attr(adj_stats, "variable"),
            method = attr(adj_stats, "method"),
            timepoints = attr(adj_stats, "timepoints"),
            n_levels = length(unique(summary_df$Level))
          )
        }

        return(summary_df)
      }

      ,
      # Main adjusted survival analysis function ----
      .runAdjustedSurvival = function() {
        if (!self$options$ac)
          return(NULL)

        # Validate inputs
        timepoints <- private$.validateTimepoints(self$options$ac_timepoints)
        if (is.null(timepoints)) {
          stop("No valid timepoints specified for adjusted survival analysis")
        }

        # Get cleaned data and model
        cleaneddata <- private$.cleandata()
        cox_model <- private$.cox_model()

        # Calculate adjusted statistics
        adj_stats <- tryCatch({
          private$.calculateAdjustedStats(
            data = cleaneddata$cleanData,
            cox_model = cox_model,
            adj_var = cleaneddata$adjexplanatory_name,
            timepoints = timepoints,
            method = self$options$ac_method
          )
        }, error = function(e) {
          stop(paste("Error in adjusted survival analysis:", e$message))
        })

        # Generate and display summary if requested
        if (self$options$ac_summary &&
            !is.null(adj_stats)) {
          summary_table <- private$.generateAdjustedSummary(adj_stats)

          if (nrow(summary_table) > 0) {
            # Add rows to results table
            for (i in seq_len(nrow(summary_table))) {
              self$results$adjustedSummaryTable$addRow(
                rowKey = i,
                values = list(
                  Level = summary_table$Level[i],
                  Timepoint = summary_table$Timepoint[i],
                  Survival = summary_table$Survival[i],
                  SE = summary_table$SE[i],
                  CI_Lower = summary_table$CI_Lower[i],
                  CI_Upper = summary_table$CI_Upper[i],
                  N_at_Risk = summary_table$N_at_Risk[i]
                )
              )
            }
          }
        }

        return(adj_stats)
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
          title = paste0("Adjusted Survival Curves for ", self$options$adjexplanatory)
        )

        print(plot)
        TRUE
      }


      ,
      # fitModelWithSelection ----

      .fitModelWithSelection = function(formula, data) {
        modelSelection <- self$options$modelSelection
        selectionCriteria <- self$options$selectionCriteria
        pEntry <- self$options$pEntry
        pRemoval <- self$options$pRemoval


        if (self$options$pEntry >= self$options$pRemoval) {
          stop("Entry significance must be less than removal significance")
        }

        if (self$options$modelSelection != "enter" &&
            length(c(self$options$explanatory, self$options$contexpl)) < 2) {
          stop("Variable selection requires at least 2 predictor variables")
        }


        # Create full and null models
        full_model <- survival::coxph(formula, data = data)
        null_model <- survival::coxph(update(formula, . ~ 1), data = data)

        # If no selection requested, return full model
        if (modelSelection == "enter") {
          return(full_model)
        }

        # Set up step parameters
        step_params <- list(
          scope = list(
            lower = formula(null_model),
            upper = formula(full_model)
          ),
          direction = modelSelection,
          k = if (selectionCriteria == "aic")
            2
          else
            0,
          # k=2 for AIC
          test = if (selectionCriteria == "lrt")
            "LRT"
          else
            "Chisq"
        )

        # Add custom test function for likelihood ratio
        if (selectionCriteria == "lrt") {
          step_params$test.statistic <- "LRT"
          step_params$alpha.to.enter <- pEntry
          step_params$alpha.to.remove <- pRemoval
        }

        # Perform stepwise selection
        if (modelSelection == "forward") {
          final_model <- do.call("step", c(list(object = null_model), step_params))
        } else if (modelSelection == "backward") {
          final_model <- do.call("step", c(list(object = full_model), step_params))
        } else {
          # both
          final_model <- do.call("step", c(list(object = null_model), step_params))
        }

        return(final_model)
      }


      # finalfit 2 ----
      ,
      .final_fit2 = function() {
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

        # self$results$mydataview$setContent(
        #     list(
        #         mydata = head(mydata, n = 30),
        #         myformula = myformula,
        #         myexplanatory = myexplanatory,
        #         mycontexpl = mycontexpl,
        #         formula2 = formula2
        #     )
        # )




        ## finalfit Multivariable table ----


        model <- private$.fitModelWithSelection(myformula, mydata)


        # finalfit::finalfit(.data = mydata,
        #                    formula = myformula,
        #                    # dependent = myformula,
        #                    # explanatory = formula2,
        #
        #                    metrics = TRUE) -> tMultivariable


        text2_model_selection <- glue::glue("
                                    <br>
                                    <b>Model Metrics:</b>
                                    ",
                                            unlist(tMultivariable[[2]]),
                                            "
                                    <br>
                                    ")

        # Add selection results to the output
        if (self$options$modelSelection != "enter") {
          text2_model_selection <- paste0(
            text2_model_selection,
            "\n<br><b>Model Selection Results:</b><br>",
            "Selection method: ",
            self$options$modelSelection,
            "<br>Selection criteria: ",
            self$options$selectionCriteria,
            "<br>Variables in final model: ",
            paste(names(model$coefficients), collapse = ", ")
          )
        }




        if (self$options$uselandmark) {
          landmark <- jmvcore::toNumeric(self$options$landmark)

          text2_model_selection <- glue::glue(
            text2_model_selection,
            "Landmark time used as: ",
            landmark,
            " ",
            self$options$timetypeoutput,
            "."
          )
        }

        if (self$options$modelSelection != "enter") {
          text2_model_selection <- glue::glue(
            text2_model_selection,
            "Note: Stepwise selection methods should be used with caution. They may not always select the most theoretically meaningful model and can lead to overfitting."
          )

        }



        self$results$text2_model_selection$setContent(text2_model_selection)



        text_model_selection <- knitr::kable(
          tMultivariable[[1]],
          row.names = FALSE,
          align = c('l', 'l', 'r', 'r', 'r', 'r'),
          format = "html"
        )

        self$results$text_model_selection$setContent(text_model_selection)

      }



    )
  )
