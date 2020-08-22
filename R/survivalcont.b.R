
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
        .run = function() {


            # private$.todo()



            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
