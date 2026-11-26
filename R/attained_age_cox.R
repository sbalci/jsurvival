# Internal building block. Each row is one independent participant.
# Both ages are in years; follow-up duration alone is not an exit age.
.fitAttainedAgeCox <- function(age_at_entry, age_at_exit, event, covariates) {
    if (!is.data.frame(covariates) || !nrow(covariates) || !ncol(covariates))
        stop("Supply a non-empty data frame of baseline covariates.", call. = FALSE)

    sizes <- lengths(list(age_at_entry, age_at_exit, event))
    if (any(sizes != nrow(covariates)))
        stop("Ages, event, and covariates must have the same number of rows.", call. = FALSE)

    valid_age <- function(x) {
        is.numeric(x) && is.null(dim(x)) && all(is.finite(x) & x >= 0)
    }
    if (!valid_age(age_at_entry) || !valid_age(age_at_exit))
        stop("Entry and exit ages must be finite, nonnegative numbers in years.", call. = FALSE)
    if (any(age_at_exit <= age_at_entry))
        stop("Exit age must be greater than entry age on every row.", call. = FALSE)
    if (!(is.logical(event) || is.numeric(event)) || !is.null(dim(event)) ||
        anyNA(event) || !all(event %in% c(0, 1)))
        stop("Event must be coded 1 for event and 0 for censoring (or TRUE/FALSE).",
             call. = FALSE)
    if (!any(event == 1))
        stop("At least one observed event is required.", call. = FALSE)

    predictors <- names(covariates)
    if (anyNA(predictors) || any(!nzchar(predictors)) ||
        anyDuplicated(predictors) || "." %in% predictors)
        stop("Covariate names must be unique and cannot be empty or '.'.", call. = FALSE)
    valid_covariate <- function(x) {
        is.null(dim(x)) && !anyNA(x) &&
            ((is.numeric(x) && all(is.finite(x))) || is.factor(x) || is.logical(x))
    }
    if (!all(vapply(covariates, valid_covariate, logical(1))))
        stop("Covariates must be complete finite numeric, factor, or logical columns.",
             call. = FALSE)

    # Build the formula from symbols rather than parsing user column names.
    internal <- utils::tail(make.unique(c(predictors, ".age_entry", ".age_exit",
                                         ".age_event")), 3L)
    model_data <- as.data.frame(covariates)
    model_data[[internal[1L]]] <- age_at_entry
    model_data[[internal[2L]]] <- age_at_exit
    model_data[[internal[3L]]] <- as.integer(event)
    lhs <- as.call(c(list(quote(survival::Surv)), lapply(internal, as.name)))
    rhs <- Reduce(function(x, y) call("+", x, y), lapply(predictors, as.name))
    model_formula <- stats::as.formula(call("~", lhs, rhs))
    survival::coxph(model_formula, data = model_data, ties = "efron",
                    na.action = stats::na.fail, singular.ok = FALSE, x = TRUE, y = TRUE)
}
