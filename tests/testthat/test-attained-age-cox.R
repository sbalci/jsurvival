attained_age_fixture <- function() {
    i <- seq_len(48)
    data.frame(
        enter = c(50, 50, 57, 55, 51 + i %% 8),
        exit = c(55, 55, 63, 64, 62 + i / 10),
        event = c(1L, 0L, 0L, 0L, as.integer((i %% 5) %in% c(0, 2))),
        group = factor(rep(c("A", "B", "B", "A"), length.out = 52)),
        score = c(2, 1, 3, 2, (i %% 11) / 3)
    )
}

test_that("attained-age Cox matches an explicit multivariable survival fit", {
    d <- attained_age_fixture()
    covariates <- d[c("group", "score")]
    names(covariates) <- c("exposure group", ".age_entry")
    fit <- .fitAttainedAgeCox(d$enter, d$exit, d$event, covariates)
    ref <- survival::coxph(survival::Surv(enter, exit, event) ~ group + score,
                           data = d, ties = "efron")
    expect_equal(unname(stats::coef(fit)), unname(stats::coef(ref)))
    expect_equal(unname(stats::vcov(fit)), unname(stats::vcov(ref)))
    expect_equal(fit$nevent, sum(d$event))
    expect_identical(attr(fit$y, "type"), "counting")
})

test_that("the risk set excludes later entrants and includes same-age censoring", {
    d <- attained_age_fixture()
    fit <- .fitAttainedAgeCox(d$enter, d$exit, d$event, d[c("group", "score")])
    detail <- survival::coxph.detail(fit, riskmat = TRUE)
    at_55 <- match(55, detail$time)
    expect_false(is.na(at_55))
    # (entry, exit]: event and censoring at 55 are at risk; entry at 55 or 57 is not.
    expect_equal(as.integer(detail$riskmat[1:4, at_55]), c(1L, 1L, 0L, 0L))
    expect_equal(as.integer(detail$nrisk[at_55]), sum(d$enter < 55 & d$exit >= 55))
})

test_that("invalid intervals or event coding are not silently changed", {
    d <- attained_age_fixture()
    x <- d[c("group", "score")]
    expect_error(.fitAttainedAgeCox(d$enter, d$enter, d$event, x), "greater than")
    expect_error(.fitAttainedAgeCox(d$enter[-1], d$exit, d$event, x), "same number")
    expect_error(.fitAttainedAgeCox(d$enter, d$exit, d$event + 1L, x), "coded 1")
    expect_error(.fitAttainedAgeCox(d$enter, d$exit, rep(0L, nrow(d)), x), "observed event")
    x$score[1] <- NA
    expect_error(.fitAttainedAgeCox(d$enter, d$exit, d$event, x), "complete")
})
