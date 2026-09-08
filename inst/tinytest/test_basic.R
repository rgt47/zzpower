# Power-function invariants. This file previously held a single
# expect_true(TRUE), which asserts nothing.

zfun <- list(trend = trend_power, mcnemar = mcnemar_power,
             mixed = mixed_model_power)

for (nm in names(zfun)) {
  f <- zfun[[nm]]

  # At the null the power of a level-alpha test is alpha. These
  # returned NA: the guard sidestepped a case whose answer is known,
  # and a grid of effect sizes that included zero came back with a
  # hole in it.
  for (a in c(0.01, 0.05, 0.10)) {
    expect_equal(f(n = 200, d = 0, sig.level = a)$power, a,
      tolerance = 1e-8,
      info = paste(nm, "at d = 0 has power equal to alpha"))
  }

  # Power rises with sample size and with effect size, and is bounded.
  p1 <- f(n = 200, d = 0.2, sig.level = 0.05)$power
  p2 <- f(n = 400, d = 0.2, sig.level = 0.05)$power
  p3 <- f(n = 200, d = 0.4, sig.level = 0.05)$power
  expect_true(p2 > p1, info = paste(nm, "power rises with n"))
  expect_true(p3 > p1, info = paste(nm, "power rises with the effect"))
  expect_true(p1 > 0 && p1 < 1, info = paste(nm, "power is a probability"))

  # Continuity through the null: no jump between d = 0 and a tiny d.
  near <- f(n = 200, d = 1e-6, sig.level = 0.05)$power
  expect_true(abs(near - 0.05) < 1e-4,
    info = paste(nm, "is continuous through d = 0"))

  # A degenerate sample size has no answer.
  expect_true(is.na(f(n = 0, d = 0.2, sig.level = 0.05)$power),
    info = paste(nm, "returns NA for n = 0"))
}

# The log-rank formula is Schoenfeld (1981): its arguments are expected
# events, not sample sizes, and power is
# pnorm(sqrt(E p1 p2) |log HR| - z).
E1 <- 60
E2 <- 60
hr <- 2
expect_equal(
  logrank_power(h = log(hr), n1 = E1, n2 = E2, sig.level = 0.05)$power,
  stats::pnorm(sqrt((E1 + E2) * (E1 / (E1 + E2)) * (E2 / (E1 + E2))) *
                 abs(log(hr)) - stats::qnorm(0.975)),
  info = "log-rank power follows the Schoenfeld formula")
expect_true(
  logrank_power(h = log(3), n1 = E1, n2 = E2, sig.level = 0.05)$power >
    logrank_power(h = log(2), n1 = E1, n2 = E2, sig.level = 0.05)$power,
  info = "log-rank power rises with the hazard ratio")
expect_true(is.na(logrank_power(h = log(2), n1 = 0, n2 = 0,
                                sig.level = 0.05)$power),
  info = "log-rank power is NA with no events")
