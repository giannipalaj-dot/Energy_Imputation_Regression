# ==============================================================================
# BUA 650 Seminar -- Imputation & Regression Review
# Data: daily GASOLINE, PROPANE, VIX, WTI prices, Jan 4 2010 - Jan 1 2025
#
# WHAT THIS EXERCISE IS ABOUT
#   Question: how closely do daily crude oil (WTI) returns move with gasoline
#   returns and with market fear (the VIX)?
#   Before we can answer that with a regression, the data has to be fit for
#   purpose. Real business data is rarely clean: it has gaps, it trends, and
#   its volatility changes over time. This script works through the full
#   pipeline, from raw spreadsheet to defensible results.
#
# THE DATA (daily closing values; blank cells are days with no trading)
#   WTI       West Texas Intermediate crude oil spot price, Cushing OK,
#             $/barrel. The U.S. crude oil benchmark.
#   GASOLINE  Conventional gasoline spot price, New York Harbor, $/gallon.
#             A refined product made from crude.
#   PROPANE   Propane spot price, Mont Belvieu TX, $/gallon. Comes from both
#             natural gas processing and oil refining.
#   VIX       CBOE Volatility Index: the market's expected S&P 500 volatility
#             over the next 30 days, in annualized % points. Often called the
#             "fear gauge". It measures general financial market stress, not
#             energy specifically.
#   Source: U.S. EIA / CBOE, via FRED (series DCOILWTICO, DGASNYH,
#   DPROPANEMBTX, VIXCLS).
#
# WHY THESE VARIABLES?
#   Gasoline is refined from crude, so the two prices should move together.
#   VIX tests whether broad market fear also moves oil. Propane is held back
#   for an exercise at the end (Section 8).
#
# WHY IT MATTERS
#   - Missing data: most tools either fail or silently drop rows when values
#     are missing. How you fill the gaps is a modeling choice, and you should
#     check that the filled values are plausible.
#   - Stationarity: regressing trending price series on each other can produce
#     impressive but meaningless results (a "spurious regression"). Testing
#     and transforming first protects against this.
#   - Diagnostics: a regression's coefficients are only half the story. If
#     the model's assumptions fail, the standard errors and p-values can be
#     badly wrong. Here, correcting for changing volatility cuts the key
#     t-statistic from about 40 to about 9.
#   The takeaway: the analyst's value lies less in running lm() than in
#   knowing whether to trust its output.
#
# Workflow:
#   0. One-time RStudio workspace settings
#   1. Setup & load data
#   2. Diagnose missing values
#   3. Impute missing values (linear interpolation)
#   4. Check the imputation visually (observed vs imputed histograms)
#   5. Test for stationarity (Augmented Dickey-Fuller)
#   6. Transform prices to log returns (DLOG) and re-test
#   7. Regress WTI returns on gasoline returns and VIX; run diagnostics
#   8. Student exercise: add propane to the model
#   9. Conclusions: putting it all together
#
# Run top to bottom. Needs only this file and "Energy Data for R.xlsx".
#
# "EXPECTED RESULTS" comments give approximate values so you can check your
# output. Small differences in the last decimal place are normal.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. BEFORE YOU START: RStudio WORKSPACE SETTINGS (one-time setup)
# ------------------------------------------------------------------------------
# By default, RStudio saves everything in memory to a hidden .RData file when
# you close it, then reloads it the next time you open it. Leftover objects
# from a past session can make a script look like it works when it doesn't
# (for example, an old copy of energy_data or model is still in memory).
#
# To turn this off (once per computer):
#   1. Tools -> Global Options...
#   2. Select "General" in the left sidebar (Basic tab)
#   3. Under "Workspace":
#        - UNCHECK "Restore .RData into workspace at startup"
#        - Set "Save workspace to .RData on exit:" to "Never"
#   4. Click Apply, then OK
#
# You'll know it worked when the console no longer shows
# "[Workspace loaded from ...]" when RStudio opens.
#
# If you did NOT change these settings, remove the # from the line below
# to clear leftover objects before the script runs:

# rm(list = ls())   # clears ALL objects in memory -- save anything you need first


# ------------------------------------------------------------------------------
# 1. SETUP & LOAD
# ------------------------------------------------------------------------------

# Point R at the folder containing the Excel file.
# Students: replace this with the path to YOUR folder, using forward slashes (/).
setwd("C:/Users/giann/Desktop/BUA 650")

# Packages used:
#   readxl   - read_excel(): read Excel files
#   zoo      - na.approx(): linear interpolation of missing values
#   tseries  - adf.test(): Augmented Dickey-Fuller stationarity test
#   ggplot2  - plotting
#   lmtest   - dwtest(), bgtest(), bptest(), coeftest(): regression diagnostics
#   car      - durbinWatsonTest(): alternative Durbin-Watson test
#   sandwich - vcovHC(), NeweyWest(): robust standard errors
pkgs <- c("readxl", "zoo", "tseries", "ggplot2", "lmtest", "car", "sandwich")

# Install only the packages that are missing, then load them all
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

# read_excel() returns a tibble (a data frame).
# If this errors, run list.files(pattern = "xlsx") and copy the exact name.
energy_data <- read_excel("Energy Data for R .xlsx")

# Excel stores dates as date-times; as.Date() keeps only the calendar date
energy_data$Date <- as.Date(energy_data$Date)

head(energy_data)
# EXPECTED RESULTS: 5 columns (Date, GASOLINE, PROPANE, VIX, WTI);
# first row 2010-01-04 with GASOLINE 2.096, PROPANE 1.373, VIX 20.04, WTI 81.52.


# ------------------------------------------------------------------------------
# 2. DIAGNOSE MISSING VALUES
# ------------------------------------------------------------------------------
# Always look before you fix: how much is missing, and where?

colSums(is.na(energy_data))   # is.na() flags each NA; colSums() counts per column
nrow(energy_data)             # total rows

# complete.cases() is TRUE for rows with no NAs; "!" selects the incomplete rows
energy_data[!complete.cases(energy_data), ]

tail(energy_data, 10)
# EXPECTED RESULTS:
#   3,913 rows. NAs: GASOLINE 150, PROPANE 157, VIX 120, WTI 148.
#   166 rows have at least one NA:
#     - 117 rows are blank in all four series. These are mostly market holidays
#       (e.g., MLK Day 2010-01-18, Christmas 2024-12-25).
#     - 49 rows are missing only some series (markets closed on different days).
#   The last row, 2025-01-01, is entirely NA.
#
# Why this matters: adf.test() stops with an error if a series contains NAs,
# so imputation has to come before stationarity testing.


# ------------------------------------------------------------------------------
# 3. IMPUTE WITH LINEAR INTERPOLATION
# ------------------------------------------------------------------------------
# zoo::na.approx() fills each NA by drawing a straight line between the
# nearest known values before and after the gap. For a one-day gap, that is
# simply the average of the two neighbors.
# It cannot fill a gap at the very start or end of a series, because there is
# no known value on one side.

# The final row (2025-01-01) has nothing after it, so drop it
energy_data <- energy_data[-nrow(energy_data), ]

vars <- c("GASOLINE", "PROPANE", "VIX", "WTI")

# Save a copy BEFORE imputing so we can compare observed vs imputed in Section 4
orig <- energy_data[vars]

# Interpolate each series.
# na.rm = FALSE keeps the output the same length as the input. With the
# default (na.rm = TRUE), any NA that can't be filled is DROPPED, the vector
# comes back shorter than the data frame, and the assignment fails.
for (v in vars) {
  energy_data[[v]] <- na.approx(energy_data[[v]], na.rm = FALSE)
}

colSums(is.na(energy_data))   # should now be all zeros

# Spot-check Christmas 2024 (middle row) against Dec 24 and Dec 26
energy_data[3907:3909, ]
# EXPECTED RESULTS: 3,912 rows, zero NAs.
#   Dec 24: WTI 70.87   Dec 25 (imputed): WTI 70.625   Dec 26: WTI 70.38
#   70.625 is exactly the midpoint of 70.87 and 70.38. That is how linear
#   interpolation fills a one-day gap.


# ------------------------------------------------------------------------------
# 4. CHECK THE IMPUTATION: OBSERVED vs IMPUTED HISTOGRAMS
# ------------------------------------------------------------------------------
# A good imputation produces values that look like the real data.
# mi.hist() (adapted from the 'mi' package, Gelman & Su) overlays three
# outline histograms on the same bins:
#   blue  = observed values (the original, non-missing data)
#   red   = imputed values only (the gaps we filled)
#   black = completed series (observed + imputed together)

# Helper: draw a histogram as an outline (a "step" line) so several can be overlaid
histlineplot <- function(h, shift = 0, col = "black", lty = 1, lwd = 1) {
  n.bins <- length(h$breaks) - 1
  # each interior break is used twice to form the vertical edges of the steps
  x.pos  <- h$breaks[rep(c(1, 2:n.bins, n.bins + 1),
                         c(1, rep(2, n.bins - 1), 1))]
  y.pos  <- rep(h$counts, rep(2, n.bins))   # each bar height for both its edges
  # close the outline at zero on both ends
  x.pos  <- c(x.pos[1], x.pos, x.pos[length(x.pos)])
  y.pos  <- c(0, y.pos, 0)
  lines(x.pos + shift, y.pos, col = col, lty = lty, lwd = lwd)
}

# Main function
#   completed : the series AFTER imputation (no NAs)
#   observed  : the same series BEFORE imputation (with NAs)
mi.hist <- function(completed, observed, main = "", xlab = "",
                    obs.col = "blue", imp.col = "black", mis.col = "red") {

  if (length(completed) != length(observed))
    stop("observed and completed vectors must be the same length")

  obs.only <- observed[!is.na(observed)]    # the real data points
  imp.only <- completed[is.na(observed)]    # the values we filled in

  # Shared bins for all three histograms (about sqrt(n) bins)
  b <- seq(min(completed), max(completed), length.out = sqrt(length(completed)))
  binwidth <- b[2] - b[1]

  # hist(..., plot = FALSE) computes bin counts without drawing
  h.obs <- hist(obs.only,  plot = FALSE, breaks = b)
  h.mis <- hist(imp.only,  plot = FALSE, breaks = b)
  h.imp <- hist(completed, plot = FALSE, breaks = b)

  # Empty plot frame sized to the tallest histogram
  plot(range(b), c(0, max(h.imp$counts) * 1.05), type = "n",
       yaxs = "i", bty = "l", main = main, xlab = xlab, ylab = "Frequency")

  # Shift the red and blue outlines slightly left/right so they don't hide
  # each other or the black line
  mlt <- if (max(h.imp$counts) > 100) 0.2 else 0.1
  histlineplot(h.mis, shift = -mlt * binwidth, col = mis.col)
  histlineplot(h.obs, shift =  mlt * binwidth, col = obs.col)
  histlineplot(h.imp, col = imp.col)
}

# One panel per variable (par(mfrow) sets a 2x2 grid; op restores it after)
op <- par(mfrow = c(2, 2))
for (v in vars) {
  mi.hist(energy_data[[v]], orig[[v]], main = v, xlab = v)
  # Add a legend to the VIX panel only (its upper-right corner is empty)
  if (v == "VIX") {
    legend("topright",
           legend = c("Observed", "Imputed", "Completed"),
           col    = c("blue", "red", "black"),
           lty    = 1, lwd = 2, bty = "n", cex = 0.9)
  }
}
par(op)
# HOW TO READ IT:
#   Only about 3-4% of each series is imputed, so the red line is low and the
#   black and blue lines nearly overlap. The question is whether the red
#   values fall where the real data lives. They should: interpolated values
#   always lie between two observed neighbors, so they can never fall outside
#   the observed range.
#   That is also the limitation of linear interpolation: it can't create
#   extremes, and it smooths over whatever really happened on the missing day.
#
# WTI PANEL: the x-axis stretches down to about -40 because of the single
# -$36.98 day on 2020-04-20. That one observation is the tiny blip at the far
# left. It is real data, not an imputation error.
#
# DISCUSSION POINT: most gaps here are holidays, when markets were closed.
# Interpolating creates a price "move" on a day with no trading. For a
# returns analysis, dropping holiday rows is a defensible alternative.


# ------------------------------------------------------------------------------
# 5. STATIONARITY: AUGMENTED DICKEY-FULLER TEST (price levels)
# ------------------------------------------------------------------------------
# A series is (weakly) stationary if its mean and variance don't drift over
# time. Regressing one non-stationary series on another risks a "spurious
# regression": high R-squared and significant t-stats with no real relationship.
#
# tseries::adf.test() regresses the change in the series on its lagged level,
# a constant, a time trend, and lagged changes. Lags default to
# trunc((n-1)^(1/3)), which is 15 here.
#   H0: unit root (non-stationary)
#   H1: stationary
#   p < 0.05 -> reject H0 -> treat the series as stationary
# Note: adf.test() reports p-values only between 0.01 and 0.99. A printed 0.01
# means "0.01 or smaller", and R shows a warning saying so. That is expected.

adf <- lapply(energy_data[vars], adf.test)   # run the test on each column

adf_results <- data.frame(
  Variable      = vars,
  ADF_Statistic = unname(sapply(adf, function(t) t$statistic)),
  P_Value       = unname(sapply(adf, function(t) t$p.value))
)
adf_results$Stationary <- adf_results$P_Value < 0.05
print(adf_results)
# EXPECTED RESULTS (approximate):
#   GASOLINE  ADF about -2.56, p about 0.34 -> non-stationary
#   PROPANE   ADF about -2.58, p about 0.33 -> non-stationary
#   VIX       ADF about -6.06, p = 0.01   -> stationary
#   WTI       ADF about -2.19, p about 0.50 -> non-stationary
# INTERPRETATION: the three commodity prices wander (unit roots), so they
# can't be used in levels. VIX is mean-reverting: fear spikes, then settles
# back. That is why VIX stays in levels in the regression while the prices
# are transformed.


# ------------------------------------------------------------------------------
# 6. TRANSFORM: DIFFERENCE OF LOGS (EViews DLOG), THEN RE-TEST
# ------------------------------------------------------------------------------
# dlog(x_t) = log(x_t) - log(x_{t-1}), approximately the daily % change.
#   diff() removes the wandering level (the unit root).
#   log() makes changes proportional, so a $1 move at $20 and a $5 move at
#   $100 are treated the same.
# diff() returns one fewer value, so pad the front with NA to keep the length.
#
# WARNING SPECIFIC TO THIS DATA: on 2020-04-20, WTI settled at -$36.98
# (the May futures contract went negative as storage ran out).
# log() of a negative number is undefined, so R returns NaN with the warning
# "NaNs produced". That makes WTI_dlog NaN on BOTH 2020-04-20 and 2020-04-21,
# because the second day's return uses the first day's log. This is expected.
# Those two days are dropped automatically from the regression below.

energy_data$GASOLINE_dlog <- c(NA, diff(log(energy_data$GASOLINE)))
energy_data$PROPANE_dlog  <- c(NA, diff(log(energy_data$PROPANE)))
energy_data$WTI_dlog      <- c(NA, diff(log(energy_data$WTI)))

head(energy_data[, c("Date", "GASOLINE", "GASOLINE_dlog", "WTI", "WTI_dlog")], 10)

# Show the rows where WTI_dlog could not be computed
energy_data[is.na(energy_data$WTI_dlog), c("Date", "WTI", "WTI_dlog")]
# EXPECTED RESULTS: 2010-01-04 (first row, NA by construction), 2020-04-20,
# and 2020-04-21.

# Re-run ADF on the transformed series to confirm the transform worked.
# na.omit() is needed because adf.test() stops on NA/NaN.
adf.test(na.omit(energy_data$GASOLINE_dlog))
adf.test(na.omit(energy_data$WTI_dlog))
# EXPECTED RESULTS: ADF statistics around -13 to -14, p = 0.01 (smaller than
# printed) -> both return series are stationary. The transform did its job.

# ---- Visual walkthrough: LEVEL -> FIRST DIFFERENCE -> DLOG ----
# Three views of the same WTI series, each one step further transformed.
# Watch what each step fixes and what it leaves behind.

# First difference in dollars (no log), for comparison with DLOG
energy_data$WTI_diff <- c(NA, diff(energy_data$WTI))

# Helper so all three charts share one style. print() makes sure each chart
# displays even when the script is run with source().
#   y_col     : column to plot
#   show_zero : draw the red dashed zero line? (not useful for price levels)
plot_wti <- function(y_col, title, subtitle, ylab, show_zero = TRUE) {
  pd <- energy_data[-1, c("Date", y_col)]          # drop the leading NA row
  names(pd)[2] <- "y"
  p <- ggplot(pd, aes(Date, y)) +
    geom_line(color = "darkblue", linewidth = 0.5) +
    geom_hline(yintercept = mean(pd$y, na.rm = TRUE),
               color = "green", linewidth = 1.2)
  if (show_zero)
    p <- p + geom_hline(yintercept = 0, color = "red",
                        linetype = "dashed", linewidth = 0.8)
  p + labs(title = title, subtitle = subtitle, x = "Date", y = ylab,
           caption = if (show_zero) "Red dashed line = 0, Green line = Mean"
                     else "Green line = Mean") +
    theme_minimal() +
    theme(plot.title    = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "gray40"))
}

# STEP 1: the price LEVEL
print(plot_wti("WTI",
         title    = "Step 1: WTI Price Level ($/barrel)",
         subtitle = "Non-stationary: wanders with no fixed mean (ADF p = 0.50)",
         ylab     = "WTI ($/barrel)", show_zero = FALSE))
# HOW TO READ IT:
#   - The price wanders: roughly $80-$110 from 2011 to mid-2014, a collapse below $30 in
#     early 2016, a plunge below zero in April 2020, then a peak of about $124
#     in March 2022 (Russia-Ukraine).
#   - The green line (overall mean, about $72) is meaningless. The series
#     spends years far above or below it and never returns to it in any
#     regular way. That is what a unit root looks like, and it matches the
#     ADF result in Section 5.
#   - The spike to -$36.98 (2020-04-20) shows up as a single downward needle.

# STEP 2: the FIRST DIFFERENCE (daily change in dollars)
print(plot_wti("WTI_diff",
         title    = "Step 2: First Difference of WTI ($ change per day)",
         subtitle = "Trend removed, but the size of moves depends on the price level",
         ylab     = "Daily change ($/barrel)"))
adf.test(na.omit(energy_data$WTI_diff))
# EXPECTED RESULTS: ADF about -15.4, p = 0.01 -> stationary.
# HOW TO READ IT:
#   - Differencing worked for the MEAN: the series now hovers around zero
#     (mean about -$0.002/day) and the ADF test says stationary.
#   - The April 2020 days dominate the chart: -$55.29 on 04-20 (from $18.31
#     to -$36.98) and +$45.89 on 04-21. Everything else is squeezed into a
#     thin band. Note that first differences CAN handle a negative price,
#     unlike logs.
#   - The hidden problem: a dollar move means different things at different
#     price levels. A $1 move at $100 oil is a 1% change; at $40 oil it is
#     2.5%. Compare two years:
#        2013: average price about $98, daily $ change SD about 1.11
#        2016: average price about $43, daily $ change SD about 1.16
#     In dollars, the two years look equally volatile.

# STEP 3: DLOG (daily log return, approximately % change)
print(plot_wti("WTI_dlog",
         title    = "Step 3: WTI Log Returns (Differenced Log Transform)",
         subtitle = "Stationary in mean; volatility still clusters (see 2020)",
         ylab     = "Log Returns (Dlog WTI)"))
# EXPECTED RESULTS: you may see a warning that ggplot removed 2 rows (the NaN
# days). The gap they leave in April 2020 is too small to see at this scale.
# HOW TO READ IT:
#   - Same two years, now in proportional terms:
#        2013: daily log return SD about 0.011 (about 1.1% per day)
#        2016: daily log return SD about 0.030 (about 3.0% per day)
#     2016 was nearly 3x as turbulent. Steps 1 and 2 hid that; dlog reveals it.
#     This is why we use proportional changes for prices.
#   - The green (mean) and red (zero) lines nearly coincide: the mean is
#     about 0.00015 (0.015% per day), essentially zero.
#   - The variance is still NOT constant: calm stretches alternate with
#     volatile bursts (2014-16 price collapse, 2020 COVID). The extremes are
#     log returns of -0.28 on 2020-03-09 (Saudi-Russia price war) and +0.43 on
#     2020-04-22 (rebound after the negative-price days).
#   - The cost: dlog cannot handle the negative price, so two days are lost.
#
# SUMMARY OF THE PROGRESSION:
#   Level        -> trending, non-stationary. Can't regress on this.
#   First diff   -> stationary mean, but variance is tied to the price level.
#   Dlog         -> stationary mean, comparable across price levels, and
#                   interpretable as % change. Remaining issue: volatility
#                   clustering, which is handled in Section 7.


# ------------------------------------------------------------------------------
# 7. REGRESSION & DIAGNOSTICS
# ------------------------------------------------------------------------------
# ROADMAP FOR THIS SECTION -- fit, look, test, fix:
#   FIT   the regression and read the coefficients
#   LOOK  at the residuals (correlograms) for patterns the model missed
#   TEST  formally: Durbin-Watson and Breusch-Godfrey (serial correlation),
#         Breusch-Pagan (heteroskedasticity)
#   FIX   the standard errors (HC3, then Newey-West) and see whether the
#         conclusions survive
# OLS assumes the errors are independent with constant variance. The
# diagnostics check whether those assumptions hold. If they don't, the
# coefficients are still fine, but the standard errors are not.
#
# Model: WTI_dlog = b0 + b1*GASOLINE_dlog + b2*VIX + error
# lm() fits by ordinary least squares. Row 1 is dropped (dlog is NA there);
# lm() also drops the two NaN days from 2020 and reports this in the output
# as "(2 observations deleted due to missingness)".

model <- lm(WTI_dlog ~ GASOLINE_dlog + VIX, data = energy_data[-1, ])
summary(model)
nobs(model)   # number of observations actually used
# EXPECTED RESULTS (approximate):
#   n = 3,909
#                  Estimate   Std.Error   t      p
#   (Intercept)    0.00095    0.00102     0.93   0.35
#   GASOLINE_dlog  0.580      0.0143     40.6    < 2e-16
#   VIX           -0.000046   0.000052   -0.88   0.38
#   R-squared about 0.299, Adj R-squared about 0.298, F about 831 (p < 2e-16)
# INTERPRETATION:
#   - GASOLINE_dlog: on days gasoline rises 1%, WTI rises about 0.58% on
#     average (holding VIX constant). Both sides are log changes, so this is
#     an elasticity. This is a same-day association, not a causal effect:
#     crude and gasoline respond to the same news.
#   - VIX: the level of market fear has no significant relationship with
#     the day's oil return.
#   - Intercept: not different from zero. There is no drift in daily oil
#     returns beyond what gasoline explains.
#   - R-squared: gasoline and VIX explain about 30% of the day-to-day
#     variation in oil returns.

# 95% confidence intervals: estimate +/- about 1.96 * standard error
conf_intervals <- confint(model)
print(conf_intervals)
# EXPECTED RESULTS: GASOLINE_dlog about [0.552, 0.608]. The VIX interval
# contains 0, which agrees with its non-significant p-value.

# --- Durbin-Watson: autocorrelation in the residuals ---
# DW = 2 means no first-order autocorrelation. Below 2 means positive
# autocorrelation; above 2 means negative.
# lmtest::dwtest() tests only for POSITIVE autocorrelation by default.
# car::durbinWatsonTest() is two-sided and gets its p-value by bootstrap
# (random resampling), so set a seed to get the same p-value every run.
print(dwtest(model))
set.seed(650)
durbinWatsonTest(model)
# EXPECTED RESULTS: DW about 2.15, lag-1 residual autocorrelation about -0.07.
#   dwtest p-value near 1: there is no POSITIVE autocorrelation.
#   durbinWatsonTest p-value 0 (i.e., below 0.001; none of the bootstrap
#   replications were as extreme): there IS slight NEGATIVE autocorrelation.
# The two tests don't contradict each other; they test different alternative
# hypotheses. In practice, -0.07 is small. Daily returns having slight
# negative autocorrelation is common, for example from bid-ask bounce or
# reversals after overreaction.

# --- Correlograms: LOOK at the residuals before testing them ---
# A correlogram plots the correlation between the residual today and the
# residual k days earlier, for k = 1, 2, 3, ... (EViews: View -> Residual
# Diagnostics -> Correlogram). We look at two versions:
#   1. Residuals          -> is the DIRECTION of today's error predictable
#                            from past errors? (serial correlation)
#   2. Squared residuals  -> is the SIZE of today's error predictable from
#                            past errors? (volatility clustering, also called
#                            ARCH effects)
# A well-behaved model shows no pattern in either.
#
# Two plots for each:
#   ACF  (autocorrelation function): total correlation at lag k.
#   PACF (partial autocorrelation): correlation at lag k AFTER removing the
#        effect of lags 1 through k-1. It shows which lags matter directly.
# Reading the plots:
#   - The bar at lag 0 on the ACF is always 1 (each value vs itself). Ignore it.
#   - Blue dashed bands = +/- 1.96/sqrt(n), about +/-0.03 here. Bars outside
#     the bands are significant at roughly the 5% level, one lag at a time.
#   - With 20 lags, expect about 1 bar outside the bands by chance alone.
#     Look for many bars outside, or very large ones.
# The Ljung-Box Q-statistic (Box.test) tests all lags up to h jointly:
#   H0: all autocorrelations up to lag h are zero
#   p < 0.05 -> reject H0 -> there is a pattern
# (EViews reports these as the "Q-Stat" and "Prob" columns.)

res <- residuals(model)

# 1. Correlogram of the residuals
op <- par(mfrow = c(1, 2))
acf(res,  lag.max = 20, main = "ACF: Residuals")
pacf(res, lag.max = 20, main = "PACF: Residuals")
par(op)

Box.test(res, lag = 5,  type = "Ljung-Box")
Box.test(res, lag = 10, type = "Ljung-Box")
Box.test(res, lag = 20, type = "Ljung-Box")
# EXPECTED RESULTS (approximate):
#   Largest ACF bars: lag 3 about -0.16, lag 20 about +0.14, lag 10 about -0.14,
#   lag 14 about +0.11, lag 4 about -0.10. Lag 1 is about -0.07, which DW caught.
#   PACF: lag 3 about -0.16, lag 10 about -0.13, lag 4 about -0.12.
#   Ljung-Box Q: lag 5 about 153, lag 10 about 271, lag 20 about 456;
#   all p-values effectively 0.
# INTERPRETATION:
#   - The residuals are not pure noise. The biggest correlations are at lags
#     3 and 4, NOT at lag 1, so DW (which checks only lag 1) understated the
#     problem.
#   - The correlations are negative: a large error tends to be partly reversed
#     a few days later.
#   - Even the biggest bars are small (|r| < 0.16), so you can't predict
#     tomorrow's oil return from them in any useful way. With n of about
#     3,900, even small correlations are statistically significant. That is
#     still enough to distort the standard errors.
#   - Where does it come from? Dropping March-June 2020 shrinks lag 3 to
#     about -0.06 and lag 4 to about 0. Much of the pattern comes from the
#     COVID-era whipsaw, when huge drops were followed by huge rebounds a few
#     days later. A few extreme weeks can dominate 15 years of data.

# 2. Correlogram of the SQUARED residuals
op <- par(mfrow = c(1, 2))
acf(res^2,  lag.max = 20, main = "ACF: Squared Residuals")
pacf(res^2, lag.max = 20, main = "PACF: Squared Residuals")
par(op)

Box.test(res^2, lag = 5,  type = "Ljung-Box")
Box.test(res^2, lag = 10, type = "Ljung-Box")
Box.test(res^2, lag = 20, type = "Ljung-Box")
# EXPECTED RESULTS (approximate):
#   ACF: every bar from lag 1 to 20 is positive and outside the bands, ranging
#   from about 0.10 to 0.47 (largest: lag 3 about 0.47, lag 6 about 0.46,
#   lag 20 about 0.37, lag 2 about 0.32).
#   Ljung-Box Q: lag 5 about 1,790, lag 10 about 3,342, lag 20 about 4,675;
#   p-values 0.
# INTERPRETATION:
#   - This is far stronger than the plain-residual pattern. The bars are
#     large, all positive, and decay slowly instead of dropping to zero.
#   - Squaring removes the sign, so this measures SIZE only: big moves
#     (either direction) are followed by big moves, and quiet days by quiet
#     days. This is the volatility clustering seen in the Section 6 plot.
#   - Is it just 2020? No. Without March-June 2020 the lag-1 value is still
#     about 0.18, and Ljung-Box Q(10) is still about 660. Volatility
#     clustering runs throughout the sample.
#   - Contrast the two correlograms: the DIRECTION of oil returns is close to
#     unpredictable, but their SIZE is quite predictable. That's typical of
#     financial returns, and it is the idea behind ARCH/GARCH models
#     (a natural next topic).
#   - Consequence: non-constant error variance, which is heteroskedasticity.
#     The Breusch-Pagan test below confirms it formally.

# --- Breusch-Godfrey: HIGHER-ORDER autocorrelation in the residuals ---
# Durbin-Watson has two limits:
#   1. It only checks lag 1 (today's error vs yesterday's). Correlation at
#      lag 3, or with last week's error, goes undetected.
#   2. It isn't valid when the model includes a lagged dependent variable.
# The Breusch-Godfrey (BG) test, also called the LM test for serial
# correlation, fixes both. It works in two steps:
#   1. Regress the residuals on the original X variables PLUS p lags of the
#      residuals (lags 1 through p).
#   2. If those lagged residuals explain the current residual, the errors are
#      serially correlated. Test statistic: LM = n * R-squared of this
#      auxiliary regression, compared to a chi-square with p degrees of freedom.
#   H0: no serial correlation at any lag up to p
#   H1: serial correlation at one or more of those lags
#   p < 0.05 -> reject H0 -> residuals are serially correlated
#
# Choosing p (order): there's no single right answer. For daily data, 5
# (one trading week) is a common choice, and 10 (two weeks) is a useful check.
# order = 1 is the lag-1 test, so it should agree with Durbin-Watson.
print(bgtest(model, order = 1))    # lag 1 only: should agree with DW
print(bgtest(model, order = 5))    # one trading week
print(bgtest(model, order = 10))   # two trading weeks

# EXPECTED RESULTS (approximate):
#   order = 1:  LM about 20,  df = 1,  p about 6e-06
#   order = 5:  LM about 178, df = 5,  p < 2e-16
#   order = 10: LM about 272, df = 10, p < 2e-16
# INTERPRETATION:
#   - order = 1 confirms the DW result: slight negative lag-1 autocorrelation.
#   - The LM statistic jumps from 20 to 178 when we add lags 2-5. The bigger
#     problem is at lags 3 and 4, exactly what the correlogram showed and
#     what DW cannot see. The correlogram SHOWS the pattern; BG formally
#     TESTS it. This is why we follow DW with BG.
#   - Consequence: as with heteroskedasticity, the OLS coefficients are still
#     unbiased, but the default standard errors are unreliable. The fix
#     (Newey-West standard errors) is below.

# --- Breusch-Pagan: heteroskedasticity (non-constant error variance) ---
# H0: constant variance. p < 0.05 -> heteroskedasticity is present.
# bptest() defaults to Koenker's studentized version, which remains valid
# when errors aren't normal. That matters here because returns are fat-tailed.
print(bptest(model))
# EXPECTED RESULTS: BP about 343, df = 2, p < 2e-16 -> strong heteroskedasticity.
# This is the volatility clustering seen in the Section 6 plot. OLS
# coefficients remain unbiased, but the usual standard errors (and so the
# t-stats and p-values above) are unreliable.

# --- Fix: heteroskedasticity-robust (HC3) standard errors ---
# coeftest() re-reports the same coefficients, with standard errors computed
# from vcovHC(), which does not assume constant variance.
coeftest(model, vcov = vcovHC(model, type = "HC3"))
# EXPECTED RESULTS (approximate):
#   GASOLINE_dlog robust SE about 0.065 (vs 0.014), t about 8.9 -> still highly
#   significant. VIX and the intercept remain insignificant.
# LESSON: the conclusions hold, but the robust SE is about 4-5x larger. The
# default t-stat of 40 overstated the precision. Always check BP before
# trusting OLS p-values on financial data.

# --- Fix for BOTH problems: Newey-West (HAC) standard errors ---
# HC3 corrects only for heteroskedasticity. We found serial correlation too.
# Newey-West standard errors are "HAC": Heteroskedasticity- and
# Autocorrelation-Consistent. They allow the error variance to change AND
# allow errors to be correlated up to a chosen number of lags. Weights
# decline with distance (lag 1 counts most, lag 5 least).
#   lag = 5       -> allow correlation up to one trading week (matches BG above)
#   prewhite = FALSE -> use the plain textbook Newey-West estimator
coeftest(model, vcov = NeweyWest(model, lag = 5, prewhite = FALSE))
# EXPECTED RESULTS (approximate):
#                  Estimate    NW Std.Error   t
#   (Intercept)    0.00095     0.0021         0.45
#   GASOLINE_dlog  0.580       0.064          9.0   -> still highly significant
#   VIX           -0.000046    0.00012       -0.37
# INTERPRETATION: the Newey-West results are close to HC3. Correcting for
# serial correlation adds little beyond the heteroskedasticity fix, and the
# conclusions don't change: gasoline returns matter, VIX and the intercept
# don't. Report Newey-West (or HC3) standard errors, not the defaults, for
# this model.
# Try it: lag = 10 gives a gasoline t-stat of about 8.4. The conclusion holds
# across reasonable lag choices.

# --- Summary table (default OLS standard errors) ---
results_summary <- data.frame(
  Estimate  = coef(model),
  Std_Error = summary(model)$coefficients[, "Std. Error"],
  t_value   = summary(model)$coefficients[, "t value"],
  p_value   = summary(model)$coefficients[, "Pr(>|t|)"],
  CI_Lower  = conf_intervals[, 1],
  CI_Upper  = conf_intervals[, 2]
)
print(results_summary)

cat("R-squared:",     round(summary(model)$r.squared, 4),
    "| Adj R-squared:", round(summary(model)$adj.r.squared, 4),
    "| F:",           round(summary(model)$fstatistic[1], 4), "\n")



# ------------------------------------------------------------------------------
# 8. STUDENT EXERCISE: ADD PROPANE TO THE MODEL
# ------------------------------------------------------------------------------
# Propane was cleaned and transformed along with the others but left out of
# the model. Your task: add it and see what changes.
#
# Before running, PREDICT:
#   a) Will propane returns be significant?
#   b) What will happen to the gasoline coefficient (0.580)? Up, down, or
#      unchanged? Why?
#   c) Will R-squared rise a lot or a little?

# Step 1: confirm propane returns are stationary
adf.test(na.omit(energy_data$PROPANE_dlog))

# Step 2: fit the expanded model
model2 <- lm(WTI_dlog ~ GASOLINE_dlog + PROPANE_dlog + VIX, data = energy_data[-1, ])
summary(model2)

# Step 3: robust (Newey-West) standard errors, as in Section 7
coeftest(model2, vcov = NeweyWest(model2, lag = 5, prewhite = FALSE))

# Step 4: how correlated are the two product returns?
cor(energy_data$GASOLINE_dlog, energy_data$PROPANE_dlog, use = "complete.obs")

# EXPECTED RESULTS (approximate). Students: try the predictions above first.
#   ADF on PROPANE_dlog: about -15.3, p = 0.01 -> stationary
#                    Estimate   NW t
#   (Intercept)      0.00092    0.43
#   GASOLINE_dlog    0.471      7.3    (was 0.580)
#   PROPANE_dlog     0.275      6.9    -> significant
#   VIX             -0.000042  -0.34
#   R-squared about 0.361 (was 0.299); Adj R-squared about 0.361
#   Correlation of gasoline and propane returns: about 0.39
# INTERPRETATION:
#   a) Yes. A 1% propane move goes with about a 0.28% WTI move the same day,
#      holding gasoline and VIX constant.
#   b) The gasoline coefficient FALLS, from 0.58 to 0.47. Gasoline and propane
#      returns are correlated (0.39), and both relate to oil. Without propane,
#      gasoline was picking up part of propane's effect. That is OMITTED
#      VARIABLE BIAS: leaving out a relevant variable that is correlated with
#      an included one distorts the included coefficient.
#   c) R-squared rises about 6 points (30% -> 36%). Propane adds information
#      gasoline doesn't already carry, but most daily oil movement is still
#      unexplained.
#   Discussion: is the first model "wrong"? It gave an unbiased answer to a
#   narrower question (oil vs gasoline alone). The lesson: a coefficient's
#   meaning depends on what else is in the model.


# ------------------------------------------------------------------------------
# 9. CONCLUSIONS: PUTTING IT ALL TOGETHER
# ------------------------------------------------------------------------------
# THE ANSWER: daily WTI returns move strongly with gasoline returns. The
# elasticity is about 0.58, or about 0.47 once propane is included. Market
# fear (VIX level) shows no relationship with daily oil returns. Gasoline and
# VIX together explain about 30% of daily oil moves.
#
# WHAT EACH STEP CONTRIBUTED:
#   Imputation    Filled about 4% missing values (mostly holidays) so the
#                 time-series tools could run. The histograms showed the
#                 filled values were plausible.
#   Stationarity  Prices had unit roots; returns did not. Regressing price
#                 levels would have risked a spurious regression.
#   Correlograms  Showed small serial correlation in the residuals and strong
#                 volatility clustering in the squared residuals.
#   Diagnostics   BG confirmed serial correlation beyond lag 1, which DW
#                 missed. BP confirmed heteroskedasticity.
#   Fixes         Newey-West standard errors cut the gasoline t-stat from 40
#                 to about 9. The conclusion survived, but the default output
#                 overstated precision by more than 4x.
#
# WHAT TO REPORT: the coefficients with Newey-West (or HC3) standard errors,
# not the default lm() output.
#
# LIMITATIONS -- what this analysis does NOT show:
#   - Causation: it's a same-day association. Oil and gasoline react to the
#     same news; neither necessarily drives the other.
#   - Imputation: interpolating holidays invents price moves on non-trading
#     days. Dropping those rows is a defensible alternative worth testing.
#   - Extreme events: two days in April 2020 were dropped (negative price),
#     and spring 2020 drives much of the serial correlation.
#   - Volatility: we corrected the standard errors for it but didn't model
#     it. ARCH/GARCH models do that directly.

# ==============================================================================
# END
# ==============================================================================
