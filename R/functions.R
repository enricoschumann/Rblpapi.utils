.connect <- function() {
    if (inherits(try(Rblpapi::defaultConnection(),
                     silent = TRUE), "try-error"))
        Rblpapi::blpConnect()
    invisible(NULL)
}

price_unit <- function(ticker, currency = "CHF") {

    .connect()
    ans <- character(length(ticker))
    names(ans) <- ticker

    ii <- grepl(" equity$", ticker, ignore.case = TRUE)
    if (any(ii)) {
        bbg <- bdp(ticker[ii], "CRNCY")
        ans[row.names(bbg)] <- bbg[,1]
    }

    ii <- grepl("^[a-z][a-z][a-z][a-z][a-z][a-z] curncy$", ticker, ignore.case = TRUE)
    if (any(ii)) {
        bbg <- bdp(ticker[ii], "CRNCY")
        ans[row.names(bbg)] <- bbg[,1]
    }

    ii <- grepl("^[a-z][a-z][a-z] curncy$", ticker, ignore.case = TRUE)
    ans[ticker[ii]] <- substr(ticker[ii], 1, 3)

    isna <- ans == ""
    if (!identical(currency, FALSE)) {
        ans <- paste0(ans, currency, " Curncy")
        names(ans) <- ticker
    }
    ans[isna] <- NA
    ans
}


## If Pricing Currency             then Market Currency
## GBp (BRITISH PENCE)             GBP (BRITISH POUND)
## ZAr (S. AFR. CENTS)             ZAR (SOUTH AFRICAN RAND)
## IEp (Irish Pence)               IEP (IRISH PUNT)
## ILs (Israeli Agorot)            ILS (ISRAELI SHEKEL)
## ZWd (Zimbabwe Cents)            ZWD (ZIMBABWE DOLLAR)
## BWp (Botswana Thebe)            BWP (BOTSWANA PULA)
## KWd (KUWAIT FILS)               KWD (KUWAITI DINAR)
## SZl (Swaziland cents)           SZL (SWAZILAND LILANGENI)
## MWk (MALAWI TAMBALA)            MWK (MALAWI KWACHA)

.specials  <- structure(list(Symbol = c("ZAr", "GBp"),
                             Divisor = c(100L, 100L)),
                        .Names = c("Symbol", "Divisor"),
                        class = "data.frame",
                        row.names = c(NA, -2L))

hist_series <- function(ticker,
                        start, end = Sys.Date() - 1,
                        field = "PX_LAST",
                        adjust.return = NULL,
                        return.class = "zoo") {

    oticker <- ticker <- trimws(ticker)
    nt <- length(ticker)

    if (grepl("^[12][0-9][0-9][0-9]$", trimws(as.character(start))))
        start <- paste0(start, "-1-1")
    if (grepl("^[12][0-9][0-9][0-9]$", trimws(as.character(end))))
        end <- paste0(end, "-12-31")

    start <- as.Date(start)
    end <- as.Date(end)

    ## SPECIAL CASES
    ##
    ## ALL tickers are like 'ABCABC Curncy' (i.e. base and
    ## counter currency are the same).
    ##    /or/
    ## ALL tickers are like 'ABC Curncy' (i.e. cash).
    ##
    ## ==> create matrix of 1s and exit
    if (all(substr(oticker, 1, 3) == substr(oticker, 4, 6)) || 
        all(grepl("^[a-z][a-z][a-z] Curncy$",
                  oticker, ignore.case = TRUE))) {
        ans <- array(1, dim = c(2, nt))
        colnames(ans) <- oticker
        if (return.class == "zoo")
            return(zoo(ans, c(start, end)))
        else
            stop("only zoo is supported as a return.class")
    }

    .connect()

    fx <- grepl("[a-z]+ curncy$", ticker, ignore.case = TRUE)
    fx0 <- ticker[fx]
    M <- rep(1, length(fx0))
    names(M) <- ticker[fx]
    if (any(fx)) {
        fx1 <- substr(fx0, 1, 3)
        m <- match(fx1, .specials[["Symbol"]], nomatch = 0)
        M[m > 0] <- 1/.specials[["Divisor"]][m]

        fx2 <- substr(fx0, 4, 6)
        m <- match(fx2, .specials[["Symbol"]], nomatch = 0)
        M[m > 0] <- M[m > 0] * .specials[["Divisor"]][m]

        PX_POS_MULT_FACTOR <- bdp(unique(tolower(fx0)), "PX_POS_MULT_FACTOR")
        px_mult <- PX_POS_MULT_FACTOR[[1]]
        names(px_mult) <- row.names(PX_POS_MULT_FACTOR)
        px_mult[is.na(px_mult)] <- 1
        if (any(px_mult != 1)) {
            mm <- match(tolower(fx0), names(px_mult), nomatch = 0)
            M[mm>0] <- M[mm>0] * px_mult[mm]
        }
    }
    ticker <- tolower(ticker)

    if (is.null(adjust.return))
        NULL
    else if (tolower(adjust.return) ==  "net")
        field <- "TOT_RETURN_INDEX_NET_DVDS"
    else if (tolower(adjust.return) ==  "gross")
        field <- "TOT_RETURN_INDEX_GROSS_DVDS"
    else
        stop(sQuote("adjust.return"), " may be: NULL, net, gross")

    tmp <- bdh(unique(ticker), fields = field,
               start.date = start, end.date = end)
    if (length(unique(ticker)) == 1L) {
        tmp <- list(tmp)
        names(tmp) <- unique(ticker)
    }
    dt <- sort(unique(unname(unlist(lapply(tmp, `[[`, 1)))))
    class(dt) <- "Date"
    ans <- array(NA, dim = c(length(dt), nt))
    for (i in seq_len(nt)) {
        tt <- tmp[[ticker[i]]]
        ii <- fmatch(tt[[1]], dt, nomatch = 0L)
        if (!is.na(M[oticker[i]]))
            ans[ii, i] <- (tt[[2]][ii > 0])*M[oticker[i]]
        else
            ans[ii, i] <- tt[[2]][ii > 0]
    }
    colnames(ans) <- oticker

    ## any ticker 'ABCABC Curncy' (i.e. base and
    ## counter currency are the same) gets a price of 1
    ans[ , substr(oticker, 1,3) == substr(oticker, 4, 6) &
           grepl(" Curncy$", oticker, ignore.case = TRUE)] <- 1

    ## any ticker 'ABC Curncy' (i.e. cash)
    ## gets a price of 1
    ans[ , grepl("^[a-z][a-z][a-z] Curncy$", oticker,
                 ignore.case = TRUE)] <- 1

    if (return.class == "zoo") {
        ans <- zoo(ans, dt)
    } else
        stop("only zoo is supported as a return.class")
    ans
}

port <- function(portfolio, when = NULL) {

    .connect()

    if (!is.null(when))
        .NotYetUsed("when")

    p <- getPortfolio(portfolio, "PORTFOLIO_DATA")

    cash <- grepl("^[a-z][a-z][a-z] curncy$", p$Security,
                  ignore.case = TRUE)
    p$Position[cash] <- 1000*p$Position[cash]
    p
}

hist_id <- function(ticker, when = Sys.Date()) {

    .connect()

    when <- format(when, "%Y%m%d")
    h.id <- bdp(ticker,
                "HISTORICAL_ID_POINT_TIME",
                overrides = c(HISTORICAL_ID_TM_RANGE_START_DT = when))
    ans <- trimws(h.id[[1]])
    names(ans) <- ticker
    ans[ans == ""] <- NA
    ans
}

index_weights <- function(ticker,
                          when = Sys.Date(),
                          strict = FALSE) {

    .connect()

    when <- format(as.Date(when), "%Y%m%d")
    names(when) <- if (strict)
                       "END_DATE_OVERRIDE"
                   else
                       "END_DT"

    bds(ticker, "INDX_MWEIGHT_HIST",
        overrides = when)

}

revenue_breakdown <- function(ticker,
                              by = c("region", "product"),
                              hierarchy.level = 1) {


    by <- by[1L]
    overrides <- c(NUMBER_OF_PERIODS  = -1,
                   PG_HIERARCHY_LEVEL = hierarchy.level)
    if (by == "region")
        overrides <- c(overrides,
                       PRODUCT_GEO_OVERRIDE = "G")
    else if (by == "product")
        overrides <- c(overrides,
                       PRODUCT_GEO_OVERRIDE = "P")
    else
       stop("unknown 'by'")

    .connect()
    bds(ticker, "PG_REVENUE", overrides = overrides)
}

vwap <- function(ticker, start, end,
                 field = "EQY_WEIGHTED_AVG_PX",
                 ...) {

    .connect()

    start <- as.POSIXct(start)
    end   <- as.POSIXct(end)

    tmp <- bdp(ticker, field,
               overrides = c(VWAP_START_TIME = format(start, "%H:%M"),
                             VWAP_END_TIME   = format(end, "%H:%M"),
                             VWAP_START_DT   = format(start, "%Y%m%d"),
                             VWAP_END_DT = format(start, "%Y%m%d")),
               ...)
    ans <- tmp[[1L]]
    names(ans) <- row.names(tmp)
    ans
}
