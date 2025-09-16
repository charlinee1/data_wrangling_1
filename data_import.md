Data Import
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(haven)
```

\##Read some data read in the litters dataset

``` r
litters_df = read.csv("./data/FAS_litters.csv")
#update the name of some variables#
litters_df = janitor::clean_names(litters_df)
```

## take a look at the data

``` r
litters_df
```

    ##    group   litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ## 1   Con7             #85       19.7        34.7          20               3
    ## 2   Con7       #1/2/95/2         27          42          19               8
    ## 3   Con7   #5/5/3/83/3-3         26        41.4          19               6
    ## 4   Con7     #5/4/2/95/2       28.5        44.1          19               5
    ## 5   Con7     #4/2/95/3-3       <NA>        <NA>          20               6
    ## 6   Con7     #2/2/95/3-2       <NA>                      20               6
    ## 7   Con7 #1/5/3/83/3-3/2       <NA>                      20               9
    ## 8   Con8       #3/83/3-3       <NA>        <NA>          20               9
    ## 9   Con8         #2/95/3                   <NA>          20               8
    ## 10  Con8     #3/5/2/2/95       28.5        <NA>          20               8
    ## 11  Con8     #5/4/3/83/3         28        <NA>          19               9
    ## 12  Con8   #1/6/2/2/95-2       <NA>        <NA>          20               7
    ## 13  Con8 #3/5/3/83/3-3-2       <NA>        <NA>          20               8
    ## 14  Con8       #2/2/95/2       <NA>        <NA>          19               5
    ## 15  Con8   #3/6/2/2/95-3       <NA>        <NA>          20               7
    ## 16  Mod7             #59         17        33.4          19               8
    ## 17  Mod7            #103       21.4        42.1          19               9
    ## 18  Mod7       #1/82/3-2       <NA>        <NA>          19               6
    ## 19  Mod7       #3/83/3-2       <NA>        <NA>          19               8
    ## 20  Mod7       #2/95/2-2       <NA>        <NA>          20               7
    ## 21  Mod7       #3/82/3-2         28        45.9          20               5
    ## 22  Mod7       #4/2/95/2       23.5        <NA>          19               9
    ## 23  Mod7     #5/3/83/5-2       22.6          37          19               5
    ## 24  Mod7      #8/110/3-2          .           .          20               9
    ## 25  Mod7            #106       21.7        37.8          20               5
    ## 26  Mod7           #94/2       24.4        42.9          19               7
    ## 27  Mod7             #62       19.5        35.9          19               7
    ## 28  Low7           #84/2       24.3        40.8          20               8
    ## 29  Low7            #107       22.6        42.4          20               9
    ## 30  Low7           #85/2       22.2        38.5          20               8
    ## 31  Low7             #98       23.8        43.8          20               9
    ## 32  Low7            #102       22.6        43.3          20              11
    ## 33  Low7            #101       23.8        42.7          20               9
    ## 34  Low7            #111       25.5        44.6          20               3
    ## 35  Low7            #112       23.9        40.5          19               6
    ## 36  Mod8             #97       24.5        42.8          20               8
    ## 37  Mod8           #5/93       <NA>        41.1          20              11
    ## 38  Mod8         #5/93/2          .           .          19               8
    ## 39  Mod8       #7/82-3-2       26.9        43.2          20               7
    ## 40  Mod8      #7/110/3-2       27.5          46          19               8
    ## 41  Mod8         #2/95/2       28.5        44.5          20               9
    ## 42  Mod8           #82/4       33.4        52.7          20               8
    ## 43  Low8             #53       21.8        37.2          20               8
    ## 44  Low8             #79       25.4        43.8          19               8
    ## 45  Low8            #100         20        39.2          20               8
    ## 46  Low8           #4/84       21.8        35.2          20               4
    ## 47  Low8            #108       25.6        47.5          20               8
    ## 48  Low8             #99       23.5          39          20               6
    ## 49  Low8            #110       25.5        42.7          20               7
    ##    pups_dead_birth pups_survive
    ## 1                4            3
    ## 2                0            7
    ## 3                0            5
    ## 4                1            4
    ## 5                0            6
    ## 6                0            4
    ## 7                0            9
    ## 8                1            8
    ## 9                0            8
    ## 10               0            8
    ## 11               0            8
    ## 12               0            6
    ## 13               0            8
    ## 14               0            4
    ## 15               0            7
    ## 16               0            5
    ## 17               1            9
    ## 18               0            6
    ## 19               0            8
    ## 20               0            7
    ## 21               0            5
    ## 22               0            7
    ## 23               0            5
    ## 24               0            9
    ## 25               0            2
    ## 26               1            3
    ## 27               2            4
    ## 28               0            8
    ## 29               0            8
    ## 30               0            6
    ## 31               0            9
    ## 32               0            7
    ## 33               0            9
    ## 34               2            3
    ## 35               1            1
    ## 36               1            8
    ## 37               0            9
    ## 38               0            8
    ## 39               0            7
    ## 40               1            8
    ## 41               0            9
    ## 42               0            6
    ## 43               1            7
    ## 44               0            7
    ## 45               0            7
    ## 46               0            4
    ## 47               0            7
    ## 48               0            5
    ## 49               0            6

``` r
head(litters_df)
```

    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ## 1  Con7           #85       19.7        34.7          20               3
    ## 2  Con7     #1/2/95/2         27          42          19               8
    ## 3  Con7 #5/5/3/83/3-3         26        41.4          19               6
    ## 4  Con7   #5/4/2/95/2       28.5        44.1          19               5
    ## 5  Con7   #4/2/95/3-3       <NA>        <NA>          20               6
    ## 6  Con7   #2/2/95/3-2       <NA>                      20               6
    ##   pups_dead_birth pups_survive
    ## 1               4            3
    ## 2               0            7
    ## 3               0            5
    ## 4               1            4
    ## 5               0            6
    ## 6               0            4

``` r
tail(litters_df)
```

    ##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ## 44  Low8           #79       25.4        43.8          19               8
    ## 45  Low8          #100         20        39.2          20               8
    ## 46  Low8         #4/84       21.8        35.2          20               4
    ## 47  Low8          #108       25.6        47.5          20               8
    ## 48  Low8           #99       23.5          39          20               6
    ## 49  Low8          #110       25.5        42.7          20               7
    ##    pups_dead_birth pups_survive
    ## 44               0            7
    ## 45               0            7
    ## 46               0            4
    ## 47               0            7
    ## 48               0            5
    ## 49               0            6

``` r
skimr::skim(litters_df)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | litters_df |
| Number of rows                                   | 49         |
| Number of columns                                | 8          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 4          |
| numeric                                          | 4          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| group         |         0 |          1.00 |   4 |   4 |     0 |        6 |          0 |
| litter_number |         0 |          1.00 |   3 |  15 |     0 |       49 |          0 |
| gd0_weight    |        12 |          0.76 |   0 |   4 |     1 |       27 |          0 |
| gd18_weight   |        13 |          0.73 |   0 |   4 |     2 |       32 |          0 |

**Variable type: numeric**

| skim_variable   | n_missing | complete_rate |  mean |   sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:----------------|----------:|--------------:|------:|-----:|----:|----:|----:|----:|-----:|:------|
| gd_of_birth     |         0 |             1 | 19.65 | 0.48 |  19 |  19 |  20 |  20 |   20 | ▅▁▁▁▇ |
| pups_born_alive |         0 |             1 |  7.35 | 1.76 |   3 |   6 |   8 |   8 |   11 | ▁▃▂▇▁ |
| pups_dead_birth |         0 |             1 |  0.33 | 0.75 |   0 |   0 |   0 |   0 |    4 | ▇▂▁▁▁ |
| pups_survive    |         0 |             1 |  6.41 | 2.05 |   1 |   5 |   7 |   8 |    9 | ▁▃▂▇▇ |

\##options to read_csv

``` r
litters_df = 
    read_csv(file = "./data/FAS_litters.csv",
    skip = 10, col_names = FALSE)
```

    ## Rows: 40 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): X1, X2, X3, X4
    ## dbl (4): X5, X6, X7, X8
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

check out `?read_csv` for more information

## read excel file

``` r
library
```

    ## function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
    ##     logical.return = FALSE, warn.conflicts, quietly = FALSE, 
    ##     verbose = getOption("verbose"), mask.ok, exclude, include.only, 
    ##     attach.required = missing(include.only)) 
    ## {
    ##     conf.ctrl <- getOption("conflicts.policy")
    ##     if (is.character(conf.ctrl)) 
    ##         conf.ctrl <- switch(conf.ctrl, strict = list(error = TRUE, 
    ##             warn = FALSE), depends.ok = list(error = TRUE, generics.ok = TRUE, 
    ##             can.mask = c("base", "methods", "utils", "grDevices", 
    ##                 "graphics", "stats"), depends.ok = TRUE), warning(gettextf("unknown conflict policy: %s", 
    ##             sQuote(conf.ctrl)), call. = FALSE, domain = NA))
    ##     if (!is.list(conf.ctrl)) 
    ##         conf.ctrl <- NULL
    ##     stopOnConflict <- isTRUE(conf.ctrl$error)
    ##     if (missing(warn.conflicts)) 
    ##         warn.conflicts <- !isFALSE(conf.ctrl$warn)
    ##     if (!missing(include.only) && !missing(exclude)) 
    ##         stop("only one of 'include.only' and 'exclude' can be used", 
    ##             call. = FALSE)
    ##     testRversion <- function(pkgInfo, pkgname, pkgpath) {
    ##         if (is.null(built <- pkgInfo$Built)) 
    ##             stop(gettextf("package %s has not been installed properly\n", 
    ##                 sQuote(pkgname)), call. = FALSE, domain = NA)
    ##         R_version_built_under <- as.numeric_version(built$R)
    ##         if (R_version_built_under < "3.0.0") 
    ##             stop(gettextf("package %s was built before R 3.0.0: please re-install it", 
    ##                 sQuote(pkgname)), call. = FALSE, domain = NA)
    ##         current <- getRversion()
    ##         if (length(Rdeps <- pkgInfo$Rdepends2)) {
    ##             for (dep in Rdeps) if (length(dep) > 1L) {
    ##                 target <- dep$version
    ##                 res <- do.call(dep$op, if (is.character(target)) 
    ##                   list(as.numeric(R.version[["svn rev"]]), as.numeric(sub("^r", 
    ##                     "", target)))
    ##                 else list(current, as.numeric_version(target)))
    ##                 if (!res) 
    ##                   stop(gettextf("This is R %s, package %s needs %s %s", 
    ##                     current, sQuote(pkgname), dep$op, target), 
    ##                     call. = FALSE, domain = NA)
    ##             }
    ##         }
    ##         if (R_version_built_under > current) 
    ##             warning(gettextf("package %s was built under R version %s", 
    ##                 sQuote(pkgname), as.character(built$R)), call. = FALSE, 
    ##                 domain = NA)
    ##         platform <- built$Platform
    ##         r_arch <- .Platform$r_arch
    ##         if (.Platform$OS.type == "unix") {
    ##         }
    ##         else {
    ##             if (nzchar(platform) && !grepl("mingw", platform)) 
    ##                 stop(gettextf("package %s was built for %s", 
    ##                   sQuote(pkgname), platform), call. = FALSE, 
    ##                   domain = NA)
    ##         }
    ##         if (nzchar(r_arch) && file.exists(file.path(pkgpath, 
    ##             "libs")) && !file.exists(file.path(pkgpath, "libs", 
    ##             r_arch))) 
    ##             stop(gettextf("package %s is not installed for 'arch = %s'", 
    ##                 sQuote(pkgname), r_arch), call. = FALSE, domain = NA)
    ##     }
    ##     checkNoGenerics <- function(env, pkg) {
    ##         nenv <- env
    ##         ns <- .getNamespace(as.name(pkg))
    ##         if (!is.null(ns)) 
    ##             nenv <- asNamespace(ns)
    ##         if (exists(".noGenerics", envir = nenv, inherits = FALSE)) 
    ##             TRUE
    ##         else {
    ##             !any(startsWith(names(env), ".__T"))
    ##         }
    ##     }
    ##     checkConflicts <- function(package, pkgname, pkgpath, nogenerics, 
    ##         env) {
    ##         dont.mind <- c("last.dump", "last.warning", ".Last.value", 
    ##             ".Random.seed", ".Last.lib", ".onDetach", ".packageName", 
    ##             ".noGenerics", ".required", ".no_S3_generics", ".Depends", 
    ##             ".requireCachedGenerics")
    ##         sp <- search()
    ##         lib.pos <- which(sp == pkgname)
    ##         ob <- names(as.environment(lib.pos))
    ##         if (!nogenerics) {
    ##             these <- ob[startsWith(ob, ".__T__")]
    ##             gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
    ##             from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
    ##             gen <- gen[from != package]
    ##             ob <- ob[!(ob %in% gen)]
    ##         }
    ##         ipos <- seq_along(sp)[-c(lib.pos, match(c("Autoloads", 
    ##             "CheckExEnv"), sp, 0L))]
    ##         cpos <- NULL
    ##         conflicts <- vector("list", 0)
    ##         for (i in ipos) {
    ##             obj.same <- match(names(as.environment(i)), ob, nomatch = 0L)
    ##             if (any(obj.same > 0L)) {
    ##                 same <- ob[obj.same]
    ##                 same <- same[!(same %in% dont.mind)]
    ##                 Classobjs <- which(startsWith(same, ".__"))
    ##                 if (length(Classobjs)) 
    ##                   same <- same[-Classobjs]
    ##                 same.isFn <- function(where) vapply(same, exists, 
    ##                   NA, where = where, mode = "function", inherits = FALSE)
    ##                 same <- same[same.isFn(i) == same.isFn(lib.pos)]
    ##                 not.Ident <- function(ch, TRAFO = identity, ...) vapply(ch, 
    ##                   function(.) !identical(TRAFO(get(., i)), TRAFO(get(., 
    ##                     lib.pos)), ...), NA)
    ##                 if (length(same)) 
    ##                   same <- same[not.Ident(same)]
    ##                 if (length(same) && identical(sp[i], "package:base")) 
    ##                   same <- same[not.Ident(same, ignore.environment = TRUE)]
    ##                 if (length(same)) {
    ##                   conflicts[[sp[i]]] <- same
    ##                   cpos[sp[i]] <- i
    ##                 }
    ##             }
    ##         }
    ##         if (length(conflicts)) {
    ##             if (stopOnConflict) {
    ##                 emsg <- ""
    ##                 pkg <- names(conflicts)
    ##                 notOK <- vector("list", 0)
    ##                 for (i in seq_along(conflicts)) {
    ##                   pkgname <- sub("^package:", "", pkg[i])
    ##                   if (pkgname %in% canMaskEnv$canMask) 
    ##                     next
    ##                   same <- conflicts[[i]]
    ##                   if (is.list(mask.ok)) 
    ##                     myMaskOK <- mask.ok[[pkgname]]
    ##                   else myMaskOK <- mask.ok
    ##                   if (isTRUE(myMaskOK)) 
    ##                     same <- NULL
    ##                   else if (is.character(myMaskOK)) 
    ##                     same <- setdiff(same, myMaskOK)
    ##                   if (length(same)) {
    ##                     notOK[[pkg[i]]] <- same
    ##                     msg <- .maskedMsg(sort(same), pkg = sQuote(pkg[i]), 
    ##                       by = cpos[i] < lib.pos)
    ##                     emsg <- paste(emsg, msg, sep = "\n")
    ##                   }
    ##                 }
    ##                 if (length(notOK)) {
    ##                   msg <- gettextf("Conflicts attaching package %s:\n%s", 
    ##                     sQuote(package), emsg)
    ##                   stop(errorCondition(msg, package = package, 
    ##                     conflicts = conflicts, class = "packageConflictError"))
    ##                 }
    ##             }
    ##             if (warn.conflicts) {
    ##                 packageStartupMessage(gettextf("\nAttaching package: %s\n", 
    ##                   sQuote(package)), domain = NA)
    ##                 pkg <- names(conflicts)
    ##                 for (i in seq_along(conflicts)) {
    ##                   msg <- .maskedMsg(sort(conflicts[[i]]), pkg = sQuote(pkg[i]), 
    ##                     by = cpos[i] < lib.pos)
    ##                   packageStartupMessage(msg, domain = NA)
    ##                 }
    ##             }
    ##         }
    ##     }
    ##     if (verbose && quietly) 
    ##         message("'verbose' and 'quietly' are both true; being verbose then ..")
    ##     if (!missing(package)) {
    ##         if (is.null(lib.loc)) 
    ##             lib.loc <- .libPaths()
    ##         lib.loc <- lib.loc[dir.exists(lib.loc)]
    ##         if (!character.only) 
    ##             package <- as.character(substitute(package))
    ##         if (length(package) != 1L) 
    ##             stop(gettextf("'%s' must be of length 1", "package"), 
    ##                 domain = NA)
    ##         if (is.na(package) || (package == "")) 
    ##             stop("invalid package name")
    ##         pkgname <- paste0("package:", package)
    ##         newpackage <- is.na(match(pkgname, search()))
    ##         if (newpackage) {
    ##             pkgpath <- find.package(package, lib.loc, quiet = TRUE, 
    ##                 verbose = verbose)
    ##             if (length(pkgpath) == 0L) {
    ##                 if (length(lib.loc) && !logical.return) 
    ##                   stop(packageNotFoundError(package, lib.loc, 
    ##                     sys.call()))
    ##                 txt <- if (length(lib.loc)) 
    ##                   gettextf("there is no package called %s", sQuote(package))
    ##                 else gettext("no library trees found in 'lib.loc'")
    ##                 if (logical.return) {
    ##                   if (!quietly) 
    ##                     warning(txt, domain = NA)
    ##                   return(FALSE)
    ##                 }
    ##                 else stop(txt, domain = NA)
    ##             }
    ##             which.lib.loc <- normalizePath(dirname(pkgpath), 
    ##                 "/", TRUE)
    ##             pfile <- system.file("Meta", "package.rds", package = package, 
    ##                 lib.loc = which.lib.loc)
    ##             if (!nzchar(pfile)) 
    ##                 stop(gettextf("%s is not a valid installed package", 
    ##                   sQuote(package)), domain = NA)
    ##             pkgInfo <- readRDS(pfile)
    ##             testRversion(pkgInfo, package, pkgpath)
    ##             if (is.character(pos)) {
    ##                 npos <- match(pos, search())
    ##                 if (is.na(npos)) {
    ##                   warning(gettextf("%s not found on search path, using pos = 2", 
    ##                     sQuote(pos)), domain = NA)
    ##                   pos <- 2
    ##                 }
    ##                 else pos <- npos
    ##             }
    ##             deps <- unique(names(pkgInfo$Depends))
    ##             depsOK <- isTRUE(conf.ctrl$depends.ok)
    ##             if (depsOK) {
    ##                 canMaskEnv <- dynGet("__library_can_mask__", 
    ##                   NULL)
    ##                 if (is.null(canMaskEnv)) {
    ##                   canMaskEnv <- new.env()
    ##                   canMaskEnv$canMask <- union("base", conf.ctrl$can.mask)
    ##                   "__library_can_mask__" <- canMaskEnv
    ##                 }
    ##                 canMaskEnv$canMask <- unique(c(package, deps, 
    ##                   canMaskEnv$canMask))
    ##             }
    ##             else canMaskEnv <- NULL
    ##             if (attach.required) 
    ##                 .getRequiredPackages2(pkgInfo, quietly = quietly, 
    ##                   lib.loc = c(lib.loc, .libPaths()))
    ##             cr <- conflictRules(package)
    ##             if (missing(mask.ok)) 
    ##                 mask.ok <- cr$mask.ok
    ##             if (missing(exclude)) 
    ##                 exclude <- cr$exclude
    ##             if (isNamespaceLoaded(package)) {
    ##                 newversion <- as.numeric_version(pkgInfo$DESCRIPTION["Version"])
    ##                 oldversion <- as.numeric_version(getNamespaceVersion(package))
    ##                 if (newversion != oldversion) {
    ##                   tryCatch(unloadNamespace(package), error = function(e) {
    ##                     P <- if (!is.null(cc <- conditionCall(e))) 
    ##                       paste("Error in", deparse(cc)[1L], ": ")
    ##                     else "Error : "
    ##                     stop(gettextf("Package %s version %s cannot be unloaded:\n %s", 
    ##                       sQuote(package), oldversion, paste0(P, 
    ##                         conditionMessage(e), "\n")), domain = NA)
    ##                   })
    ##                 }
    ##             }
    ##             tt <- tryCatch({
    ##                 attr(package, "LibPath") <- which.lib.loc
    ##                 ns <- loadNamespace(package, lib.loc)
    ##                 env <- attachNamespace(ns, pos = pos, deps, exclude, 
    ##                   include.only)
    ##             }, error = function(e) {
    ##                 P <- if (!is.null(cc <- conditionCall(e))) 
    ##                   paste(" in", deparse(cc)[1L])
    ##                 else ""
    ##                 msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
    ##                   sQuote(package), P, conditionMessage(e))
    ##                 if (logical.return && !quietly) 
    ##                   message(paste("Error:", msg), domain = NA)
    ##                 else stop(msg, call. = FALSE, domain = NA)
    ##             })
    ##             if (logical.return && is.null(tt)) 
    ##                 return(FALSE)
    ##             attr(package, "LibPath") <- NULL
    ##             {
    ##                 on.exit(detach(pos = pos))
    ##                 nogenerics <- !.isMethodsDispatchOn() || checkNoGenerics(env, 
    ##                   package)
    ##                 if (isFALSE(conf.ctrl$generics.ok) || (stopOnConflict && 
    ##                   !isTRUE(conf.ctrl$generics.ok))) 
    ##                   nogenerics <- TRUE
    ##                 if (stopOnConflict || (warn.conflicts && !exists(".conflicts.OK", 
    ##                   envir = env, inherits = FALSE))) 
    ##                   checkConflicts(package, pkgname, pkgpath, nogenerics, 
    ##                     ns)
    ##                 on.exit()
    ##                 if (logical.return) 
    ##                   return(TRUE)
    ##                 else return(invisible(.packages()))
    ##             }
    ##         }
    ##         if (verbose && !newpackage) 
    ##             warning(gettextf("package %s already present in search()", 
    ##                 sQuote(package)), domain = NA)
    ##     }
    ##     else if (!missing(help)) {
    ##         if (!character.only) 
    ##             help <- as.character(substitute(help))
    ##         pkgName <- help[1L]
    ##         pkgPath <- find.package(pkgName, lib.loc, verbose = verbose)
    ##         docFiles <- c(file.path(pkgPath, "Meta", "package.rds"), 
    ##             file.path(pkgPath, "INDEX"))
    ##         if (file.exists(vignetteIndexRDS <- file.path(pkgPath, 
    ##             "Meta", "vignette.rds"))) 
    ##             docFiles <- c(docFiles, vignetteIndexRDS)
    ##         pkgInfo <- vector("list", 3L)
    ##         readDocFile <- function(f) {
    ##             if (basename(f) %in% "package.rds") {
    ##                 txt <- readRDS(f)$DESCRIPTION
    ##                 if ("Encoding" %in% names(txt)) {
    ##                   to <- if (Sys.getlocale("LC_CTYPE") == "C") 
    ##                     "ASCII//TRANSLIT"
    ##                   else ""
    ##                   tmp <- try(iconv(txt, from = txt["Encoding"], 
    ##                     to = to))
    ##                   if (!inherits(tmp, "try-error")) 
    ##                     txt <- tmp
    ##                   else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible", 
    ##                     call. = FALSE)
    ##                 }
    ##                 nm <- paste0(names(txt), ":")
    ##                 formatDL(nm, txt, indent = max(nchar(nm, "w")) + 
    ##                   3L)
    ##             }
    ##             else if (basename(f) %in% "vignette.rds") {
    ##                 txt <- readRDS(f)
    ##                 if (is.data.frame(txt) && nrow(txt)) 
    ##                   cbind(basename(gsub("\\.[[:alpha:]]+$", "", 
    ##                     txt$File)), paste(txt$Title, paste0(rep.int("(source", 
    ##                     NROW(txt)), ifelse(nzchar(txt$PDF), ", pdf", 
    ##                     ""), ")")))
    ##                 else NULL
    ##             }
    ##             else readLines(f)
    ##         }
    ##         for (i in which(file.exists(docFiles))) pkgInfo[[i]] <- readDocFile(docFiles[i])
    ##         y <- list(name = pkgName, path = pkgPath, info = pkgInfo)
    ##         class(y) <- "packageInfo"
    ##         return(y)
    ##     }
    ##     else {
    ##         if (is.null(lib.loc)) 
    ##             lib.loc <- .libPaths()
    ##         db <- matrix(character(), nrow = 0L, ncol = 3L)
    ##         nopkgs <- character()
    ##         for (lib in lib.loc) {
    ##             a <- .packages(all.available = TRUE, lib.loc = lib)
    ##             for (i in sort(a)) {
    ##                 file <- system.file("Meta", "package.rds", package = i, 
    ##                   lib.loc = lib)
    ##                 title <- if (nzchar(file)) {
    ##                   txt <- readRDS(file)
    ##                   if (is.list(txt)) 
    ##                     txt <- txt$DESCRIPTION
    ##                   if ("Encoding" %in% names(txt)) {
    ##                     to <- if (Sys.getlocale("LC_CTYPE") == "C") 
    ##                       "ASCII//TRANSLIT"
    ##                     else ""
    ##                     tmp <- try(iconv(txt, txt["Encoding"], to, 
    ##                       "?"))
    ##                     if (!inherits(tmp, "try-error")) 
    ##                       txt <- tmp
    ##                     else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible", 
    ##                       call. = FALSE)
    ##                   }
    ##                   txt["Title"]
    ##                 }
    ##                 else NA
    ##                 if (is.na(title)) 
    ##                   title <- " ** No title available ** "
    ##                 db <- rbind(db, cbind(i, lib, title))
    ##             }
    ##             if (length(a) == 0L) 
    ##                 nopkgs <- c(nopkgs, lib)
    ##         }
    ##         dimnames(db) <- list(NULL, c("Package", "LibPath", "Title"))
    ##         if (length(nopkgs) && !missing(lib.loc)) {
    ##             pkglist <- paste(sQuote(nopkgs), collapse = ", ")
    ##             msg <- sprintf(ngettext(length(nopkgs), "library %s contains no packages", 
    ##                 "libraries %s contain no packages"), pkglist)
    ##             warning(msg, domain = NA)
    ##         }
    ##         y <- list(header = NULL, results = db, footer = NULL)
    ##         class(y) <- "libraryIQR"
    ##         return(y)
    ##     }
    ##     if (logical.return) 
    ##         TRUE
    ##     else invisible(.packages())
    ## }
    ## <bytecode: 0x00000235fc25abd0>
    ## <environment: namespace:base>

``` r
mlb_df = read_excel("./data/mlb11.xlsx", rang= "A1:f7")
mlb_df
```

    ## # A tibble: 6 × 6
    ##   team                 runs at_bats  hits homeruns bat_avg
    ##   <chr>               <dbl>   <dbl> <dbl>    <dbl>   <dbl>
    ## 1 Texas Rangers         855    5659  1599      210   0.283
    ## 2 Boston Red Sox        875    5710  1600      203   0.28 
    ## 3 Detroit Tigers        787    5563  1540      169   0.277
    ## 4 Kansas City Royals    730    5672  1560      129   0.275
    ## 5 St. Louis Cardinals   762    5532  1513      162   0.273
    ## 6 New York Mets         718    5600  1477      108   0.264

## read sas file

``` r
pulse_df = read_sas("./data/public_pulse_data.sas7bdat")
pulse_df
```

    ## # A tibble: 1,087 × 7
    ##       ID   age Sex    BDIScore_BL BDIScore_01m BDIScore_06m BDIScore_12m
    ##    <dbl> <dbl> <chr>        <dbl>        <dbl>        <dbl>        <dbl>
    ##  1 10003  48.0 male             7            1            2            0
    ##  2 10015  72.5 male             6           NA           NA           NA
    ##  3 10022  58.5 male            14            3            8           NA
    ##  4 10026  72.7 male            20            6           18           16
    ##  5 10035  60.4 male             4            0            1            2
    ##  6 10050  84.7 male             2           10           12            8
    ##  7 10078  31.3 male             4            0           NA           NA
    ##  8 10088  56.9 male             5           NA            0            2
    ##  9 10091  76.0 male             0            3            4            0
    ## 10 10092  74.2 female          10            2           11            6
    ## # ℹ 1,077 more rows
