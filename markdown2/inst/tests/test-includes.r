#' Returns a copy of a function with particular functions masked.
#'
#' This returns the input function, except that any arguments provided in `...`
#' are masked by the values provided in `...`.
#'
#' @param f function in which we want to mask things.
#' @param ... a set of name=value pairs of what we wish to mask and with what
#' @return the function `f` such that whenever it accesses a variable that was
#'         provided in `...`, it gets the masked version instead.
#' @examples
#' # Suppose my package has a function:
#' myFunc <- function (x) {
#'     if (!require(grid)) {
#'         stop('grid is not installed')
#'     }
#'     # ...
#' }
#'
#' # I wish to test that if `grid` is not installed, the error is thrown. i.e.:
#'
#' expect_that(myFunc(1), throws_error('grid is not installed'))
#'
#' # But what if grid *is* installed on my testing computer?
#' # `require` will return TRUE, so the test will fail.
#' #
#' # Instead, I can override `require` to return FALSE (so that appears that
#' # grid is not installed) for the duration of one function call:
#' test.myFunc <- overrideIn(myFunc, require=function(...) FALSE)
#' expect_that(test.myFunc(1), throws_error('grid is not installed'))
#'
#' # This will definitely pass regardless of whether my testing machine has
#' # grid installed or not because when `myFunc` is called in `test.myFunc`,
#' # require(...) always returns FALSE.
#'
#' @details
#' This is useful when you wish to temporarily override (say) a function that is
#' called by `f` for the purposes of testing.
overrideIn <- function(f, ...) {
    overrides <- list(...)
    nms <- names(overrides)[names(overrides) != '']
    # stub out the functions
    for (nm in nms) {
        assign(nm, overrides[[nm]])
    }

    # copy over f
    fff <- function () {}
    formals(fff) <- formals(f)
    body(fff) <- body(f)
    return(fff)
}

# FUNCTIONS SPECIFIC TO THIS PACKAGE:
#' Asks whether the given variable (name as string) was exported from the markdown2 package.
#'
#' This is just `exists_as` with 'package: markdown2' as the `where` argument.
#' @param package the package we want to check (a string).
#' @inheritParams exists_as
#' @examples
#' # see if there is a 'plot' function in the 'graphics' package.
#' expect_that('plot', is_exported('function', 'graphics')) # passes.
#' @family expectations
#' @seealso \code{\link{exists_as}}, which this wraps around.
is_exported <- function(mode="any", package='markdown2') {
    exists_as(mode=mode, where=paste('package', package, sep=':'))
}

# ----------------------------------------------------------------------------
# Generic.

#' negates a test (doesn't change the failure message though)
#' @param f test to negate (e.g. equals, matches, shows_message, ...)
#' @author richfitz
#' @seealso \link{https://gist.github.com/richfitz/5056365}
#' @examples
#' expect_that(1, not(equals(2))) # passes
#' @family expectations
not <- function(f) {
    function(...) {
        res <- f(...)
        res$passed <- !res$passed
        # poor attempt at fixing message
        res$message <- sprintf('NOT(%s)', res$message)
        res
    }
}

#' test that the given path exists and is a directory
#' @family expectations
is_directory <- function() {
    function(pth) {
        i <- file.info(pth)
        isDir <- any(i[, 'isdir'])
        exists <- any(!is.na(i[, 'isdir']))
        expectation(isTRUE(isDir && exists),
                    sprintf("The path '%s' %s",
                            pth,
                            ifelse(exists,
                                   "exists but is not a directory",
                                   "does not exist")))
    }
}

#' test that the given path exists
#' @family expectations
file_exists <- function() {
    function (pth) {
        expectation(file.exists(pth), sprintf("The path '%s' does not exist", pth))
    }
}

#' test that the given path exists and is a file (i.e. exists and not a directory)
#' @family expectations
is_file <- function() {
    function(pth) {
        i <- file.info(pth)
        isFile <- any(!i[, 'isdir'])
        exists <- any(!is.na(i[, 'isdir']))
        expectation(isTRUE(isFile && exists),
                    sprintf("The path '%s' %s",
                            pth,
                            ifelse(exists,
                                   "exists but is not a file (perhaps a directory)",
                                   "does not exist")))
    }
}

#' Tests that the given variable (name as a string) exists in the given mode.
#'
#' For more information on the parameters see \code{\link[base]{exists}}.
#' Also, check out \code{\link{is_exported}} which asks whether the variable
#'  is exported from a particular package.
#' @param mode  the mode of the object to search for, e.g. "function" or "any".
#' @param where where to search, by default the parent frame.
#'              You can also use 'package:packagename' to see if something exists
#'              in a package.
#' @seealso \code{\link[base]{exists}} which is what this wraps around.
#' @examples
#' x <- function () {}
#' expect_that('x', exists_as('function')) # passes
#'
#' expect_that('plot', exists_as('function', 'package:graphics')) # passes
#' @family expectations
exists_as <- function (mode="any", where=parent.frame()) {
    function (.x) {
        # something might be off with the environment...
        expectation(exists(.x, mode=mode, where=where), "does not exist")
    }
}

#' Tests for null-ness.
#' @examples
#' expect_that(NULL, is_null()) # passes
#' @family expectations
is_null <- function() {
    function(x) {
        expectation(is.null(x), "is not null")
    }
}

#' Expectation that something is NaN
#' @examples
#' expect_that(log(-1), is_NaN()) # passes
#' @family expectations
is_NaN <- function() {
    function(x) {
        expectation(is.nan(x), "is not NaN")
    }
}

#' Expectation that something is printed to console with `cat` (synonym for prints_text).
#'
#' Just a synonym for \code{\link[testthat]{prints_text}}.
#' @family expectations
#' @seealso \code{\link[testthat]{prints_text}}
#' @seealso \code{\link[testthat]{shows_message}}
#' @seealso \code{\link[testthat]{gives_warning}}
#' @seealso \code{\link[testthat]{throws_error}}
#' @examples
#' expect_that(cat('foo bar'), cats()) # passes
#' expect_that(cat('foo bar'), cats('foo')) # passes
cats <- prints_text

.topic <- NULL
#' Acts like a context, allows nice code indenting and topics.
#'
#' @details
#' This is essentially a call to `context(topic)` and then the tests.
#' All it does is allow me to indent the tests relative to their context which
#' I prefer when reviewing tests (for visual grouping of tests to their context).
#'
#' Also, if you use \code{\link{test}} rather than `test_that` within the `describe`,
#' the topic for the context is automatically prepended onto the test text.
#'
#' Use of `describe` also provides a convenient way to skip all tests for that
#' topic by using \code{\link{xdescribe}}.
#'
#' See the examples.
#'
#' @param topic string describing the topic of these tests
#' @param tests group of test_that expressions.
#'
#' @seealso \code{\link[testthat]{context}}
#' @seealso \code{\link{test}}
#' @examples
#' # Usually I'd write:
#'
#' context('myFunction')
#' test_that('myFunction exists', {
#'     # ...
#' })
#' test_that('myFunction does this', {
#'     # ...
#' })
#'
#' # Using `describe` lets me indent the tests to show they belong to 'myFunction':
#'
#' describe('myFunction', {
#'     test_that('myFunction exists', {
#'        # ...
#'     })
#'     test_that('myFunction does this', {
#'        # ...
#'     })
#'     # ...
#' })
#'
#' # Furthermore if I use `test` instead of test_that I can
#' # drop the preceding 'myFunction':
#'
#' describe('myFunction', {
#'     test_that('exists', {
#'        # ...
#'     })
#'     test_that('does this', {
#'        # ...
#'     })
#'     # ...
#' })
# TODO: put into a private environment?
describe <- function(topic, tests) {
    .topic <<- topic
    context(topic)
    tests
}

#' Used within `describe`, prepends the topic to the test message.
#'
#' @param desc test name, passed into test_that
#' @param code the tests, passed into test_that
#' @param times the number of times to repeat this test (default 1)
#'
#' @details
#' Use this within `describe` and the describe topic will be prepended onto the
#' test descriptions. This allows for greater brevity in the test descriptions
#' while not being confusing if a test fails.
#'
#' See the examples.
#'
#' @examples
#' # in the examle below the test descriptions will become 'myFunction exists'
#' # and 'myFunction does this' (i.e. the 'myFunction' from `describe` is
#' # automatically prepended to the test descriptions).
#' describe('myFunction', {
#'     test('exists', {
#'        # ...
#'     })
#'     test('does this', {
#'        # ... repeat this test 3 times
#'     }, times=3)
#'     # ...
#' })
#'
#' @seealso \code{\link[testthat]{test_that}}
#' @seealso \code{\link{describe}}
test <- function(desc, code, times=1) {
    cd <- substitute(code)
    pf <- parent.frame()

    if (is.null(.topic)) {
        test_that(desc, {
                  for (i in 1:times) {
                      eval(cd, pf)
                  }})
    } else {
        test_that(sprintf('%s %s', .topic, desc), {
                  for (i in 1:times) {
                      eval(cd, pf)
                  }})
    }
}

#' Convenient ways to comment out expectations, tests or groups of them.
#'
#' These functions all do nothing, so if you wish to skip a group of tests in
#' a `test_that`, put an 'x' in front of it.
#' @param ... parameters to \code{\link{describe}}, \code{\link{test}},
#'            \code{\link[testthat]{test_that}}, \code{\link[testthat]{expect_that}},
#' @rdname skipping-tests
#' @name Skipping Tests
#' @aliases xdescribe xtest xtest_that xexpect_that
xdescribe <- function (...) {}
xtest <- function (...) {}
xtest_that <- function (...) {}
xexpect_that <- function (...) {}
