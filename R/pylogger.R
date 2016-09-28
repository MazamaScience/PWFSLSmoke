#' @name logger.setup
#' @export
#' @title Set Up Python-Style Logging
#' @param traceLog file name or full path where \code{logger.trace()} messages will be sent
#' @param debugLog file name or full path where \code{logger.debug()} messages will be sent
#' @param infoLog file name or full path where \code{logger.info()} messages will be sent
#' @param warnLog file name or full path where \code{logger.warn()} messages will be sent
#' @param errorLog file name or full path where \code{logger.error()} messages will be sent
#' @param fatalLog file name or full path where \code{logger.fatal()} messages will be sent
#' @return No return value.
#' @description Good logging allows package developers and users to create log files at different
#' levels to track and debug lengthy or complex calculations. "Python-style" logging is intended
#' to suggest that users should set up multiple log files for different log severities 
#' so that the \code{errorLog} will contain only log messages at or above the \code{ERROR} level while
#' a \code{debugLog} will contain log messages at the \code{DEBUG} level as well as all higher
#' levels.
#' 
#' Python-style log files are set up with \code{logger.setup()}. Logs can be set up for any
#' combination of log levels. Accepting the default \code{NULL} setting for any log file
#' simply means that log file will not be created.
#' 
#' Python-style logging requires the use of \code{logger.debug()} style logging statements as seen
#' in the example below.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(debugLog='debug.log', infoLog='info.log', errorLog='error.log')
#' 
#' # But allow lot statements at all levels within the code
#' logger.trace('trace statement #%d', 1)
#' logger.debug('debug statement')
#' logger.info('info statement %s %s', "with", "arguments")
#' logger.warn('warn statement %s', "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error('error message: %s', geterrmessage())
#' logger.fatal('fatal statement %s', "THE END")
#' }
#' @seealso \code{\link{logger.trace}} \code{\link{logger.debug}}  \code{\link{logger.info}}
#' \code{\link{logger.warn}} \code{\link{logger.error}} \code{\link{logger.fatal}}

# Set up logging namespaces
logger.setup <- function(traceLog=NULL,
                         debugLog=NULL,
                         infoLog=NULL,
                         warnLog=NULL,
                         errorLog=NULL,
                         fatalLog=NULL) {
  
  if ( ! 'futile.logger' %in% loadedNamespaces() ) requireNamespace('futile.logger', quietly=TRUE)
  
  # By default, the console receives only FATAL messages.
  futile.logger::flog.threshold(futile.logger::FATAL)
  
  # Set up TRACE logging
  if ( is.null(traceLog) ) {
    invisible( futile.logger::flog.logger("trace", futile.logger::TRACE, appender.null()) )
  } else {
    if ( file.exists(traceLog) ) result <- file.remove(traceLog)
    invisible( futile.logger::flog.logger("trace", futile.logger::TRACE, futile.logger::appender.file(traceLog)) )
  }
  
  # Set up DEBUG logging
  if ( is.null(debugLog) ) {
    invisible( futile.logger::flog.logger("debug", futile.logger::DEBUG, appender.null()) )
  } else {
    if ( file.exists(debugLog) ) result <- file.remove(debugLog)
    invisible( futile.logger::flog.logger("debug", futile.logger::DEBUG, futile.logger::appender.file(debugLog)) )
  }
  
  # Set up INFO logging
  if ( is.null(infoLog) ) {
    invisible( futile.logger::flog.logger("info", futile.logger::INFO, appender.null()) )
  } else {
    if ( file.exists(infoLog) ) result <- file.remove(infoLog)
    invisible( futile.logger::flog.logger("info", futile.logger::INFO, futile.logger::appender.file(infoLog)) )
  }
  
  # Set up WARN logging
  if ( is.null(warnLog) ) {
    invisible( futile.logger::flog.logger("warn", futile.logger::WARN, appender.null()) )
  } else {
    if ( file.exists(warnLog) ) result <- file.remove(warnLog)
    invisible( futile.logger::flog.logger("warn", futile.logger::WARN, futile.logger::appender.file(warnLog)) )
  }
  
  # Set up ERROR logging
  if ( is.null(errorLog) ) {
    invisible( futile.logger::flog.logger("error", futile.logger::ERROR, appender.null()) )
  } else {
    if ( file.exists(errorLog) ) result <- file.remove(errorLog)
    invisible( futile.logger::flog.logger("error", futile.logger::ERROR, futile.logger::appender.file(errorLog)) )
  }
  
  # Set up FATAL logging
  if ( is.null(fatalLog) ) {
    invisible( futile.logger::flog.appender(futile.logger::appender.console(), name='ROOT') )
  } else {
    if ( file.exists(fatalLog) ) result <- file.remove(fatalLog)
    invisible( futile.logger::flog.appender(futile.logger::appender.tee(fatalLog)) )
  }
  
}

#' @name logger.setLevel
#' @export
#' @title Set Console Log Level
#' @param level threshold level
#' @return No return value.
#' @description By default, the looger threshold is set to \code{FATAL} so that the console
#' will typically receive no log messages. By setting the level to one of the other log levels:
#' \code{TRACE, DEBUG, INFO, WARN, ERROR} users can see logging messages while running 
#' commands at the command line.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @seealso \code{\link{logger.setup}}
#' @examples
#' \dontrun{
#' # Set up console logging only
#' logger.setup()
#' logger.setLevel(DEBUG)
#' }
logger.setLevel <- function(level) {
  invisible( futile.logger::flog.threshold(level) )
}


#' @name logger.trace
#' @export
#' @title Python-Style Logging Statements
#' @param msg message with format strings applied to additional arguments
#' @param \dots additional arguments to be formatted
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{TRACE} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(debugLog='debug.log', infoLog='info.log', errorLog='error.log')
#' 
#' # But allow log statements at all levels within the code
#' logger.trace('trace statement #%d', 1)
#' logger.debug('debug statement')
#' logger.info('info statement %s %s', "with", "arguments")
#' logger.warn('warn statement %s', "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error('error message: %s', geterrmessage())
#' logger.fatal('fatal statement %s', "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the TRACE level
logger.trace <- function(msg, ...) {
  stopIfNotInitialized()
  futile.logger::flog.trace(msg, ..., name='ROOT')
  futile.logger::flog.trace(msg, ..., name='trace')
}

#' @name logger.debug
#' @export
#' @title Python-Style Logging Statements
#' @param msg message with format strings applied to additional arguments
#' @param \dots additional arguments to be formatted
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{DEBUG} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(debugLog='debug.log', infoLog='info.log', errorLog='error.log')
#' 
#' # But allow log statements at all levels within the code
#' logger.trace('trace statement #%d', 1)
#' logger.debug('debug statement')
#' logger.info('info statement %s %s', "with", "arguments")
#' logger.warn('warn statement %s', "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error('error message: %s', geterrmessage())
#' logger.fatal('fatal statement %s', "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the DEBUG level
logger.debug <- function(msg, ...) {
  stopIfNotInitialized()
  futile.logger::flog.debug(msg, ..., name='ROOT')
  futile.logger::flog.debug(msg, ..., name='trace')
  futile.logger::flog.debug(msg, ..., name='debug')
}

#' @name logger.info
#' @export
#' @title Python-Style Logging Statements
#' @param msg message with format strings applied to additional arguments
#' @param \dots additional arguments to be formatted
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{INFO} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(debugLog='debug.log', infoLog='info.log', errorLog='error.log')
#' 
#' # But allow log statements at all levels within the code
#' logger.trace('trace statement #%d', 1)
#' logger.debug('debug statement')
#' logger.info('info statement %s %s', "with", "arguments")
#' logger.warn('warn statement %s', "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error('error message: %s', geterrmessage())
#' logger.fatal('fatal statement %s', "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the INFO level
logger.info <- function(msg, ...) {
  stopIfNotInitialized()
  futile.logger::flog.info(msg, ..., name='ROOT')
  futile.logger::flog.info(msg, ..., name='trace')
  futile.logger::flog.info(msg, ..., name='debug')
  futile.logger::flog.info(msg, ..., name='info')
}

#' @name logger.warn
#' @export
#' @title Python-Style Logging Statements
#' @param msg message with format strings applied to additional arguments
#' @param \dots additional arguments to be formatted
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{WARN} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(debugLog='debug.log', infoLog='info.log', errorLog='error.log')
#' 
#' # But allow log statements at all levels within the code
#' logger.trace('trace statement #%d', 1)
#' logger.debug('debug statement')
#' logger.info('info statement %s %s', "with", "arguments")
#' logger.warn('warn statement %s', "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error('error message: %s', geterrmessage())
#' logger.fatal('fatal statement %s', "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the WARN level
logger.warn <- function(msg, ...) {
  stopIfNotInitialized()
  futile.logger::flog.warn(msg, ..., name='ROOT')
  futile.logger::flog.warn(msg, ..., name='trace')
  futile.logger::flog.warn(msg, ..., name='debug')
  futile.logger::flog.warn(msg, ..., name='info')
  futile.logger::flog.warn(msg, ..., name='warn')
}

#' @name logger.error
#' @export
#' @title Python-Style Logging Statements
#' @param msg message with format strings applied to additional arguments
#' @param \dots additional arguments to be formatted
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{ERROR} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(debugLog='debug.log', infoLog='info.log', errorLog='error.log')
#' 
#' # But allow log statements at all levels within the code
#' logger.trace('trace statement #%d', 1)
#' logger.debug('debug statement')
#' logger.info('info statement %s %s', "with", "arguments")
#' logger.warn('warn statement %s', "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error('error message: %s', geterrmessage())
#' logger.fatal('fatal statement %s', "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the ERROR level
logger.error <- function(msg, ...) {
  stopIfNotInitialized()
  futile.logger::flog.error(msg, ..., name='ROOT')
  futile.logger::flog.error(msg, ..., name='trace')
  futile.logger::flog.error(msg, ..., name='debug')
  futile.logger::flog.error(msg, ..., name='info')
  futile.logger::flog.error(msg, ..., name='warn')
  futile.logger::flog.error(msg, ..., name='error')
}

#' @name logger.fatal
#' @export
#' @title Python-Style Logging Statements
#' @param msg message with format strings applied to additional arguments
#' @param \dots additional arguments to be formatted
#' @return No return value.
#' @description After initializing the level-specific log files with \code{logger.setup(...)},
#' this function will generate \code{FATAL} level log statements.
#' @note All functionality is built on top of the excellent \pkg{futile.logger} package.
#' @examples
#' \dontrun{
#' # Only save three log files
#' logger.setup(debugLog='debug.log', infoLog='info.log', errorLog='error.log')
#' 
#' # But allow log statements at all levels within the code
#' logger.trace('trace statement #%d', 1)
#' logger.debug('debug statement')
#' logger.info('info statement %s %s', "with", "arguments")
#' logger.warn('warn statement %s', "about to try something dumb")
#' result <- try(1/"a", silent=TRUE)
#' logger.error('error message: %s', geterrmessage())
#' logger.fatal('fatal statement %s', "THE END")
#' }
#' @seealso \code{\link{logger.setup}}

# Log at the fatal level
logger.fatal <- function(msg, ...) {
  stopIfNotInitialized()
  futile.logger::flog.fatal(msg, ..., name='ROOT')
  futile.logger::flog.fatal(msg, ..., name='trace')
  futile.logger::flog.fatal(msg, ..., name='debug')
  futile.logger::flog.fatal(msg, ..., name='info')
  futile.logger::flog.fatal(msg, ..., name='warn')
  futile.logger::flog.fatal(msg, ..., name='error')
}

# ----- Utility functions -----------------------------------------------------

# This function is missing from futile.logger
appender.null <- function() {
  function(line) invisible(NULL)
}

# Quick test if futile.logger namespace is loaded
stopIfNotInitialized <- function() {
  if ( ! 'futile.logger' %in% loadedNamespaces() ) {
    stop("You must initialize with 'logger.setup()' before issuing logger statements.", call.=FALSE)
  }
}


# ----- Constants -------------------------------------------------------------

# Verbatim values from futile.logger::constants
#' @docType data
#' @name logLevels
#' @aliases FATAL ERROR WARN INFO DEBUG TRACE
#' @title Log Levels
#' @description Log levels matching those found in \pkg{futile.logger}.
#' @export
FATAL <- 1L
names(FATAL) <- "FATAL"
#' @export
ERROR <- 2L
names(ERROR) <- "ERROR"
#' @export
WARN <- 4L
names(WARN) <- "WARN"
#' @export
INFO <- 6L
names(INFO) <- "INFO"
#' @export
DEBUG <- 8L
names(DEBUG) <- "DEBUG"
#' @export
TRACE <- 9L
names(TRACE) <- "TRACE"

