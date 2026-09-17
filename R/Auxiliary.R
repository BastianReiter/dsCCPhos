

#===============================================================================
#   dsCCPhos Internal Auxiliary Functions
#===============================================================================



#===============================================================================
#' FormatPercentage
#' @keywords internal
#' @noRd
FormatPercentage <- function(x, digits = 0)
{
    if (is.na(x)) return ("NA")
    assert_that(is.numeric(x))
    return(paste0(round(x * 100, digits = digits), "%"))
}


#===============================================================================
#' Log.New
#'
#' Initiate a log report \code{tibble} with one or multiple entries
#'
#' @param PrintMessage \code{logical flag} - Whether to print messages after creation of log
#' @param PrintMessage.Compile \code{logical flag} - Whether to compile a message from different features or just print the content of 'Message'.
#' @param PrintMessage.Compilation \code{string} - Using pseudo-code tags, which features of the input log should be compiled into a printed message.
#'
#' @return A \code{tibble}
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Log.New <- function(Timestamp = Sys.time(),
                    ProcessingStage = NA_character_,
                    Table = NA_character_,
                    ProcessTopic = NA_character_,
                    ProcessTopic.Subgroup = NA_character_,
                    ProcessExecution = NA_character_,
                    Message = NA_character_,
                    MessageClass = "Info",
                    MessagePriority = 1L,
                    PrintMessage = FALSE,
                    PrintMessage.Compile = TRUE,
                    PrintMessage.Compilation = "<$Table$> - <$ProcessTopic$>: <$Message$>")
#-------------------------------------------------------------------------------
{
  assert_that(is.time(Timestamp),
              is.character(ProcessingStage),
              is.character(Table),
              is.character(ProcessTopic),
              is.character(ProcessTopic.Subgroup),
              is.character(ProcessExecution),
              is.character(Message),
              is.character(MessageClass),
              is.numeric(MessagePriority),
              is.flag(PrintMessage),
              is.flag(PrintMessage.Compile),
              is.string(PrintMessage.Compilation))

  Log <- tibble(Timestamp = Timestamp,
                ProcessingStage = ProcessingStage,
                Table = Table,
                ProcessTopic = ProcessTopic,
                ProcessTopic.Subgroup = ProcessTopic.Subgroup,
                ProcessExecution = ProcessExecution,
                Message = Message,
                MessageClass = MessageClass,
                MessagePriority = MessagePriority)

  if (PrintMessage == TRUE) { Log.Print(Log = Log,
                                        .Compile = PrintMessage.Compile,
                                        .Compilation = PrintMessage.Compilation) }

  return(Log)
}

#-------------------------------------------------------------------------------

#' Log.Add
#'
#' Add one or more records to an existing log data.frame and optionally print messages in the process.
#'
#' @param Log \code{data.frame} - An existing log.
#' @param Entry \code{data.frame} - New log entry
#' @param PrintMessage \code{logical flag} - Whether to print the messages contained in 'Log'
#' @param PrintMessage.Compile \code{logical flag} - Whether to compile a message from different features or just print the content of 'Message'.
#' @param PrintMessage.Compilation \code{string} - Using pseudo-code tags, which features of the input log should be compiled into a printed message.
#' @return The updated log \code{data.frame}
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Log.Add <- function(Log,
                    Entry,
                    PrintMessage = FALSE,
                    PrintMessage.Compile = TRUE,
                    PrintMessage.Compilation = "<$Table$> - <$ProcessTopic$>: <$Message$>")
#-------------------------------------------------------------------------------
{
  assert_that(is.data.frame(Log),
              is.data.frame(Entry),
              is.flag(PrintMessage),
              is.flag(PrintMessage.Compile),
              is.string(PrintMessage.Compilation))

  Log <- Log %>%
            bind_rows(Entry) %>%
            fill(ProcessingStage,      # Adopt values from previous rows
                 .direction = "down")

  if (PrintMessage == TRUE) { Log.Print(Log = Entry,
                                        .Compile = PrintMessage.Compile,
                                        .Compilation = PrintMessage.Compilation) }

  return(Log)
}

#-------------------------------------------------------------------------------

#' Log.Make
#'
#' Turn a data.frame with some properties of a log entry into a full log entry
#'
#' @param LogData \code{data.frame} - data on new log entries
#' @param PrintMessage \code{logical flag} - Whether messages should be printed to console
#' @param PrintMessage.Compile \code{logical flag} - Whether to compile a message from different features or just print the content of 'Message'.
#' @param PrintMessage.Compilation \code{string} - Using pseudo-code tags, which features of the input log should be compiled into a printed message.
#' @return A \code{data.frame} containing full log entry properties
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Log.Make <- function(LogData,
                     PrintMessage = FALSE,
                     PrintMessage.Compile = TRUE,
                     PrintMessage.Compilation = "<$Table$> - <$ProcessTopic$>: <$Message$>")
#-------------------------------------------------------------------------------
{
  assert_that(is.data.frame(LogData),
              is.flag(PrintMessage),
              is.flag(PrintMessage.Compile),
              is.string(PrintMessage.Compilation))

  # Select only columns in 'LogData' that would appear in a log entry
  LogData <- LogData %>%
                  select(any_of(names(Log.New())))

  Log <- do.call(Log.New, args = c(as.list(LogData),
                                   PrintMessage = PrintMessage,
                                   PrintMessage.Compile = PrintMessage.Compile,
                                   PrintMessage.Compilation = PrintMessage.Compilation))

  return(Log)
}

#-------------------------------------------------------------------------------

#' Log.Print
#'
#' Print messages contained in a log report data.frame
#'
#' @param Log \code{data.frame} - Log data.frame
#' @param .Compile \code{logical flag} - Whether to compile a message from different features or just print the content of 'Message'.
#' @param .Compilation \code{string} - Using pseudo-code tags, which features of the input log should be compiled into a printed message.
#' @return No return
#' @keywords internal
#' @noRd
#-------------------------------------------------------------------------------
Log.Print <- function(Log,
                      .Compile = TRUE,
                      .Compilation = "<$Table$> - <$ProcessTopic$>: <$Message$>")
#-------------------------------------------------------------------------------
{
  assert_that(is.data.frame(Log),
              is.flag(.Compile),
              is.string(.Compilation))

  PrintExpression <- "Message"

  if (.Compile == TRUE)
  {
      PrintExpression <- .Compilation %>%
                              str_replace_all("<\\$(.*?)\\$>", '", if_else(is.na(\\1), "", as.character(\\1)), "') %>%
                              { paste0('str_squish(paste0("', ., '"))') }
  }

  Messages <- Log %>%
                mutate(Message = case_when(.Compile == FALSE ~ Message,
                                           str_starts(MessageClass, "Details.") ~ Message,
                                           .default = !!rlang::parse_expr(PrintExpression)),
                       Message = Message %>% str_replace_all("- :", "") %>% str_trim()) %>%
                select(MessageClass,
                       Message) %>%
                tibble::deframe()

  for (i in 1:length(Messages)) { PrintSoloMessage(Messages[i]) }
}
#===============================================================================
