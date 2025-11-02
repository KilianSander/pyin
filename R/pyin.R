

#' Test pyin is working
#'
#' @inheritParams pyin
#'
#' @return If the pYIN setup was successful on your system, the function
#' returns a 10x5 tibble with the transcribed note events of the test file.
#'
#' @export
test_pyin <- function(if_bad_result_return_single_na = TRUE) {
  pyin(file_name = system.file('extdata/test.wav', package = 'pyin'),
       if_bad_result_return_single_na = if_bad_result_return_single_na)
}


#' Compute pYIN on an audio track
#'
#' @param file_name (character scalar) File to analyse.
#' See \url{https://vamp-plugins.org/sonic-annotator/} for information about
#' allowed file types.
#'
#' @param transform_file (character scalar or \code{NULL}) If a file name is
#' specified, the \code{-t} argument is used with the corresponding transform
#' file for pYIN execution.
#'
#' @param normalise (logical scalar) Whether or not to use the
#' \code{--normalise} argument for the pYIN execution.
#'
#' @param hidePrint (logical scalar) Whether or not to hide the
#' standard error stream (stderr) from pYIN execution in the R console.
#'
#' @param type (character scalar \code{"notes"}, \code{"pitch_track"},
#' or \code{"both"}) determines the output (see Value).
#'
#' @param if_bad_result_return_single_na (logical scalar) Whether or not to
#' return a single \code{NA} instead of a tibble if pYIN execution has a bad
#' result.
#'
#' @return For \code{type = "notes"} and \code{type = "pitch_track"},
#' \code{pyin()} returns a tibble containing the notes or the pitch track data,
#' respectively.
#' For \code{type = "both"}, it returns a list containing both tibbles.
#' The \code{notes} tibble contains the columns \code{file_name}, \code{onset},
#' \code{dur} (duration), \code{freq} (frequency), and \code{note}.
#' The \code{pitch_track} tibble contains the columns \code{file_name},
#' \code{onset}, and \code{freq} (frequency).
#'
#' @export
pyin <- function(file_name,
                 transform_file = NULL,
                 normalise = FALSE,
                 hidePrint = TRUE,
                 type = c("notes", "pitch_track", "both"),
                 if_bad_result_return_single_na = TRUE) {

  type <- match.arg(type)

  stopifnot(assertthat::is.string(file_name),
            assertthat::is.string(transform_file) | is.null(transform_file),
            assertthat::is.flag(normalise),
            assertthat::is.flag(hidePrint),
            assertthat::is.flag(if_bad_result_return_single_na))

  logging::loginfo("Calling pyin")
  logging::loginfo("Type: %s", type)

  if(type == "both") {

    notes_res <- pyin_single(file_name,
                       transform_file,
                       normalise,
                       hidePrint,
                       "notes",
                       if_bad_result_return_single_na)

    pitch_track_res <- pyin_single(file_name,
                             transform_file,
                             normalise,
                             hidePrint,
                             "pitch_track",
                             if_bad_result_return_single_na)

    res <- list(notes = notes_res,
                pitch_track = pitch_track_res)

  } else {
    res <- pyin_single(file_name,
                       transform_file,
                       normalise,
                       hidePrint,
                       type,
                       if_bad_result_return_single_na)
  }


  return(res)
}

suppoted_os <- c("osx", "linux", "windows")
os_bin <- c(
  "osx" = "bin/osx/sonic-annotator",
  "linux" = "bin/linux64/sonic-annotator",
  "windows" = "bin/windows64/sonic-annotator64.exe"
)

pyin_single <- function(file_name,
                        transform_file,
                        normalise,
                        hidePrint,
                        type,
                        if_bad_result_return_single_na) {

  op_sys <- get_os()

  logging::loginfo("Operating system: %s", op_sys)


  if(op_sys %in% suppoted_os) {

    set_vamp_variable(op_sys)

    vamp_cmd <- get_correct_vamp_cmd(type)

    args <- pyin_construct_args(transform_file, vamp_cmd, file_name, normalise)

    sa_out <- pyin_construct_command(args, hidePrint, op_sys)

    if(length(sa_out) == 0) {
      res <- pyin_handle_null(type, file_name)
    } else {

      res <- read.csv(text = sa_out, header = FALSE) %>% tibble::as_tibble()
      res <- pyin_tidy(res, type)

      file_name <- res$V1[[1]]

      res <- res %>% dplyr::select(-V1)

      res <- tibble::tibble(file_name, res)
    }
  } else {
    warning('OS not supported.')
  }


  cond <- is.null(res$freq) | all(is.na(res$freq))
  if(if_bad_result_return_single_na & cond) {
    res <- NA
  }
  return(res)
}

set_vamp_variable <- function(os) {

  if(os == "linux") {
    set_linux()
  } else if(os == "osx") {
    set_osx()
  } else if(os == "windows") {
    set_windows()
  } else {
    warning("Only Linux or Windows 64 bit is currently supported")
  }

}



set_windows <- function() {

  # package library path
  pkg_path <- system.file('bin/windows64', package = 'pyin')

  # in case the user already has VAMP plugins installed

  vamp_path0 <- system2("echo", args = "$VAMP_PATH")

  # potential library path one
  vamp_path1 <- homePath <- paste0(fs::path_home(), 'C:\\Program Files\\Vamp Plugins')

  # put all together separated by a colon
  dirs <- paste(pkg_path, vamp_path0, vamp_path1, sep = ";")

  Sys.setenv(VAMP_PATH = dirs)
  Sys.getenv("VAMP_PATH")
}

set_osx <- function() {

  # package library path
  pkg_path <- system.file('bin/osx', package = 'pyin')

  # in case the user already has VAMP plugins installed

  vamp_path0 <- system2("echo", args = "$VAMP_PATH")

  # potential library path one
  vamp_path1 <- paste0(fs::path_home(), '/Library/Audio/Plug-Ins/Vamp')

  # potential library path 2
  vamp_path2 <- '/Library/Audio/Plug-Ins/Vamp'

  # put all together separated by a colon
  dirs <- paste(pkg_path, vamp_path0, vamp_path1, vamp_path2, sep = ":")

  Sys.setenv(VAMP_PATH = dirs)
}


set_linux <- function() {

  logging::loginfo("Set linux")

  # package library path
  pkg_path <- system.file('bin/linux64', package = 'pyin')

  logging::loginfo("Package path: %s", pkg_path)

  # in case the user already has VAMP plugins installed

  vamp_path0 <- system2("echo", args = "$VAMP_PATH")

  # potential library path 1
  vamp_path1 <- paste0('/usr/local/lib/vamp')

  # put all together separated by a colon
  dirs <- paste(pkg_path, vamp_path0, vamp_path1, sep = ":")

  logging::loginfo("Vamp dirs: %s", dirs)

  Sys.setenv(VAMP_PATH = dirs)
}

pyin_tidy <- function(res, type) {
  if(type == "notes") {
    res <- res %>%
      dplyr::rename(onset = V2, dur = V3, freq = V4) %>%
      dplyr::mutate(
        onset = round(onset, 2),
        dur = round(dur, 2),
        freq = round(freq, 2),
        note = round(hrep::freq_to_midi(freq)))
  } else {
    res <- res %>%
      dplyr::rename(onset = V2, freq = V3) %>%
      dplyr::mutate(
        onset = round(onset, 2),
        freq = round(freq, 2))
  }
}

pyin_construct_args <- function(transform_file, vamp_cmd, file_name, normalise) {
  if(is.null(transform_file)) {
    args <- c("-d",
              vamp_cmd,
              file_name,
              "-w",
              "csv --csv-stdout")
  } else {
    args <- c(paste0('-t ', transform_file),
              file_name,
              "-w",
              "csv --csv-stdout")
  }

  if(normalise == 1) {
    args <- c(args, "--normalise")
  }
  args
}

pyin_construct_command <- function(args, hidePrint, os) {

  if(os %in% suppoted_os) {
    cmd <- system.file(os_bin[os], package = 'pyin')
  } else {
    warning('OS not supported')
  }

  logging::loginfo("Execute command: %s", cmd)

  if(hidePrint) {
    sa_out <- system2(command = cmd,
                      args = args,
                      stdout = TRUE, stderr = FALSE)
  } else {
    sa_out <- system2(command = cmd,
                      args = args,
                      stdout = TRUE)
  }
}


pyin_handle_null <- function(type, file_name) {
  if(type == "notes") {
    res <- tibble::tibble(onset = NA, dur = NA, freq = NA, note = NA, file_name = file_name)
  } else {
    res <- tibble::tibble(onset = NA, freq = NA, file_name = file_name)
  }
}

get_correct_vamp_cmd <- function(type) {

  if(type == "pitch_track") {
    "vamp:pyin:pyin:smoothedpitchtrack"
  } else if(type == "notes") {
    "vamp:pyin:pyin:notes"
  } else {
    stop("Unknown type")
  }
}


get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

