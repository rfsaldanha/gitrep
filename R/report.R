#' Render a Git repository report
#'
#' @param after character. Starting date.
#' @param until character. Ending data. Defaults to today.
#' @param max numeric. Maximum number of commits to retrieve. Defaults to 1e+9.
#' @param repo character. Repository path on your computer. Defaults to the active repository. You can also provide an SSH repository address that will be temporally cloned.
#' @param output_dir character. Output directory. Defaults to temporary dir.
#' @param output_file character. Output report name. Defaults to \code{report.html}.
#' @param files_stats logical. Fetch file stats. Can take longer on large repos. Defaults to \code{FALSE}.
#' @param render_quiet logical. Show report rendering messages. Defaults to \code{FALSE}.
#'
#' @return an html report file.
#' @export
report <- function(after, until = Sys.Date(), max = 1e+9, repo = ".", output_dir = tempdir(), output_file = "report.html", files_stats = FALSE, render_quiet = TRUE){
  # Convert to date
  after <- lubridate::as_date(after)
  until <- lubridate::as_date(until)

  # Assert date
  checkmate::assert_date(after)
  checkmate::assert_date(until)

  # Assert other params
  checkmate::assert_count(max)
  checkmate::assert_logical(files_stats)
  checkmate::assert_logical(render_quiet)



  # If remote repo
  if(substr(repo, 0, 4) == "git@"){
    tmp_dir <- fs::path_temp("clone_repo")

    if(fs::dir_exists(tmp_dir)){
      fs::dir_delete(path = tmp_dir)
    }

    cli::cli_alert_info("Cloning repo to temp dir...")
    gert::git_clone(url = repo, path = tmp_dir)
    repo = tmp_dir
    cli::cli_alert_success("Done!")
  }

  # Retrieve repo log
  cli::cli_alert_info("Retrieving repo log...")
  log_df <- gert::git_log(after = after, max = max, repo = repo)
  log_df <- subset(log_df, time >= after)
  log_df <- subset(log_df, time <= until+1)
  info_ls <- gert::git_info(repo = repo)
  cli::cli_alert_success("Done!")

  # Retrieve files stats
  if(files_stats == TRUE){
    cli::cli_alert_info("Retrieving file stats...")
    files_ls <- gert::git_ls(repo = repo)
    files_stats <- purrr::map(files_ls$path, .f = gert::git_stat_files, repo = repo)
    files_stats <- purrr::list_rbind(files_stats)
    files_stats$head <- NULL
    cli::cli_alert_success("Done!")
  } else {
    files_stats <- NULL
  }


  # Report template
  file <- system.file("rmd/report.rmd", package = "gitrep")

  # Render report
  cli::cli_alert_info("Rendering report...")
  rmarkdown::render(
    input = file,
    output_file = output_file,
    output_dir = output_dir,
    quiet = render_quiet,
    params = list(
      since_date = after,
      until_date = until,
      info_ls = info_ls,
      log_df = log_df,
      files_stats = files_stats
    )
  )

  cli::cli_alert_success("Done!")
  cli::cli_alert("Output: {normalizePath(paste0(output_dir, '/', output_file))}")

}
