# app/logic/file_browser.R
#
# Filesystem queries behind the app's own file dialog: quick-access places,
# directory listings and file-name checks that hold on Linux, macOS, native
# Windows and Windows through WSL (where Windows drives appear under /mnt).

box::use(
  fs[dir_ls, file_info, path_home],
  rlang[`%||%`],
)

# Entries listed at most per folder; a larger folder is cut off with a notice.
MAX_ENTRIES <- 5000L

# Folders up to this many entries also show sizes and dates (one stat() each).
STAT_LIMIT <- 1000L

# Names Windows reserves for devices, refused as file names on every platform so
# a database created on Linux can still be copied to Windows.
RESERVED_NAMES <- c(
  "CON", "PRN", "AUX", "NUL",
  paste0("COM", 1:9),
  paste0("LPT", 1:9)
)

#' Is the App Running on Windows?
#'
#' @return Logical scalar.
#' @export
on_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' Is the App Running Inside WSL?
#'
#' @return Logical scalar.
#' @export
on_wsl <- function() {
  if (nzchar(Sys.getenv("WSL_DISTRO_NAME"))) {
    return(TRUE)
  }
  version <- tryCatch(
    readLines("/proc/version", n = 1L, warn = FALSE),
    error = function(e) ""
  )
  length(version) == 1L && grepl("microsoft", version, ignore.case = TRUE)
}

#' Home Directory
#'
#' @return Character path with forward slashes.
#' @export
home_dir <- function() {
  as.character(path_home())
}

#' Quick-Access Places
#'
#' @description The places a file dialog offers next to the folder listing: the
#'   home folder and its usual subfolders, then the roots of the file system -
#'   drive letters on Windows, the Windows drives WSL mounts under `/mnt`, and
#'   `/` plus removable-media mount points elsewhere.
#'
#' @return Data frame with `label`, `path` and `icon`, existing folders only.
#' @export
places <- function() {
  home <- home_dir()
  rows <- data.frame(
    label = c("Home", "Desktop", "Documents", "Downloads"),
    path = c(home, file.path(home, c("Desktop", "Documents", "Downloads"))),
    icon = c("house", "desktop", "file-lines", "download"),
    stringsAsFactors = FALSE
  )

  if (on_windows()) {
    drives <- paste0(LETTERS, ":/")
    drives <- drives[dir.exists(drives)]
    rows <- rbind(rows, data.frame(
      label = paste0("Drive ", substr(drives, 1L, 2L)),
      path = drives,
      icon = "hard-drive"
    ))
  } else {
    if (on_wsl()) {
      mounts <- list.files("/mnt", pattern = "^[a-z]$", full.names = TRUE)
      mounts <- mounts[dir.exists(mounts)]
      rows <- rbind(rows, data.frame(
        label = paste0("Windows ", toupper(basename(mounts)), ":"),
        path = mounts,
        icon = "hard-drive"
      ))
    }
    user <- Sys.getenv("USER")
    media <- c(
      if (nzchar(user)) file.path(c("/media", "/run/media"), user),
      "/Volumes"
    )
    media <- media[dir.exists(media)]
    rows <- rbind(
      rows,
      data.frame(label = "Computer", path = "/", icon = "computer"),
      if (length(media)) {
        data.frame(label = "Removable media", path = media, icon = "hard-drive")
      }
    )
  }
  rows <- rows[dir.exists(rows$path), , drop = FALSE]
  rows[!duplicated(rows$path), , drop = FALSE]
}

#' Normalise a Folder Path
#'
#' @param path Character path as typed or picked; `~` is expanded.
#'
#' @return The absolute folder path with forward slashes, or `NA` when it is
#'   not an existing folder.
#' @export
normalize_dir <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
    return(NA_character_)
  }
  path <- path.expand(trimws(path))
  if (!dir.exists(path)) {
    return(NA_character_)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Parent Folder
#'
#' @param path Character absolute folder path.
#'
#' @return The parent folder, or `path` itself at a file system root.
#' @export
parent_dir <- function(path) {
  parent <- dirname(path)
  if (identical(parent, ".") || identical(parent, path)) path else parent
}

#' Breadcrumbs of a Folder Path
#'
#' @param path Character absolute folder path with forward slashes.
#'
#' @return Data frame with `label` and `path`, root first.
#' @export
path_crumbs <- function(path) {
  out <- list()
  repeat {
    label <- basename(path)
    if (!nzchar(label) || identical(parent_dir(path), path)) {
      label <- if (grepl("^[A-Za-z]:", path)) substr(path, 1L, 2L) else "/"
      out[[length(out) + 1L]] <- c(label, path)
      break
    }
    out[[length(out) + 1L]] <- c(label, path)
    path <- parent_dir(path)
  }
  out <- do.call(rbind, rev(out))
  data.frame(label = out[, 1], path = out[, 2], stringsAsFactors = FALSE)
}

#' List a Folder
#'
#' @description Folders first, then files, each sorted by name, case-blind.
#'   Unreadable entries are skipped rather than failing the listing.
#'
#' @details
#' Whether an entry is a folder is read from the directory itself (the entry
#' type `readdir()` reports), not by a `stat()` per entry. That keeps large
#' folders fast where every `stat()` is expensive: Windows drives seen from
#' WSL (`/mnt/c`, each call crossing into Windows), NTFS under antivirus
#' scanning, and network shares. Sizes and dates do need a `stat()` each, so
#' they are only read for folders of up to `STAT_LIMIT` entries.
#'
#' @param path Character absolute folder path.
#' @param files Logical. List files as well as folders.
#' @param pattern Character regular expression (case-blind) marking the files
#'   the caller expects, or `NULL`. It never hides a file: browsers save some
#'   downloads without a name or extension, and those must stay pickable.
#' @param hidden Logical. Include dot-files and dot-folders.
#'
#' @return Data frame with `name`, `path`, `is_dir`, `match`, `size` and
#'   `modified`; the attribute `"truncated"` is `TRUE` when entries were cut
#'   off.
#' @export
list_dir <- function(path, files = TRUE, pattern = NULL, hidden = FALSE) {
  listed <- function(type) {
    tryCatch(
      as.character(dir_ls(path, all = hidden, type = type, fail = FALSE)),
      error = function(e) character(0)
    )
  }
  dirs <- listed("directory")
  plain <- if (files) listed("file") else character(0)
  # A symlink is resolved only to tell a linked folder from a linked file.
  links <- listed("symlink")
  link_dir <- dir.exists(links)
  dirs <- c(dirs, links[link_dir])
  if (files) {
    plain <- c(plain, links[!link_dir & file.exists(links)])
  }

  entries <- c(dirs, plain)
  is_dir <- rep(c(TRUE, FALSE), c(length(dirs), length(plain)))
  names <- basename(entries)
  order <- order(!is_dir, tolower(names))
  entries <- entries[order]
  is_dir <- is_dir[order]
  names <- names[order]
  truncated <- length(entries) > MAX_ENTRIES
  if (truncated) {
    keep <- seq_len(MAX_ENTRIES)
    entries <- entries[keep]
    is_dir <- is_dir[keep]
    names <- names[keep]
  }

  size <- rep(NA_real_, length(entries))
  modified <- rep(as.POSIXct(NA), length(entries))
  if (length(entries) && length(entries) <= STAT_LIMIT) {
    info <- tryCatch(file_info(entries, fail = FALSE), error = function(e) NULL)
    if (!is.null(info)) {
      size <- as.numeric(info$size)
      modified <- info$modification_time
    }
  }

  out <- data.frame(
    name = names,
    path = entries,
    is_dir = is_dir,
    match = !is_dir & (is.null(pattern) | grepl(pattern %||% "", names, ignore.case = TRUE)),
    size = size,
    modified = modified,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  structure(out, truncated = truncated)
}

#' Check a File or Folder Name
#'
#' @description Accepts only names valid on both Windows and POSIX file
#'   systems, so a database created on one can be moved to the other.
#'
#' @param name Character scalar, without any folder part.
#'
#' @return `NA` for a valid name, otherwise the reason it is refused.
#' @export
name_problem <- function(name) {
  if (is.null(name) || length(name) != 1L || is.na(name) || !nzchar(trimws(name))) {
    return("Enter a name.")
  }
  if (nchar(name) > 150L) {
    return("The name is too long.")
  }
  if (grepl("[<>:\"/\\\\|?*[:cntrl:]]", name)) {
    return("The name contains characters that are not allowed: < > : \" / \\ | ? *")
  }
  if (grepl("[ .]$", name) || grepl("^[ ]", name) || name %in% c(".", "..")) {
    return("The name may not start with a space or end with a space or dot.")
  }
  if (toupper(sub("\\..*$", "", name)) %in% RESERVED_NAMES) {
    return("This name is reserved by Windows.")
  }
  NA_character_
}

#' Create a Folder
#'
#' @param parent Character existing folder path.
#' @param name Character new folder name.
#'
#' @return The new folder's path; stops with a readable message on failure.
#' @export
create_folder <- function(parent, name) {
  problem <- name_problem(name)
  if (!is.na(problem)) {
    stop(problem)
  }
  target <- file.path(parent, name)
  if (file.exists(target)) {
    stop("A file or folder with this name already exists.")
  }
  if (!dir.create(target, showWarnings = FALSE)) {
    stop("The folder could not be created here (no write permission?).")
  }
  normalizePath(target, winslash = "/")
}

#' Is a Folder Writable?
#'
#' @param path Character folder path.
#'
#' @return Logical scalar.
#' @export
dir_writable <- function(path) {
  !is.na(path) && dir.exists(path) && file.access(path, 2L) == 0L
}

#' Human-Readable File Size
#'
#' @param bytes Numeric vector.
#'
#' @return Character vector, empty where the size is unknown.
#' @export
format_size <- function(bytes) {
  units <- c("B", "KB", "MB", "GB", "TB")
  out <- character(length(bytes))
  ok <- !is.na(bytes)
  power <- pmin(floor(log(pmax(bytes[ok], 1), 1024)), length(units) - 1)
  value <- bytes[ok] / 1024^power
  out[ok] <- ifelse(
    power == 0,
    sprintf("%.0f B", bytes[ok]),
    sprintf("%.1f %s", value, units[power + 1])
  )
  out
}
