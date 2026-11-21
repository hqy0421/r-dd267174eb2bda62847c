#' Apply a batch of row changes to a tibble
#'
#' @description
#' `merge_rows()` applies an ordered batch of row-level insert, update and
#' delete operations carried by a staging tibble to a main table.
#' The rows of `.changes` are processed in arrival order, and the same
#' business key may appear in several rows.
#'
#' The main table is identified by a composite business key (`.by`).
#' Key values must be unique in `.data`, and no key component may be
#' missing, neither in `.data` nor in `.changes`.
#'
#' Each staging row carries:
#'
#' * the key columns (named in `.by`),
#' * an operation, one of `"insert"`, `"update"` or `"delete"`, in the
#'   column named by `.op`,
#' * a character vector of non-key column names in the list column named
#'   by `.cols`. These are the columns for which the row supplies a value.
#'   Delete operations must carry an empty vector (or `NULL`).
#'
#' Semantics:
#'
#' * `"insert"` requires the key to be absent. Columns listed in `.cols`
#'   receive the staging values; all other columns are filled with a
#'   type-correct missing value. The new row is appended at the end.
#' * `"update"` requires the key to be present. Only the columns listed
#'   in `.cols` change; unlisted columns keep their current values.
#'   An explicitly supplied missing value overwrites the current value.
#' * `"delete"` requires the key to be present and removes the row.
#'   Inserting the same key afterwards appends a new row at the end.
#'
#' Staging values are cast to the column types of `.data` using the
#' [vctrs][vctrs::vec_cast] coercion rules, so lossy conversions are
#' rejected. The column order, column types and the relative order of
#' the surviving rows of `.data` are preserved.
#'
#' Any invalid key, column list or operation aborts the whole batch and
#' the error identifies the offending row in `.changes`.
#'
#' @param .data Main table, a data frame or tibble. Its `.by` columns must
#'   be unique and free of missing values.
#' @param .changes Staging tibble whose rows describe the changes in
#'   arrival order. It contains the `.by` columns, the operation column,
#'   the column-list column and the non-key columns referenced by the
#'   column lists; columns that do not correspond to a non-key column of
#'   `.data` are rejected.
#' @param .by A non-empty character vector of column names forming the
#'   composite business key. The columns must exist in both `.data` and
#'   `.changes`.
#' @param .op Name of the operation column in `.changes`, a single
#'   non-missing string. The column itself must be a character vector
#'   containing only `"insert"`, `"update"` and `"delete"`.
#' @param .cols Name of the column-list column in `.changes`, a single
#'   non-missing string. The column itself must be a list whose elements
#'   are character vectors (or `NULL`) naming non-key columns of `.data`
#'   that are also present in `.changes`.
#' @return A tibble with the same column order and column types as
#'   `.data` and the row changes applied.
#' @family addition
#' @examples
#' main <- tibble(
#'   id = c(1L, 2L, 3L),
#'   name = c("a", "b", "c"),
#'   value = c(10, 20, 30)
#' )
#'
#' staging <- tibble(
#'   id = c(3L, 4L, 1L, 2L),
#'   .op = c("delete", "insert", "update", "update"),
#'   .cols = list(
#'     character(0),
#'     c("name", "value"),
#'     "value",
#'     character(0)
#'   ),
#'   name = c(NA, "d", NA, NA),
#'   value = c(NA, 40, 99, NA)
#' )
#'
#' merge_rows(main, staging, .by = "id")
#' @export
merge_rows <- function(
  .data,
  .changes,
  .by,
  .op = ".op",
  .cols = ".cols"
) {
  call <- caller_env()

  if (inherits(.data, "grouped_df")) {
    abort_merge_grouped_df(call)
  }
  if (!is.data.frame(.data)) {
    abort_merge_not_data_frame(".data", call)
  }
  if (!is.data.frame(.changes)) {
    abort_merge_not_data_frame(".changes", call)
  }

  check_merge_control_name(.op, ".op", call)
  check_merge_control_name(.cols, ".cols", call)

  .by <- check_merge_by(.by, call)

  data_names <- names(.data)
  changes_names <- names(.changes)

  missing_in_data <- setdiff(.by, data_names)
  if (has_length(missing_in_data)) {
    abort_merge_missing_columns(missing_in_data, ".data", call)
  }

  control_cols <- c(.by, .op, .cols)
  missing_in_changes <- setdiff(control_cols, changes_names)
  if (has_length(missing_in_changes)) {
    abort_merge_missing_columns(missing_in_changes, ".changes", call)
  }

  if (.op == .cols || .op %in% .by || .cols %in% .by) {
    abort_merge_control_conflict(.op, .cols, .by, call)
  }

  # Staging value columns must correspond to non-key columns of the main
  # table. Columns that no row lists are allowed and simply ignored.
  value_cols <- setdiff(data_names, .by)
  unknown_cols <- setdiff(
    setdiff(changes_names, c(.by, .op, .cols)),
    value_cols
  )
  if (has_length(unknown_cols)) {
    abort_merge_unknown_columns(unknown_cols, call)
  }

  n <- fast_nrow(.changes)

  # Operations -----------------------------------------------------------
  ops <- .changes[[.op]]
  if (!is_bare_character(ops)) {
    abort_merge_op_not_character(.op, call)
  }
  if (anyNA(ops)) {
    abort_merge_op_missing(which(is.na(ops))[[1L]], call)
  }
  bad_op <- !(ops %in% merge_valid_ops)
  if (any(bad_op)) {
    bad_row <- which(bad_op)[[1L]]
    abort_merge_op_invalid(ops[[bad_row]], bad_row, call)
  }

  # Column lists ---------------------------------------------------------
  col_lists <- .changes[[.cols]]
  if (!is.list(col_lists)) {
    abort_merge_cols_not_list(.cols, call)
  }

  row_cols <- vector("list", n)
  for (i in seq_len(n)) {
    listed <- col_lists[[i]]
    if (is.null(listed)) {
      listed <- character(0)
    }
    if (!is_bare_character(listed)) {
      abort_merge_cols_invalid(i, call)
    }
    if (anyNA(listed)) {
      abort_merge_cols_na(i, call)
    }
    if (anyDuplicated(listed)) {
      abort_merge_cols_duplicate(i, call)
    }

    unknown <- setdiff(listed, value_cols)
    if (has_length(unknown)) {
      abort_merge_cols_unknown(i, unknown, call)
    }
    absent <- setdiff(listed, changes_names)
    if (has_length(absent)) {
      abort_merge_cols_absent(i, absent, call)
    }
    if (ops[[i]] == "delete" && has_length(listed)) {
      abort_merge_delete_with_cols(i, call)
    }

    row_cols[[i]] <- listed
  }

  listed_union <- unique(unlist(row_cols, use.names = FALSE))

  # Main table keys ------------------------------------------------------
  main_keys <- .data[.by]
  key_missing <- vec_detect_missing(main_keys)
  if (any(key_missing)) {
    missing_key_cols <- .by[vapply(
      lapply(main_keys, vec_detect_missing),
      any,
      logical(1)
    )]
    abort_merge_main_key_na(missing_key_cols, call)
  }
  if (vec_duplicate_any(main_keys)) {
    key_id <- vec_group_id(main_keys)
    dup_row <- which(duplicated(key_id))[[1L]]
    abort_merge_main_key_duplicate(dup_row, call)
  }

  # Staging keys ---------------------------------------------------------
  changes_keys <- .changes[.by]
  staging_key_missing <- vec_detect_missing(changes_keys)
  if (any(staging_key_missing)) {
    abort_merge_changes_key_na(which(staging_key_missing)[[1L]], call)
  }

  # Type casting (whole batch, lossy casts are rejected) -----------------
  cast_cols <- c(.by, listed_union)
  cast <- new_list(length(cast_cols))
  names(cast) <- cast_cols
  for (col in cast_cols) {
    cast[[col]] <- merge_cast_column(
      .changes[[col]],
      .data[[col]],
      col,
      call
    )
  }

  # Hash index over key groups, built once --------------------------------
  n_main <- fast_nrow(.data)
  changes_key_frame <- new_data_frame(cast[.by], n = n)
  all_key_id <- vec_group_id(vec_rbind(main_keys, changes_key_frame))
  main_id <- vec_slice(all_key_id, seq_len(n_main))
  changes_id <- vec_slice(all_key_id, seq2(n_main + 1L, n_main + n))

  # Key group id -> row position in the working table.
  # Removed keys are deleted from the environment so that a subsequent
  # insert sees the key as absent.
  locations <- new_environment()
  for (i in seq_len(n_main)) {
    locations[[as.character(main_id[[i]])]] <- i
  }

  # Working-table bookkeeping -------------------------------------------
  # At most `n` rows are appended; preallocate the alive marker.
  alive <- rep(TRUE, n_main + n)
  n_ins <- 0L

  # Each change contributes at most one fill event per column, so `n`
  # entries per column is a safe upper bound. Preallocate and track the
  # fill count with a named cursor vector to keep the change loop linear.
  fill_pos <- new_list(length(data_names))
  fill_idx <- new_list(length(data_names))
  fill_n <- integer(length(data_names))
  names(fill_pos) <- data_names
  names(fill_idx) <- data_names
  names(fill_n) <- data_names
  for (col in data_names) {
    fill_pos[[col]] <- integer(n)
    fill_idx[[col]] <- integer(n)
  }

  for (i in seq_len(n)) {
    group <- as.character(changes_id[[i]])
    current <- locations[[group]]
    op <- ops[[i]]

    if (op == "insert") {
      if (!is.null(current)) {
        abort_merge_insert_exists(i, call)
      }

      n_ins <- n_ins + 1L
      slot <- n_main + n_ins
      locations[[group]] <- slot

      # Key components are always supplied by an insert.
      filled_cols <- c(.by, row_cols[[i]])
      for (col in filled_cols) {
        j <- fill_n[[col]] + 1L
        fill_n[[col]] <- j
        fill_pos[[col]][j] <- slot
        fill_idx[[col]][j] <- i
      }
    } else {
      if (is.null(current)) {
        abort_merge_change_missing(i, op, call)
      }

      if (op == "delete") {
        alive[[current]] <- FALSE
        rm(list = group, envir = locations)
      } else {
        # Update: only listed columns change.
        for (col in row_cols[[i]]) {
          j <- fill_n[[col]] + 1L
          fill_n[[col]] <- j
          fill_pos[[col]][j] <- current
          fill_idx[[col]][j] <- i
        }
      }
    }
  }

  # Assemble the working table ------------------------------------------
  work <- as.list(.data)
  for (j in seq_along(work)) {
    col_name <- names(work)[[j]]
    column <- work[[j]]

    if (n_ins > 0L) {
      # Type-correct missing values, including NULL for list columns.
      column <- vec_c(column, vec_init(column, n_ins))
    }

    used <- fill_n[[col_name]]
    if (used > 0L) {
      positions <- vec_slice(fill_pos[[col_name]], seq_len(used))
      change_rows <- vec_slice(fill_idx[[col_name]], seq_len(used))
      values <- vec_slice(cast[[col_name]], change_rows)
      column <- vec_assign(column, positions, values)
    }

    work[[j]] <- column
  }

  keep <- which(alive[seq_len(n_main + n_ins)])
  out <- new_data_frame(work, n = n_main + n_ins)
  out <- vec_slice(out, keep)
  vectbl_restore(out, .data)
}

merge_valid_ops <- c("insert", "update", "delete")

# Helpers -----------------------------------------------------------------

check_merge_control_name <- function(x, arg, call) {
  if (!is_bare_string(x)) {
    abort_merge_control_not_string(arg, call)
  }
}

check_merge_by <- function(.by, call) {
  if (!is_bare_character(.by) || !has_length(.by)) {
    abort_merge_by_invalid(call)
  }
  if (anyNA(.by)) {
    abort_merge_by_invalid(call)
  }
  if (anyDuplicated(.by)) {
    abort_merge_by_duplicate(call)
  }
  .by
}

merge_cast_column <- function(x, to, col_name, call) {
  tryCatch(
    vec_cast(x, to),
    vctrs_error = function(cnd) {
      # Locate the first row that cannot be cast.
      n <- vec_size(x)
      for (i in seq_len(n)) {
        tryCatch(
          vec_cast(vec_slice(x, i), to),
          vctrs_error = function(e) {
            abort_merge_cast(i, col_name, e, call)
          }
        )
      }
      abort_merge_cast(NA_integer_, col_name, cnd, call)
    }
  )
}

# Errors ------------------------------------------------------------------

abort_merge_grouped_df <- function(call = caller_env()) {
  tibble_abort(call = call, "Can't apply row changes to grouped data frames.")
}

abort_merge_not_data_frame <- function(arg, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("`", arg, "` must be a data frame.")
  )
}

abort_merge_control_not_string <- function(arg, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("`", arg, "` must be a single non-missing string.")
  )
}

abort_merge_by_invalid <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "`.by` must be a non-empty character vector of column names without missing values."
  )
}

abort_merge_by_duplicate <- function(call = caller_env()) {
  tibble_abort(call = call, "`.by` must not contain duplicate column names.")
}

abort_merge_missing_columns <- function(names, arg, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      paste0("Can't find column(s) in `", arg, "`:"),
      msg_unknown_column_names(names)
    ),
    names = names
  )
}

abort_merge_control_conflict <- function(.op, .cols, .by, call = caller_env()) {
  tibble_abort(
    call = call,
    bullets(
      "The operation and column-list columns must be distinct from each other and from `.by`:",
      x = paste0("`.op` = ", tick(.op), ", `.cols` = ", tick(.cols)),
      i = paste0("`.by` = ", paste(tick(.by), collapse = ", "))
    )
  )
}

abort_merge_unknown_columns <- function(names, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      "`.changes` contains column(s) that are not non-key columns of `.data`:",
      msg_unknown_column_names(names)
    ),
    names = names
  )
}

abort_merge_op_not_character <- function(col, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "The operation column ", tick(col), " must be a character vector."
    )
  )
}

abort_merge_op_missing <- function(row, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("Missing operation in row ", row, " of `.changes`.")
  )
}

abort_merge_op_invalid <- function(value, row, call = caller_env()) {
  tibble_abort(
    call = call,
    bullets(
      paste0("Invalid operation in row ", row, " of `.changes`:"),
      x = paste0(tick(value), " is not one of ", paste(merge_valid_ops, collapse = ", "), ".")
    ),
    row = row
  )
}

abort_merge_cols_not_list <- function(col, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "The column-list column ", tick(col), " must be a list column."
    )
  )
}

abort_merge_cols_invalid <- function(row, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "`.cols` must contain character vectors or NULL, see row ",
      row,
      " of `.changes`."
    ),
    row = row
  )
}

abort_merge_cols_na <- function(row, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("`.cols` must not contain missing column names, see row ", row, " of `.changes`."),
    row = row
  )
}

abort_merge_cols_duplicate <- function(row, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0("`.cols` must not contain duplicate column names, see row ", row, " of `.changes`."),
    row = row
  )
}

abort_merge_cols_unknown <- function(row, names, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      paste0("Unknown column name(s) in `.cols` in row ", row, " of `.changes`:"),
      msg_unknown_column_names(names)
    ),
    row = row,
    names = names
  )
}

abort_merge_cols_absent <- function(row, names, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      paste0("Column name(s) listed in `.cols` are missing from `.changes`, see row ", row, ":"),
      msg_unknown_column_names(names)
    ),
    row = row,
    names = names
  )
}

abort_merge_delete_with_cols <- function(row, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Delete operations must not list columns in `.cols`, see row ",
      row,
      " of `.changes`."
    ),
    row = row
  )
}

abort_merge_main_key_na <- function(names, call = caller_env()) {
  tibble_abort(
    call = call,
    pluralise_commas(
      "Key column(s) of `.data` contain missing values: ",
      tick(names),
      "."
    ),
    names = names
  )
}

abort_merge_main_key_duplicate <- function(row, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "`.by` columns must be unique in `.data`, the first duplicate key is in row ",
      row,
      "."
    ),
    row = row
  )
}

abort_merge_changes_key_na <- function(row, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Key column(s) must not contain missing values, see row ",
      row,
      " of `.changes`."
    ),
    row = row
  )
}

abort_merge_insert_exists <- function(row, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Insert in row ",
      row,
      " of `.changes` failed: the key already exists."
    ),
    row = row
  )
}

abort_merge_change_missing <- function(row, op, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      capitalize_first(op),
      " in row ",
      row,
      " of `.changes` failed: the key does not exist."
    ),
    row = row
  )
}

abort_merge_cast <- function(row, col_name, parent, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Can't cast the value of column ",
      tick(col_name),
      if (is.na(row)) "." else paste0(" in row ", row, " of `.changes`.")
    ),
    row = row,
    col = col_name,
    parent = parent
  )
}

capitalize_first <- function(x) {
  paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
}
