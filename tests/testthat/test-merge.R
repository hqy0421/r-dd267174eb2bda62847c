# merge_rows --------------------------------------------------------------

test_that("inserts, updates and deletes are applied in arrival order", {
  main <- tibble(
    id = c(1L, 2L, 3L),
    name = c("a", "b", "c"),
    value = c(10, 20, 30)
  )
  staging <- tibble(
    id = c(3L, 4L, 1L, 2L),
    .op = c("delete", "insert", "update", "update"),
    .cols = list(
      character(0),
      c("name", "value"),
      "value",
      character(0)
    ),
    name = c(NA, "d", NA, NA),
    value = c(NA, 40, 99, NA)
  )

  out <- merge_rows(main, staging, .by = "id")

  expect_identical(out$id, c(1L, 2L, 4L))
  expect_identical(out$name, c("a", "b", "d"))
  expect_identical(out$value, c(99, 20, 40))
  # Column order, names and class are preserved.
  expect_identical(names(out), names(main))
  expect_s3_class(out, "tbl_df")
})

test_that("surviving rows keep their relative order, reinserted rows move to the end", {
  main <- tibble(
    k1 = c(1L, 1L, 2L),
    k2 = c("a", "b", "a"),
    v = c(10, 20, 30)
  )
  staging <- tibble(
    k1 = c(1L, 2L, 1L, 3L),
    k2 = c("a", "a", "a", "z"),
    .op = c("delete", "update", "insert", "insert"),
    .cols = list(character(0), "v", "v", "v"),
    v = c(NA, 99, 77, 5)
  )

  out <- merge_rows(main, staging, .by = c("k1", "k2"))

  expect_identical(out$k1, c(1L, 2L, 1L, 3L))
  expect_identical(out$k2, c("b", "a", "a", "z"))
  expect_identical(out$v, c(20, 99, 77, 5))
})

test_that("inserts fill unlisted columns with type-correct missing values", {
  main <- tibble(
    id = 1L,
    f = factor("a", levels = c("a", "b")),
    l = list(1)
  )
  staging <- tibble(
    id = 2L,
    .op = "insert",
    .cols = list(character(0)),
    f = NA,
    l = list(NULL)
  )

  out <- merge_rows(main, staging, .by = "id")

  expect_identical(levels(out$f), c("a", "b"))
  expect_true(is.na(out$f[[2]]))
  expect_null(out$l[[2]])
  expect_identical(out$l[[1]], 1)
})

test_that("updates keep unlisted columns and honour explicit missing values", {
  main <- tibble(id = 1L, v = 5, w = "kept")

  keep <- tibble(
    id = 1L, .op = "update", .cols = list(character(0)),
    v = NA_real_, w = NA_character_
  )
  out <- merge_rows(main, keep, .by = "id")
  expect_identical(out$v, 5)
  expect_identical(out$w, "kept")

  overwrite <- tibble(
    id = 1L, .op = "update", .cols = list("v"),
    v = NA_real_, w = NA_character_
  )
  out <- merge_rows(main, overwrite, .by = "id")
  expect_true(is.na(out$v))
  expect_identical(out$w, "kept")
})

test_that("the same key may appear multiple times across operations", {
  main <- tibble(id = 1L, v = 1)
  staging <- tibble(
    id = c(1L, 1L, 1L, 2L, 2L),
    .op = c("update", "delete", "insert", "insert", "update"),
    .cols = list("v", character(0), "v", "v", "v"),
    v = c(9, NA, 7, 3, 4)
  )

  out <- merge_rows(main, staging, .by = "id")

  expect_identical(out$id, c(1L, 2L))
  expect_identical(out$v, c(7, 4))
})

test_that("an empty batch returns an unchanged tibble", {
  main <- tibble(id = 1:3, v = c("a", "b", "c"))
  empty <- tibble(
    id = integer(0), .op = character(0), .cols = list(),
    v = character(0)
  )
  expect_identical(merge_rows(main, empty, .by = "id"), main)
})

test_that("inserts work against an empty main table", {
  main <- tibble(id = integer(0), v = numeric(0))
  staging <- tibble(
    id = 1:2, .op = c("insert", "insert"),
    .cols = list("v", "v"), v = c(1.5, 2.5)
  )
  out <- merge_rows(main, staging, .by = "id")
  expect_identical(out$id, 1:2)
  expect_identical(out$v, c(1.5, 2.5))
  expect_s3_class(out, "tbl_df")
})

test_that("staging values are cast with vctrs and lossy casts are rejected", {
  main <- tibble(id = integer(0), v = integer(0))
  staging <- tibble(
    id = c(1L, 2L),
    .op = c("insert", "insert"),
    .cols = list("v", "v"),
    v = c(1.1, 2)
  )
  cnd <- expect_error(
    merge_rows(main, staging, .by = "id"),
    class = "tibble_error_merge_cast"
  )
  expect_match(conditionMessage(cnd), "row 1")
})

test_that("cast errors locate the first offending row", {
  main <- tibble(
    id = integer(0),
    v = factor(character(0), levels = c("a", "b"))
  )
  staging <- tibble(
    id = c(1L, 2L),
    .op = c("insert", "insert"),
    .cols = list("v", "v"),
    v = c("a", "zzz")
  )
  cnd <- expect_error(
    merge_rows(main, staging, .by = "id"),
    class = "tibble_error_merge_cast"
  )
  expect_match(conditionMessage(cnd), "row 2")
  expect_s3_class(cnd$parent, "vctrs_error")
})

test_that("custom operation and column-list names are supported", {
  main <- tibble(id = c(1L, 2L), v = c(1, 2))
  staging <- tibble(
    id = c(1L, 3L),
    action = c("update", "insert"),
    supplied = list("v", "v"),
    v = c(42, 7)
  )
  out <- merge_rows(
    main, staging, .by = "id",
    .op = "action", .cols = "supplied"
  )
  expect_identical(out$v, c(42, 2, 7))
})

# Validation --------------------------------------------------------------

test_that("inserts require an absent key and updates/deletes a present one", {
  main <- tibble(id = c(1L, 2L), v = c(1, 2))

  expect_error(
    merge_rows(
      main,
      tibble(id = 1L, .op = "insert", .cols = list(character(0)), v = 1),
      .by = "id"
    ),
    "row 1.*already exists",
    class = "tibble_error_merge_insert_exists"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 9L, .op = "update", .cols = list("v"), v = 1),
      .by = "id"
    ),
    "row 1.*does not exist",
    class = "tibble_error_merge_change_missing"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 9L, .op = "delete", .cols = list(character(0)), v = NA_real_),
      .by = "id"
    ),
    "row 1.*does not exist",
    class = "tibble_error_merge_change_missing"
  )
})

test_that("missing and duplicate keys are rejected", {
  expect_error(
    merge_rows(
      tibble(id = c(1L, NA_integer_), v = c(1, 2)),
      tibble(id = 3L, .op = "insert", .cols = list(character(0)), v = 1),
      .by = "id"
    ),
    class = "tibble_error_merge_main_key_na"
  )
  expect_error(
    merge_rows(
      tibble(id = c(1L, 1L), v = c(1, 2)),
      tibble(id = 3L, .op = "insert", .cols = list(character(0)), v = 1),
      .by = "id"
    ),
    "row 2",
    class = "tibble_error_merge_main_key_duplicate"
  )
  expect_error(
    merge_rows(
      tibble(id = 1L, v = 1),
      tibble(id = NA_integer_, .op = "insert", .cols = list(character(0)), v = 1),
      .by = "id"
    ),
    "row 1",
    class = "tibble_error_merge_changes_key_na"
  )
})

test_that("operations are validated and located by row", {
  main <- tibble(id = 1L, v = 1)

  expect_error(
    merge_rows(
      main,
      tibble(id = 2L, .op = "upsert", .cols = list(character(0)), v = 1),
      .by = "id"
    ),
    "row 1",
    class = "tibble_error_merge_op_invalid"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 2L, .op = NA_character_, .cols = list(character(0)), v = 1),
      .by = "id"
    ),
    "row 1",
    class = "tibble_error_merge_op_missing"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 2L, .op = 1L, .cols = list(character(0)), v = 1),
      .by = "id"
    ),
    class = "tibble_error_merge_op_not_character"
  )
})

test_that("column lists are validated and located by row", {
  main <- tibble(id = 1L, v = 1, w = 2)

  expect_error(
    merge_rows(
      main,
      tibble(id = 1L, .op = "update", .cols = list("nope"), v = 1, w = 1),
      .by = "id"
    ),
    "row 1",
    class = "tibble_error_merge_cols_unknown"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 1L, .op = "update", .cols = list("v"), w = 1),
      .by = "id"
    ),
    "row 1",
    class = "tibble_error_merge_cols_absent"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 1L, .op = "delete", .cols = list("v"), v = 1, w = 1),
      .by = "id"
    ),
    "row 1",
    class = "tibble_error_merge_delete_with_cols"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 1L, .op = "update", .cols = list(1), v = 1, w = 1),
      .by = "id"
    ),
    "row 1",
    class = "tibble_error_merge_cols_invalid"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 1L, .op = "update", .cols = list(c("v", "v")), v = 1, w = 1),
      .by = "id"
    ),
    "row 1",
    class = "tibble_error_merge_cols_duplicate"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 1L, .op = "update", .cols = "v", v = 1, w = 1),
      .by = "id"
    ),
    class = "tibble_error_merge_cols_not_list"
  )
})

test_that("staging columns and key arguments are validated", {
  main <- tibble(id = 1L, v = 1)

  expect_error(
    merge_rows(
      main,
      tibble(id = 2L, .op = "insert", .cols = list(character(0)), v = 1, z = 1),
      .by = "id"
    ),
    "`z`",
    class = "tibble_error_merge_unknown_columns"
  )
  expect_error(
    merge_rows(main, tibble(id = 1L), .by = "nope"),
    class = "tibble_error_merge_missing_columns"
  )
  expect_error(
    merge_rows(main, tibble(id = 1L), .by = character(0)),
    class = "tibble_error_merge_by_invalid"
  )
  expect_error(
    merge_rows(main, tibble(id = 1L), .by = c("id", "id")),
    class = "tibble_error_merge_by_duplicate"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 1L, .op = "x", .cols = list(character(0)), v = 1),
      .by = "id", .op = "id"
    ),
    class = "tibble_error_merge_control_conflict"
  )
  expect_error(
    merge_rows(1, tibble(id = 1L), .by = "id"),
    class = "tibble_error_merge_not_data_frame"
  )
  expect_error(
    merge_rows(tibble(id = 1L), 1, .by = "id"),
    class = "tibble_error_merge_not_data_frame"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 1L, .op = "x", .cols = list(character(0)), v = 1),
      .by = "id", .op = c("a", "b")
    ),
    class = "tibble_error_merge_control_not_string"
  )
  expect_error(
    merge_rows(
      main,
      tibble(id = 1L, .op = "update", .cols = list(NA_character_), v = 1),
      .by = "id"
    ),
    "row 1",
    class = "tibble_error_merge_cols_na"
  )
})

test_that("a failing batch does not touch the main table", {
  main <- tibble(id = c(1L, 2L), v = c(1, 2))
  staging <- tibble(
    id = c(2L, 9L),
    .op = c("delete", "update"),
    .cols = list(character(0), "v"),
    v = c(NA, 5)
  )
  expect_error(merge_rows(main, staging, .by = "id"))
  expect_identical(main, tibble(id = c(1L, 2L), v = c(1, 2)))
})
