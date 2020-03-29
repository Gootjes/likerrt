context("scale")

library(tidyverse)
library(rlang)


n1 <- 100
values1 <- 1:10
gen <- function(values, n) sample(values, n, replace=TRUE, prob=rbeta(length(values), 1, 1))
d1 <- data.frame(A = gen(values1, n1), B = gen(values1, n1), C = gen(values1, n1)) %>%
  as_likert(A, B, C, .labels = c("lowest" = 1, 2, 3, 4, 5, 6, 7, 8, 9, "highest"=10))


i_na <- sort(sample(1:nrow(d1), size = nrow(d1)/4, replace = FALSE))

d2 <- d1
d2$A[i_na] <- NA


a <- d1 %>%
  likert_scale(A, B, .name = "D") %>%
  pluck("D")

b <- rowMeans(data.frame(d1$A, d1$B))

testthat::expect_equal(object = a, expected = b)


a <- d1 %>%
  likert_reverse_code(C, .suffix = "r") %>%
  likert_scale(
    A,
    B,
    Cr,
    .name = "D",
    .label = "D measure",
    .drop = TRUE
  )

b <- structure(rowMeans(data.frame(d1$A, d1$B, 11-d1$C)), label = "D measure")

testthat::expect_equal(object = a, expected = b)


a <- d1  %>%
  likert_reverse_code(C, .suffix = "r") %>%
  likert_scale(
    A,
    B,
    Cr,
    .name = "D",
    .label = "D measure",
    .drop = FALSE
  )

b <- d1 %>% likert_reverse_code(C, .suffix = "r") %>% mutate(D = rowMeans(data.frame(.$A, .$B, .$Cr)))
attributes(b$D) <- list(label = "D measure")

testthat::expect_equal(a, b)


a <- d1 %>%
  likert_reverse_code(C) %>%
  likert_scale(
    A,
    B,
    C,
    .name = "D",
    .label = "D measure",
    .drop = FALSE
  )

b <- d1 %>% likert_reverse_code(C) %>% mutate(D = rowMeans(data.frame(.$A, .$B, .$C)))
attributes(b$D) <- list(label = "D measure")

testthat::expect_equal(a, b)


a <- d2 %>%
  likert_reverse_code(C) %>%
  likert_scale(
    A,
    B,
    C,
    .name = "D",
    .label = "D measure",
    .drop = FALSE,
    na.rm = TRUE
  )

b <- d2 %>% likert_reverse_code(C) %>% mutate(D = rowMeans(data.frame(.$A, .$B, .$C), na.rm = TRUE))
attributes(b$D) <- list(label = "D measure")

testthat::expect_equal(a, b)


a <- d2 %>%
  likert_reverse_code(C) %>%
  likert_scale(
    A,
    B,
    C,
    .name = "D",
    .label = "D measure",
    .drop = FALSE,
    na.rm = FALSE
  )

b <- d2 %>% likert_reverse_code(C) %>% mutate(D = rowMeans(data.frame(.$A, .$B, .$C), na.rm = FALSE))
attributes(b$D) <- list(label = "D measure")

testthat::expect_equal(a, b)



testthat::expect_warning(
  object =
    d1 %>% set_labels(B, .value = c("Not at all" = 1, "A lot" = 10)) %>%
    likert_scale(A, B, .name = "C", .label = "Scale of A and B")
)


testthat::expect_warning(
  object =
    data.frame(A = c(1:7), B = 1:7) %>%
    as_likert(A, .labels = c(
      "Disagree" = 1, "Agree" = 7
    )) %>%
    as_likert(B, .labels = c(
      "Not at all" = 1, "A lot" = 7
    )) %>%
    likert_scale(A, B, .name = "C", .label = "Scale of A and B")
)

testthat::expect_equal(
  object = data.frame(A = c(1:7), B = 1:7) %>%
    as_likert(A, .labels = c(
      "Disagree" = 1, "Agree" = 7
    )) %>%
    as_likert(B, .labels = c(
      "Not at all" = 1, "A lot" = 7
    )) %>%
    likert_scale(
      A,
      B,
      .name = "C",
      .label = "Scale of A and B",
      .assumptions = "values"
    ),
  expected = structure(
    list(
      A = structure(
        1:7,
        labels = structure(c(
          Disagree = 1,
          `2` = 2,
          `3` = 3,
          `4` = 4,
          `5` = 5,
          `6` = 6,
          Agree = 7
        ), class = "likerrt_labels"),
        class = c("likerrt_likert",
                  "haven_labelled", "integer")
      ),
      B = structure(
        1:7,
        labels = structure(
          c(
            `Not at all` = 1,
            `2` = 2,
            `3` = 3,
            `4` = 4,
            `5` = 5,
            `6` = 6,
            `A lot` = 7
          ),
          class = "likerrt_labels"
        ),
        class = c("likerrt_likert",
                  "haven_labelled", "integer")
      ),
      C = structure(c(1, 2, 3, 4, 5,
                      6, 7), label = "Scale of A and B")
    ),
    row.names = c(NA, -7L),
    class = "data.frame"
  )
)

testthat::expect_warning(
  object =
    data.frame(A = c(1:7, 1:3), B = 1:10) %>%
    as_likert(A, .labels = c(
      "Disagree" = 1, "Agree" = 7
    )) %>%
    as_likert(B, .labels = c(
      "Not at all" = 1, "A lot" = 10
    )) %>%
    likert_scale(
      A,
      B,
      .name = "C",
      .label = "Scale of A and B",
      .assumptions = "range"
    )
)

testthat::expect_equal(
  object =
    data.frame(A = c(1:7, 1:3), B = 1:10) %>%
    as_likert(A, .labels = c(
      "Disagree" = 1, "Agree" = 7
    )) %>%
    as_likert(B, .labels = c(
      "Not at all" = 1, "A lot" = 10
    )) %>%
    likert_scale(
      A,
      B,
      .name = "C",
      .label = "Scale of A and B",
      .assumptions = "none"
    ),
  expected = structure(
    list(
      A = structure(
        c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 1L,
          2L, 3L),
        labels = structure(c(
          Disagree = 1,
          `2` = 2,
          `3` = 3,
          `4` = 4,
          `5` = 5,
          `6` = 6,
          Agree = 7
        ), class = "likerrt_labels"),
        class = c("likerrt_likert",
                  "haven_labelled", "integer")
      ),
      B = structure(
        1:10,
        labels = structure(
          c(
            `Not at all` = 1,
            `2` = 2,
            `3` = 3,
            `4` = 4,
            `5` = 5,
            `6` = 6,
            `7` = 7,
            `8` = 8,
            `9` = 9,
            `A lot` = 10
          ),
          class = "likerrt_labels"
        ),
        class = c("likerrt_likert",
                  "haven_labelled", "integer")
      ),
      C = structure(c(1, 2, 3, 4, 5,
                      6, 7, 4.5, 5.5, 6.5), label = "Scale of A and B")
    ),
    row.names = c(NA,-10L),
    class = "data.frame"
  )
)

testthat::expect_equal(
  object =
    data.frame(A = c(1:7, 1:3), B = 1:10) %>%
    as_likert(A, .labels = c(
      "Disagree" = 1, "Agree" = 7
    )) %>%
    as_likert(B, .labels = c(
      "Not at all" = 1, "A lot" = 10
    )) %>%
    likert_rescale(B, .min = 1, .max = 7) %>%
    likert_scale(
      A,
      B,
      .name = "C",
      .label = "Scale of A and B",
      .assumptions = "range"
    ),
  expected = structure(
    list(
      A = structure(
        c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 1L,
          2L, 3L),
        labels = structure(c(
          Disagree = 1,
          `2` = 2,
          `3` = 3,
          `4` = 4,
          `5` = 5,
          `6` = 6,
          Agree = 7
        ), class = "likerrt_labels"),
        class = c("likerrt_likert",
                  "haven_labelled", "integer")
      ),
      B = structure(
        c(
          1,
          1.66666666666667,
          2.33333333333333,
          3,
          3.66666666666667,
          4.33333333333333,
          5,
          5.66666666666667,
          6.33333333333333,
          7
        ),
        labels = structure(
          c(
            `Not at all` = 1,
            `2` = 1.66666666666667,
            `3` = 2.33333333333333,
            `4` = 3,
            `5` = 3.66666666666667,
            `6` = 4.33333333333333,
            `7` = 5,
            `8` = 5.66666666666667,
            `9` = 6.33333333333333,
            `A lot` = 7
          ),
          class = "likerrt_labels"
        ),
        class = c("likerrt_likert",
                  "haven_labelled", "integer")
      ),
      C = structure(
        c(
          1,
          1.83333333333333,
          2.66666666666667,
          3.5,
          4.33333333333333,
          5.16666666666667,
          6,
          3.33333333333333,
          4.16666666666667,
          5
        ),
        label = "Scale of A and B"
      )
    ),
    row.names = c(NA,-10L),
    class = "data.frame"
  )
)

testthat::expect_error(
  object =
    d1 %>%
    likert_scale(
      A,
      .name = "C",
      .label = "Scale of A and B",
      .assumptions = "none"
    )
)
