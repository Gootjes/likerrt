context("test of attribute system")

library(tidyverse)

d <- data.frame(A = rep(1:7, 10), B = rep(7:1, 10)) %>%
  as_likert(everything(), .label = "Variable", .labels = c("Disagree"=1, "Neutral"= 4, "Agree"=7), .complement = T)

testthat::expect_equal(object = attributes(d$A)$label, expected = get_label(d)$A)
testthat::expect_equal(object = attributes(attributes(d$A)$label)$class, expected = "likerrt_label")

testthat::expect_equal(object = attributes(d$A)$labels, expected = get_labels(d)$A)
testthat::expect_equal(object = attributes(attributes(d$A)$labels)$class, expected = "likerrt_labels")

testthat::expect_equal(object = levels(as_factor(d$A)), expected = names(get_labels(d$A)))

d %>% set_label(A, .value = "Variable A") %>% set_label(B, .value = "Variable B") -> d

testthat::expect_equal(object = get_label(d$A), expected = structure("Variable A", class = "likerrt_label"))
