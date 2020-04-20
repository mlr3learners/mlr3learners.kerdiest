library(mlr3learners.kerdiest)

test_that("dens.kdeKD", {
  learner = lrn("dens.kdeKD")
  fun = kerdiest::kde
  exclude = c(
    "vec_data", # handled by predict
    "y" # handled by predict
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("\nMissing parameters:\n",
    paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})
