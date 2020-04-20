#' @title Density Kerdiest Kernel Learner
#'
#' @name mlr_learners_dens.kdeKD
#'
#' @description
#' A [mlr3::LearnerDens] implementing kdeKD from package
#'   \CRANpkg{kerdiest}.
#' Calls [kerdiest::kde()].
#'
#' @templateVar id dens.kdeKD
#' @template section_dictionary_learner
#'
#' @template seealso_learner
#' @template example
#' @export
# <Adapt the name to your learner. For regression learners inherit = LearnerRegr>
LearnerDensKDEkd = R6Class("LearnerDensKDEkd",
  inherit = LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "bw", lower = 0, tags = "train"),
          ParamFct$new("type_kernel",
            levels = c("n", "e", "t", "b"),
            default = "n", tags = "train")
        )
      )

      super$initialize(
        id = "dens.kdeKD",
        packages = "kerdiest",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "pdf",
        param_set = ps,
        man = "mlr3learners.kerdiest::mlr_learners_dens.kdeKD"
      )
    }
  ),

  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tag = "train")

      data = task$truth()

      pdf <- function(x1) {
      }
      body(pdf) <- substitute({
        invoke(kerdiest::kde, vec_data = data, y = x1, .args = pars)$Estimated_values
      })

      if (is.null(pars$type_kernel)) {
        kernel = "Normal"
      } else {
        kernel = switch(pars$type_kernel,
          "n" = "Normal",
          "e" = "Epanechnikov",
          "b" = "Biweight",
          "t" = "Triweight")
      }

      Distribution$new(
        name = paste("kerdiest KDE", kernel),
        short_name = paste0("kerdiestKDEKern_", kernel),
        pdf = pdf)
    },

    .predict = function(task) {
      newdata = task$truth()

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }
  )
)
