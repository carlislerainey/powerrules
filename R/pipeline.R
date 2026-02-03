#' Build pipeline code string
#'
#' Constructs the pipeline code string from source type, goal type, and
#' inputs. Used by the interactive guide and the Shiny app.
#'
#' @param source_type Character: "sd", "existing", or "pilot".
#' @param goal_type Character: "mde", "power", or "n".
#' @param inputs Named list with the relevant input values. Expected fields
#'   depend on source and goal type. Always expects `interaction`
#'   (logical), `ci` (numeric), and `power` (numeric, for
#'   goal_type "n"). Source-specific: `sd_y`, `r_squared` (sd);
#'   `se_existing`, `n_existing` (existing); `se_pilot`,
#'   `n_pilot` (pilot). Goal-specific: `n_planned` (mde, power);
#'   `tau` (power, n).
#' @return A character string containing the pipeline code.
#' @keywords internal
#' @export
build_pipeline_code <- function(source_type, goal_type, inputs) {
  interaction <- isTRUE(inputs$interaction)
  ci <- if (!is.null(inputs$ci)) inputs$ci else 0.90
  power_val <- if (!is.null(inputs$power)) inputs$power else 0.80

  source_call <- switch(source_type,
    sd = {
      args <- paste0("sd_y = ", fmt_val(inputs$sd_y))
      r2 <- if (!is.null(inputs$r_squared)) inputs$r_squared else 0
      if (r2 > 0) args <- paste0(args, ", r_squared = ", fmt_val(r2))
      if (interaction) args <- paste0(args, ", interaction = TRUE")
      paste0("from_sd(", args, ")")
    },
    existing = {
      args <- paste0("se_existing = ", fmt_val(inputs$se_existing),
                     ", n_existing = ", fmt_val(inputs$n_existing))
      if (interaction) args <- paste0(args, ", interaction = TRUE")
      paste0("from_existing(", args, ")")
    },
    pilot = {
      args <- paste0("se_pilot = ", fmt_val(inputs$se_pilot),
                     ", n_pilot = ", fmt_val(inputs$n_pilot))
      if (interaction) args <- paste0(args, ", interaction = TRUE")
      paste0("from_pilot(", args, ")")
    }
  )

  n_arg <- if (source_type == "sd") "n" else "n_planned"

  goal_call <- switch(goal_type,
    mde = {
      args <- paste0(n_arg, " = ", fmt_val(inputs$n_planned))
      if (ci != 0.90) args <- paste0(args, ", ci = ", fmt_val(ci))
      paste0("find_mde(", args, ")")
    },
    power = {
      args <- paste0(n_arg, " = ", fmt_val(inputs$n_planned),
                     ", tau = ", fmt_val(inputs$tau))
      if (ci != 0.90) args <- paste0(args, ", ci = ", fmt_val(ci))
      paste0("find_power(", args, ")")
    },
    n = {
      args <- paste0("tau = ", fmt_val(inputs$tau))
      if (power_val != 0.80) args <- paste0(args, ", power = ", fmt_val(power_val))
      if (ci != 0.90) args <- paste0(args, ", ci = ", fmt_val(ci))
      paste0("find_n(", args, ")")
    }
  )

  paste0(source_call, " |> ", goal_call)
}
