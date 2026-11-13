# --- Pipeline code string (internal) ------------------------------------------

# Builds the `from_*() |> find_*()` code string. Shared by the interactive
# guide and the Shiny app. `inputs` is a named list: `interaction`, `ci`, and
# `power` (for goal "n"), plus the source's inputs (`sd_y` and `r_squared`,
# `se_existing` and `n_existing`, or `se_pilot` and `n_pilot`) and the goal's
# (`n_planned` for "mde" and "power"; `tau` for "power" and "n"). Arguments
# left at their defaults are omitted from the code.
build_pipeline_code <- function(source_type, goal_type, inputs) {
  ci <- if (!is.null(inputs$ci)) inputs$ci else 0.90
  power <- if (!is.null(inputs$power)) inputs$power else 0.80
  r2 <- if (!is.null(inputs$r_squared)) inputs$r_squared else 0

  int_arg <- if (isTRUE(inputs$interaction)) ", interaction = TRUE" else ""
  ci_arg <- if (ci != 0.90) paste0(", ci = ", fmt_val(ci)) else ""

  source_call <- switch(source_type,
    sd = paste0(
      "from_sd(sd_y = ", fmt_val(inputs$sd_y),
      if (r2 > 0) paste0(", r_squared = ", fmt_val(r2)), int_arg, ")"
    ),
    existing = paste0(
      "from_existing(se_existing = ", fmt_val(inputs$se_existing),
      ", n_existing = ", fmt_val(inputs$n_existing), int_arg, ")"
    ),
    pilot = paste0(
      "from_pilot(se_pilot = ", fmt_val(inputs$se_pilot),
      ", n_pilot = ", fmt_val(inputs$n_pilot), int_arg, ")"
    )
  )

  n_name <- if (source_type == "sd") "n" else "n_planned"

  goal_call <- switch(goal_type,
    mde = paste0("find_mde(", n_name, " = ", fmt_val(inputs$n_planned),
                 ci_arg, ")"),
    power = paste0("find_power(", n_name, " = ", fmt_val(inputs$n_planned),
                   ", tau = ", fmt_val(inputs$tau), ci_arg, ")"),
    n = paste0(
      "find_n(tau = ", fmt_val(inputs$tau),
      if (power != 0.80) paste0(", power = ", fmt_val(power)), ci_arg, ")"
    )
  )

  paste0(source_call, " |> ", goal_call)
}
