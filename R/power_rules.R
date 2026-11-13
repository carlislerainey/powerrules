#' Interactive power analysis guide
#'
#' Walks through an interactive session to build a power analysis pipeline.
#' The guide asks questions about your design, collects inputs, runs the
#' analysis, and prints the exact pipeline code you can copy into a script.
#'
#' @return The result of the pipeline (a `"power_result"` object),
#'   returned invisibly.
#' @export
power_rules <- function() {

  check_interactive()

  # --- Step 1: Welcome ---
  cat("\n")
  cat(rule_line("Power Rules"), "\n\n")
  cat("  Welcome! Let's work through a power analysis together.\n")
  cat("  I'll ask a few questions, show the calculation step by step,\n")
  cat("  and give you a pipeline you can save to your script.\n")

  # --- Step 2: What are you testing? ---
  ask_divider()
  design <- ask_menu(
    "What are you testing?",
    c(a = "A treatment effect (two-group comparison)",
      b = "An interaction (2x2 factorial)")
  )
  interaction <- (design == "b")

  # --- Step 3: What do you want to find out? ---
  ask_divider()
  goal <- ask_menu(
    "What do you want to find out?",
    c(a = "MDE -- I know my sample size; what's the smallest effect I can detect?",
      b = "Power -- I know my sample size and the effect; what's my power?",
      c = "Sample size -- I know the effect; how many respondents do I need?")
  )
  goal_type <- switch(goal, a = "mde", b = "power", c = "n")

  # --- Step 4: What information do you have? ---
  ask_divider()
  info <- ask_menu(
    "What information do you have about the SE?",
    c(a = "Reference population -- I have an estimate of SD(Y)",
      b = "Existing study -- I have the SE and n from a published study",
      c = "Pilot data -- I have the SE and n from my own pilot")
  )
  source_type <- switch(info, a = "sd", b = "existing", c = "pilot")

  # --- Step 5: Confidence interval ---
  ask_divider()
  ci_choice <- ask_menu(
    "What confidence interval level?",
    c(a = "90% CI (size-0.05 test) -- recommended, the default in the paper",
      b = "95% CI (size-0.025 test)")
  )
  ci <- if (ci_choice == "a") 0.90 else 0.95

  # --- Step 6: Power level (only for sample size goal) ---
  power_val <- NULL
  if (goal_type == "n") {
    ask_divider()
    pow_choice <- ask_menu(
      "What power level do you want?",
      c(a = "80%",
        b = "90%",
        c = "95%",
        d = "Custom")
    )
    if (pow_choice == "d") {
      # Power cannot be at or below the size of the test
      size <- fmt_val(test_size(ci))
      repeat {
        power_val <- ask_power(
          "Enter your desired power level (e.g., 0.85 for 85%):",
          hint = paste0("Must be between ", size, " and 1")
        )
        if (power_val > test_size(ci) + 1e-8) break
        cat("  Please enter a number greater than ", size,
            " (the size of the test).\n", sep = "")
      }
    } else {
      power_val <- switch(pow_choice, a = 0.80, b = 0.90, c = 0.95)
    }
  }

  # --- Step 7: Collect numeric inputs ---
  ask_divider()
  cat("\n  Now I need some numbers. I'll explain each one.\n")

  # Source-specific inputs
  sd_y <- r_squared <- se_existing <- n_existing <- se_pilot <- n_pilot <- NULL
  n_planned <- tau <- NULL

  if (source_type == "sd") {
    sd_y <- ask_number(
      "What is the standard deviation of the outcome, SD(Y)?",
      hint = "This is the SD in a reference population similar to your sample"
    )

    cat("\n  Do you have control variables that predict the outcome?\n")
    cat("  If so, the adjustment can shrink the SE and improve power.\n\n")
    r2_choice <- ask_menu(
      "Do you want to adjust for covariates?",
      c(a = "No adjustment (R-squared = 0)",
        b = "Yes, I have an estimate of R-squared")
    )
    r_squared <- 0
    if (r2_choice == "b") {
      r_squared <- ask_r_squared(
        "What is the R-squared of your control variables?",
        hint = "A number between 0 and 1, e.g., 0.05 for 5%"
      )
    }

  } else if (source_type == "existing") {
    se_existing <- ask_number(
      "What is the standard error from the existing study?",
      hint = "This is the SE of the treatment effect estimate"
    )
    n_existing <- ask_number(
      "How many respondents per condition in the existing study?",
      hint = "The number in each arm, not the total"
    )

  } else {
    se_pilot <- ask_number(
      "What is the standard error from your pilot study?",
      hint = "This is the SE of the treatment effect estimate"
    )
    n_pilot <- ask_number(
      "How many respondents per condition in the pilot?",
      hint = "The number in each arm, not the total"
    )
  }

  # Goal-specific inputs
  if (goal_type %in% c("mde", "power")) {
    repeat {
      n_planned <- ask_number(
        "How many respondents per condition in the planned study?",
        hint = "The number in each arm, not the total"
      )
      if (abs(n_planned - round(n_planned)) < 1e-8) break
      cat("  Please enter a whole number.\n")
    }
  }

  if (goal_type %in% c("power", "n")) {
    effect_name <- if (interaction) "interaction" else "treatment effect"
    tau <- ask_number(
      paste0("What is the assumed ", effect_name, " (tau)?"),
      hint = "The smallest effect you consider meaningful, in the units of the outcome"
    )
  }

  # --- Step 8: Walk through the calculation ---
  ask_divider()
  cat("\n  Great! Let me work through this...\n\n")

  # Build and run the pipeline
  source_obj <- switch(source_type,
    sd = from_sd(sd_y = sd_y, r_squared = r_squared,
                 interaction = interaction),
    existing = from_existing(se_existing = se_existing,
                             n_existing = n_existing,
                             interaction = interaction),
    pilot = from_pilot(se_pilot = se_pilot, n_pilot = n_pilot,
                       interaction = interaction)
  )

  result <- switch(goal_type,
    mde = find_mde(source_obj, n_planned = n_planned, ci = ci, quiet = FALSE),
    power = find_power(source_obj, n_planned = n_planned, tau = tau, ci = ci,
                       quiet = FALSE),
    n = find_n(source_obj, tau = tau, power = power_val, ci = ci,
               quiet = FALSE)
  )

  # --- Step 9: Print the pipeline ---
  ask_divider()
  cat(rule_line("Save this to your script"), "\n")

  inputs <- list(
    sd_y = sd_y, r_squared = r_squared,
    se_existing = se_existing, n_existing = n_existing,
    se_pilot = se_pilot, n_pilot = n_pilot,
    n_planned = n_planned, tau = tau,
    interaction = interaction, ci = ci, power = power_val
  )
  pipeline <- build_pipeline_code(source_type, goal_type, inputs)
  cat(pipeline, "\n")

  # --- Step 10: Return ---
  invisible(result)
}
