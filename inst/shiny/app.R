library(shiny)
library(powerrules)
source("helpers.R", local = TRUE)

# --- UI helpers -------------------------------------------------------------

inline_select <- function(inputId, choices, selected = NULL) {
  opts <- mapply(function(label, value) {
    if (!is.null(selected) && value == selected) {
      tags$option(value = value, selected = NA, label)
    } else {
      tags$option(value = value, label)
    }
  }, names(choices), unname(choices), SIMPLIFY = FALSE, USE.NAMES = FALSE)

  tags$span(
    class = "form-group shiny-input-container inline-select-container",
    tags$select(id = inputId, class = "form-control inline-select", opts)
  )
}

# One row of the inputs table: name | value | description. `name` is the
# label text, or a tag to use in its place; `...` follows the input box.
input_row <- function(name, inputId, desc, ..., width = "120px") {
  div(class = "input-row",
    if (is.character(name)) span(class = "input-name", name) else name,
    div(class = "input-value",
      numericInput(inputId, label = NULL, value = NA, step = NA,
                   width = width),
      ...
    ),
    span(class = "input-desc", desc)
  )
}

# --- UI ---------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "board.css"),
    tags$script(HTML("
      function copyText(elementId) {
        var el = document.getElementById(elementId);
        var text = el.innerText;
        var btn = el.parentElement.querySelector('.copy-btn');
        navigator.clipboard.writeText(text).then(function() {
          var orig = btn.innerText;
          btn.innerText = 'Copied!';
          setTimeout(function() { btn.innerText = orig; }, 1500);
        });
      }

      // Set placeholders on numeric inputs after Shiny initializes
      $(document).on('shiny:connected', function() {
        $('#sd_y').attr('placeholder', 'e.g., 20.8');
        $('#r_squared').attr('placeholder', 'e.g., 0.05');
        $('#se_existing').attr('placeholder', 'e.g., 1.8');
        $('#n_existing').attr('placeholder', 'per condition');
        $('#se_pilot').attr('placeholder', 'e.g., 2.13');
        $('#n_pilot').attr('placeholder', 'per condition');
        $('#n_planned').attr('placeholder', 'per condition');
        $('#tau').attr('placeholder', 'effect size');
        $('#custom_power').attr('placeholder', 'e.g., 0.85');
      });
    "))
  ),

  div(class = "board-container",
    div(class = "board",

      # --- Title ---
      div(class = "board-title", "Power Rules"),
      tags$hr(class = "title-rule"),

      # --- Problem statement ---
      div(class = "problem-statement",
        "I have ",
        inline_select("source_type",
          c("an estimate of SD(Y)" = "sd",
            "the SE from an existing study" = "existing",
            "the SE from my pilot" = "pilot"),
          selected = "sd"),
        " and I want to find ",
        inline_select("goal_type",
          c("the MDE" = "mde",
            "the power" = "power",
            "the sample size" = "n"),
          selected = "mde"),
        " for ",
        inline_select("design_type",
          c("a treatment effect" = "treatment",
            "an interaction" = "interaction"),
          selected = "treatment"),
        "."
      ),

      tags$hr(class = "section-rule"),

      # --- Settings ---
      div(class = "settings-section",
        conditionalPanel("input.goal_type == 'n'",
          radioButtons("power_choice", "Power level:",
            choices = c("80%" = "0.80", "90%" = "0.90",
                        "95%" = "0.95", "Custom" = "custom"),
            selected = "0.80", inline = TRUE),
          conditionalPanel("input.power_choice == 'custom'",
            span(class = "custom-power-wrapper",
              numericInput("custom_power", label = NULL, value = NA,
                           min = 0.01, max = 0.99, step = 0.01,
                           width = "80px")
            )
          )
        )
      ),

      tags$hr(class = "section-rule"),

      # --- Numeric inputs (table: name | value | description) ---
      div(class = "inputs-section",

        # from_sd
        conditionalPanel("input.source_type == 'sd'",
          input_row("SD(Y) =", "sd_y",
            "SD of the outcome in a population like yours, e.g., from a census or prior study"),
          input_row("R\u00b2 =", "r_squared",
            "proportion of variance explained by covariates; leave blank for no adjustment",
            width = "80px")
        ),

        # from_existing
        conditionalPanel("input.source_type == 'existing'",
          input_row("SE (existing) =", "se_existing",
            "standard error of the treatment effect from that study"),
          input_row("n (existing) =", "n_existing",
            "number of respondents per condition in that study")
        ),

        # from_pilot
        conditionalPanel("input.source_type == 'pilot'",
          input_row("SE (pilot) =", "se_pilot",
            "standard error of the treatment effect from that study"),
          input_row("n (pilot) =", "n_pilot",
            "number of respondents per condition in that study")
        ),

        # n_planned (for find_mde and find_power)
        conditionalPanel(
          "input.goal_type == 'mde' || input.goal_type == 'power'",
          input_row(uiOutput("n_label", container = span, class = "input-name"),
            "n_planned",
            "number of respondents per condition in the planned study",
            uiOutput("total_annotation", inline = TRUE))
        ),

        # tau (for find_power and find_n)
        conditionalPanel(
          "input.goal_type == 'power' || input.goal_type == 'n'",
          input_row("tau =", "tau",
            "the smallest treatment effect you'd consider meaningful, in units of the outcome")
        )
      ),

      # --- Input problem (shown instead of a silently wrong or blank result) ---
      uiOutput("problem_box"),

      tags$hr(class = "section-rule"),

      # --- Computation ---
      div(class = "computation-section",
        uiOutput("computation")
      ),

      # --- Result highlight ---
      uiOutput("result_highlight"),

      tags$hr(class = "section-rule"),

      # --- Manuscript sentence ---
      uiOutput("sentence_box"),

      # --- Note ---
      uiOutput("note_box"),

      tags$hr(class = "section-rule"),

      # --- Pipeline ---
      uiOutput("pipeline_box")
    )
  )
)

# --- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  ci <- 0.90  # the board always uses 90% confidence intervals

  # --- Derived reactives ---

  interaction <- reactive(input$design_type == "interaction")

  # R-squared counts only when it is a usable proportion; blank means none
  use_r2 <- reactive({
    val <- input$r_squared
    input$source_type == "sd" &&
      !is.null(val) && !is.na(val) && is.numeric(val) && val > 0 && val < 1
  })

  # NULL while a custom power level is blank or out of range
  power_val <- reactive({
    if (input$goal_type != "n") return(0.80)
    if (input$power_choice == "custom") {
      val <- input$custom_power
      if (!is.null(val) && !is.na(val) && val > 0 && val < 1) return(val)
      return(NULL)
    }
    as.numeric(input$power_choice)
  })

  # --- Input problems the board explains rather than ignoring ---
  # An out-of-range value (e.g., R-squared typed as 5 for 5%) must not be
  # silently dropped, so it blocks the result and shows a message instead.

  input_problem <- reactive({
    st <- input$source_type
    gt <- input$goal_type

    r2 <- input$r_squared
    if (st == "sd" && is.numeric(r2) && !is.na(r2) && (r2 < 0 || r2 >= 1))
      return(paste0("R\u00b2 must be a proportion between 0 and 1 ",
                    "(for example, 0.05 for 5%)."))

    if (gt == "n" && input$power_choice == "custom") {
      cp <- input$custom_power
      test_size <- (1 - ci) / 2
      if (is.numeric(cp) && !is.na(cp) &&
          (cp <= test_size + 1e-8 || cp >= 1))
        return(paste0("Power must be a proportion between ",
                      fmt_val(test_size),
                      " and 1 (for example, 0.85 for 85%)."))
    }

    if (gt %in% c("mde", "power")) {
      n <- input$n_planned
      if (is.numeric(n) && !is.na(n) && n > 0 && abs(n - round(n)) > 1e-8)
        return("The sample size per condition must be a whole number.")
    }

    NULL
  })

  # --- Collect inputs (NULL for a blank or non-positive value) ---

  vp <- function(x) {
    if (is.null(x) || is.na(x) || !is.numeric(x) || x <= 0) NULL else x
  }

  collect_inputs <- reactive({
    list(
      sd_y        = vp(input$sd_y),
      r_squared   = if (use_r2()) input$r_squared,
      se_existing = vp(input$se_existing),
      n_existing  = vp(input$n_existing),
      se_pilot    = vp(input$se_pilot),
      n_pilot     = vp(input$n_pilot),
      n_planned   = vp(input$n_planned),
      tau         = vp(input$tau)
    )
  })

  # --- Check if all required inputs are present ---

  inputs_complete <- reactive({
    if (!is.null(input_problem())) return(FALSE)

    inp <- collect_inputs()
    src_ok <- switch(input$source_type,
      sd       = !is.null(inp$sd_y),
      existing = !is.null(inp$se_existing) && !is.null(inp$n_existing),
      pilot    = !is.null(inp$se_pilot) && !is.null(inp$n_pilot)
    )
    goal_ok <- switch(input$goal_type,
      mde   = !is.null(inp$n_planned),
      power = !is.null(inp$n_planned) && !is.null(inp$tau),
      n     = !is.null(inp$tau) && !is.null(power_val())
    )
    src_ok && goal_ok
  })

  # --- Compute the result (NULL until the inputs are complete) ---

  result <- reactive({
    if (!inputs_complete()) return(NULL)
    inp <- collect_inputs()
    int <- interaction()

    tryCatch({
      src <- switch(input$source_type,
        sd = from_sd(sd_y = inp$sd_y,
                     r_squared = if (is.null(inp$r_squared)) 0 else inp$r_squared,
                     interaction = int),
        existing = from_existing(se_existing = inp$se_existing,
                                 n_existing = inp$n_existing,
                                 interaction = int),
        pilot = from_pilot(se_pilot = inp$se_pilot, n_pilot = inp$n_pilot,
                           interaction = int)
      )
      switch(input$goal_type,
        mde = find_mde(src, n_planned = inp$n_planned, ci = ci,
                       quiet = TRUE),
        power = find_power(src, n_planned = inp$n_planned, tau = inp$tau,
                           ci = ci, quiet = TRUE),
        n = find_n(src, tau = inp$tau, power = power_val(), ci = ci,
                   quiet = TRUE)
      )
    }, error = function(e) NULL)
  })

  # --- Outputs ---

  # n input label: "n =" for SD source, "n (planned) =" for existing/pilot
  output$n_label <- renderUI({
    if (input$source_type == "sd") "n =" else "n (planned) ="
  })

  # Total annotation
  output$total_annotation <- renderUI({
    val <- vp(input$n_planned)
    if (!is.null(val)) {
      n_cond <- if (interaction()) 4 else 2
      total <- n_cond * val
      tags$span(class = "total-annotation",
                paste0("(", format(total, big.mark = ",",
                                   scientific = FALSE),
                       " total)"))
    }
  })

  # Input problem message
  output$problem_box <- renderUI({
    msg <- input_problem()
    if (!is.null(msg)) div(class = "problem-box", msg)
  })

  # Computation section: formulas show blanks until their values arrive
  output$computation <- renderUI({
    pv <- power_val()
    if (is.null(pv)) pv <- 0.80

    HTML(render_computation(input$source_type, input$goal_type,
                            interaction(), ci, pv, collect_inputs(),
                            use_r2(), result()))
  })

  # Everything below appears only once there is a result

  output$result_highlight <- renderUI({
    res <- result()
    if (!is.null(res))
      HTML(render_result_highlight(res, input$goal_type, interaction()))
  })

  output$sentence_box <- renderUI({
    res <- result()
    if (!is.null(res)) HTML(render_sentence(res))
  })

  output$note_box <- renderUI({
    res <- result()
    if (is.null(res)) return(NULL)
    html <- render_note(res)
    if (nzchar(html)) HTML(html)
  })

  output$pipeline_box <- renderUI({
    res <- result()
    if (!is.null(res))
      HTML(render_pipeline(input$source_type, input$goal_type, interaction(),
                           ci, power_val(), collect_inputs()))
  })
}

shinyApp(ui, server)
