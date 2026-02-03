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
          div(class = "input-row",
            span(class = "input-name", "SD(Y) ="),
            div(class = "input-value",
              numericInput("sd_y", label = NULL, value = NA,
                           step = NA, width = "120px")
            ),
            span(class = "input-desc",
              "SD of the outcome in a population like yours, e.g., from a census or prior study")
          ),
          div(class = "input-row",
            span(class = "input-name", "R\u00b2 ="),
            div(class = "input-value",
              numericInput("r_squared", label = NULL, value = NA,
                           step = NA, width = "80px")
            ),
            span(class = "input-desc",
              "proportion of variance explained by covariates; leave blank for no adjustment")
          )
        ),

        # from_existing
        conditionalPanel("input.source_type == 'existing'",
          div(class = "input-row",
            span(class = "input-name", "SE (existing) ="),
            div(class = "input-value",
              numericInput("se_existing", label = NULL, value = NA,
                           step = NA, width = "120px")
            ),
            span(class = "input-desc",
              "standard error of the treatment effect from that study")
          ),
          div(class = "input-row",
            span(class = "input-name", "n (existing) ="),
            div(class = "input-value",
              numericInput("n_existing", label = NULL, value = NA,
                           step = NA, width = "120px")
            ),
            span(class = "input-desc",
              "number of respondents per condition in that study")
          )
        ),

        # from_pilot
        conditionalPanel("input.source_type == 'pilot'",
          div(class = "input-row",
            span(class = "input-name", "SE (pilot) ="),
            div(class = "input-value",
              numericInput("se_pilot", label = NULL, value = NA,
                           step = NA, width = "120px")
            ),
            span(class = "input-desc",
              "standard error of the treatment effect from that study")
          ),
          div(class = "input-row",
            span(class = "input-name", "n (pilot) ="),
            div(class = "input-value",
              numericInput("n_pilot", label = NULL, value = NA,
                           step = NA, width = "120px")
            ),
            span(class = "input-desc",
              "number of respondents per condition in that study")
          )
        ),

        # n_planned (for find_mde and find_power)
        conditionalPanel(
          "input.goal_type == 'mde' || input.goal_type == 'power'",
          div(class = "input-row",
            uiOutput("n_label", container = span, class = "input-name"),
            div(class = "input-value",
              numericInput("n_planned", label = NULL, value = NA,
                           step = NA, width = "120px"),
              uiOutput("total_annotation", inline = TRUE)
            ),
            span(class = "input-desc",
              "number of respondents per condition in the planned study")
          )
        ),

        # tau (for find_power and find_n)
        conditionalPanel(
          "input.goal_type == 'power' || input.goal_type == 'n'",
          div(class = "input-row",
            span(class = "input-name", "tau ="),
            div(class = "input-value",
              numericInput("tau", label = NULL, value = NA,
                           step = NA, width = "120px")
            ),
            span(class = "input-desc",
              "the smallest treatment effect you'd consider meaningful, in units of the outcome")
          )
        )
      ),

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

  # --- Derived reactives ---

  interaction <- reactive(input$design_type == "interaction")

  ci <- reactive(0.90)

  use_r2 <- reactive({
    val <- input$r_squared
    input$source_type == "sd" &&
      !is.null(val) && !is.na(val) && is.numeric(val) && val > 0 && val < 1
  })

  power_val <- reactive({
    if (input$goal_type != "n") return(0.80)
    if (input$power_choice == "custom") {
      val <- input$custom_power
      if (!is.null(val) && !is.na(val) && val > 0 && val < 1) return(val)
      return(NULL)
    }
    as.numeric(input$power_choice)
  })

  # --- Collect validated inputs ---

  vp <- function(x) {
    if (is.null(x) || is.na(x) || !is.numeric(x) || x <= 0) NULL else x
  }

  collect_inputs <- reactive({
    r2 <- NULL
    if (use_r2()) {
      val <- input$r_squared
      if (!is.null(val) && !is.na(val) && is.numeric(val) &&
          val >= 0 && val < 1) {
        r2 <- val
      }
    }

    list(
      sd_y        = vp(input$sd_y),
      r_squared   = r2,
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
    inp <- collect_inputs()
    st  <- input$source_type
    gt  <- input$goal_type

    # Source inputs
    src_ok <- switch(st,
      sd = {
        ok <- !is.null(inp$sd_y)
        if (use_r2()) ok <- ok && !is.null(inp$r_squared)
        ok
      },
      existing = !is.null(inp$se_existing) && !is.null(inp$n_existing),
      pilot    = !is.null(inp$se_pilot) && !is.null(inp$n_pilot)
    )
    if (!src_ok) return(FALSE)

    # Goal inputs
    goal_ok <- switch(gt,
      mde   = !is.null(inp$n_planned),
      power = !is.null(inp$n_planned) && !is.null(inp$tau),
      n     = !is.null(inp$tau) && !is.null(power_val())
    )

    goal_ok
  })

  # --- Build source object ---

  source_obj <- reactive({
    req(inputs_complete())
    inp <- collect_inputs()
    st  <- input$source_type
    int <- interaction()

    tryCatch(
      switch(st,
        sd = {
          r2 <- if (!is.null(inp$r_squared)) inp$r_squared else 0
          from_sd(sd_y = inp$sd_y, r_squared = r2, interaction = int)
        },
        existing = from_existing(se_existing = inp$se_existing,
                                  n_existing = inp$n_existing,
                                  interaction = int),
        pilot    = from_pilot(se_pilot = inp$se_pilot,
                               n_pilot = inp$n_pilot,
                               interaction = int)
      ),
      error = function(e) NULL
    )
  })

  # --- Compute result ---

  result <- reactive({
    src <- source_obj()
    req(src)
    inp <- collect_inputs()
    gt  <- input$goal_type

    tryCatch({
      switch(gt,
        mde = {
          capture.output(
            res <- find_mde(src, n_planned = inp$n_planned, ci = ci())
          )
          res
        },
        power = {
          capture.output(
            res <- find_power(src, n_planned = inp$n_planned,
                               tau = inp$tau, ci = ci())
          )
          res
        },
        n = {
          pv <- power_val()
          req(pv)
          capture.output(
            res <- find_n(src, tau = inp$tau, power = pv, ci = ci())
          )
          res
        }
      )
    }, error = function(e) NULL)
  })

  # --- Outputs ---

  # n input label: "n =" for SD source, "n (planned) =" for existing/pilot
  output$n_label <- renderUI({
    label <- if (input$source_type == "sd") "n =" else "n (planned) ="
    label
  })

  # Total annotation
  output$total_annotation <- renderUI({
    val <- vp(input$n_planned)
    if (!is.null(val)) {
      n_cond <- if (interaction()) 4 else 2
      total <- n_cond * val
      tags$span(class = "total-annotation",
                paste0("(", format(total, big.mark = ","),
                       " total)"))
    }
  })

  # Computation section
  output$computation <- renderUI({
    st  <- input$source_type
    gt  <- input$goal_type
    int <- interaction()
    inp <- collect_inputs()
    res <- tryCatch(result(), error = function(e) NULL)
    pv  <- power_val()
    if (is.null(pv)) pv <- 0.80

    html <- render_computation(st, gt, int, ci(), pv, inp, use_r2(), res)
    HTML(html)
  })

  # Result highlight
  output$result_highlight <- renderUI({
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)

    html <- render_result_highlight(res, input$goal_type, interaction())
    HTML(html)
  })

  # Manuscript sentence
  output$sentence_box <- renderUI({
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)

    html <- render_sentence(res, interaction())
    HTML(html)
  })

  # MDE factor note
  output$note_box <- renderUI({
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)

    html <- render_note(res)
    if (nzchar(html)) HTML(html) else NULL
  })

  # Pipeline
  output$pipeline_box <- renderUI({
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)

    inp <- collect_inputs()
    pv  <- power_val()
    if (is.null(pv)) pv <- 0.80

    html <- render_pipeline(input$source_type, input$goal_type,
                            interaction(), ci(), pv, inp)
    HTML(html)
  })
}

shinyApp(ui, server)
