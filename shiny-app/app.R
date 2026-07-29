# Companion Shiny app for the link-function paper.
# Deterministic, explanatory examples only: no model fitting, no Monte Carlo
# simulation, no expensive computation. Dependencies: shiny and ggplot2.

for (pkg in c("shiny", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package '", pkg, "' is required but not installed.\n",
      "Install it with: install.packages('", pkg, "')",
      call. = FALSE
    )
  }
}

library(shiny)
library(ggplot2)

# Locate helpers whether the working directory is the app directory
# (shiny::runApp sets it there) or the repository root.
helper_candidates <- c("R/app_helpers.R", "shiny-app/R/app_helpers.R")
helper_path <- helper_candidates[file.exists(helper_candidates)][1]
if (is.na(helper_path)) stop("Cannot find R/app_helpers.R relative to the working directory.")
source(helper_path)

# Fixed two-series palette (Okabe-Ito blue and vermillion), colorblind-safe.
# Linetype is used as a secondary encoding so identity is never color-alone.
COL2 <- c("#0072B2", "#D55E00")

# Locate a repository file whether the working directory is the app
# directory or the repository root. Returns NA if not found.
find_repo_file <- function(rel) {
  candidates <- c(file.path("..", rel), rel)
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

MISSING_FILE_MSG <- "This file will appear after running the corresponding paper script."

PAPER_FIGS <- c(
  "figs/motivating-example.png",
  "figs/forced-choice-simulation.png",
  "figs/sum-score-simulation.png",
  "figs/logit-probit-fitted-example.png",
  "figs/within-family-links.png"
)

PAPER_TABLES <- c(
  "tables/simulation-summary-forced-choice.csv",
  "tables/simulation-summary-sum-scores.csv",
  "tables/simulation-summary-within-family-links.csv",
  "tables/diagnostic-scenarios.csv"
)

app_theme <- theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(1.05))
  )

# ---------------------------------------------------------------- UI --------

ui <- fluidPage(
  titlePanel("Link functions and interactions: a deterministic companion"),
  tabsetPanel(

    # Tab 1 ----------------------------------------------------------------
    tabPanel(
      "Overview",
      br(),
      h4("What this app shows"),
      p("In a generalized linear model, three things are easy to conflate:"),
      tags$ol(
        tags$li(strong("Outcome family:"),
                " the response support and sampling model (for example binomial counts on {0, ..., n})."),
        tags$li(strong("Link function:"),
                " the scale on which the linear predictor is additive."),
        tags$li(strong("Target metric:"),
                " the quantity the researcher verbally claims the interaction is about.")
      ),
      p("A zero product term means no interaction on the link scale,",
        "not necessarily on the observed response scale.",
        "Equal intervals on the linear predictor (eta) map to unequal intervals",
        "on the expected response (mu) through any nonlinear inverse link.",
        "When the assumed link differs from the scale a claim refers to,",
        "this curvature can create apparent interactions on the response scale,",
        "making the interaction conclusion sensitive to the chosen scale."),
      p("All examples in this app are deterministic. The app does not perform",
        "model fitting or Monte Carlo simulation; it only evaluates inverse link",
        "functions at values you choose. It shows scale dependence, not evidence",
        "that one link is universally correct."),
      p("The simulation results referenced in the paper are produced by the",
        "scripts in scripts/ and, when available, are displayed unchanged in the",
        "last tab.")
    ),

    # Tab 2 ----------------------------------------------------------------
    tabPanel(
      "Link scale explorer",
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput("t2_link", "Link function", choices = LINKS_ALL, selected = "logit"),
          sliderInput("t2_eta_range", "eta range", min = -10, max = 10,
                      value = c(-4, 4), step = 0.5),
          sliderInput("t2_n_int", "Number of equal eta intervals",
                      min = 2, max = 20, value = 8, step = 1),
          conditionalPanel(
            "input.t2_link == 'chance-corrected logit'",
            sliderInput("t2_chance", "Chance level", min = 0, max = 0.95,
                        value = 0.50, step = 0.01)
          )
        ),
        mainPanel(
          plotOutput("t2_plot", height = "420px"),
          verbatimTextOutput("t2_text"),
          p("Equal distances on the eta axis become unequal distances on the mu",
            "axis for any nonlinear link. On the link scale the spacing is",
            "constant by construction; on the response scale it is not.",
            "This is a property of the chosen scale, in this deterministic",
            "example, not a statement about which link is correct for any",
            "particular data set.")
        )
      )
    ),

    # Tab 3 ----------------------------------------------------------------
    tabPanel(
      "Four-cell interaction calculator",
      br(),
      sidebarLayout(
        sidebarPanel(
          sliderInput("t3_b0", "beta_0 (intercept)", min = -4, max = 4, value = -1, step = 0.1),
          sliderInput("t3_bx", "beta_x (effect of X)", min = -4, max = 4, value = 1.5, step = 0.1),
          sliderInput("t3_bz", "beta_z (effect of Z)", min = -4, max = 4, value = 1.5, step = 0.1),
          sliderInput("t3_bxz", "beta_xz (product term)", min = -4, max = 4, value = 0, step = 0.1),
          selectInput("t3_link", "Link function", choices = LINKS_FOUR_CELL, selected = "logit"),
          conditionalPanel(
            "input.t3_link == 'chance-corrected logit'",
            sliderInput("t3_chance", "Chance level", min = 0, max = 0.95,
                        value = 0.50, step = 0.01)
          )
        ),
        mainPanel(
          h5("Four cells (X = 0/1, Z = 0/1)"),
          tableOutput("t3_table"),
          verbatimTextOutput("t3_did"),
          plotOutput("t3_plot", height = "360px"),
          p("Setting beta_xz = 0 makes the link-scale difference-in-differences",
            "exactly zero, while the observed-scale difference-in-differences can",
            "still be nonzero whenever the inverse link is nonlinear and the two",
            "main effects move the cells across regions of different curvature.",
            "On this scale, the apparent interaction is induced by the link, not",
            "by the product term.")
        )
      )
    ),

    # Tab 3b ---------------------------------------------------------------
    # Reverse of Tab 3: the four cell values are chosen directly, and the
    # implied model coefficients are shown for each link function at once.
    tabPanel(
      "Reverse four-cell calculator",
      br(),
      sidebarLayout(
        sidebarPanel(
          p("Set the four expected cell values (as probabilities) directly.",
            "The table shows the model coefficients each link function",
            "implies for exactly these cells."),
          numericInput("t3b_mu00", "mu (X = 0, Z = 0)", value = 0.10,
                       min = 0.001, max = 0.999, step = 0.01),
          numericInput("t3b_mu10", "mu (X = 1, Z = 0)", value = 0.50,
                       min = 0.001, max = 0.999, step = 0.01),
          numericInput("t3b_mu01", "mu (X = 0, Z = 1)", value = 0.25,
                       min = 0.001, max = 0.999, step = 0.01),
          numericInput("t3b_mu11", "mu (X = 1, Z = 1)", value = 0.75,
                       min = 0.001, max = 0.999, step = 0.01),
          sliderInput("t3b_chance", "Chance level (chance-corrected logit row only)",
                      min = 0, max = 0.95, value = 0.50, step = 0.01),
          hr(),
          h6("Implied 2 x 2 design"),
          plotOutput("t3b_design", height = "220px")
        ),
        mainPanel(
          h5("Coefficients implied by the same four cells under each link"),
          uiOutput("t3b_table"),
          plotOutput("t3b_plot", height = "360px"),
          p("The four cell values are held fixed while the model scale changes,",
            "so any difference between rows is due to the link alone.",
            "The product term beta_xz (highlighted) is the link-scale",
            "difference-in-differences: it can be exactly zero under one link",
            "and clearly nonzero under another for the same cells.",
            "The default values are chosen so that the logit product term is",
            "exactly zero. Cells at or below the chance level have no finite",
            "linear predictor under the chance-corrected logit and are shown",
            "as missing.")
        )
      )
    ),

    # Tab 4 ----------------------------------------------------------------
    tabPanel(
      "Forced-choice chance floor",
      br(),
      sidebarLayout(
        sidebarPanel(
          sliderInput("t4_chance", "Chance level", min = 0, max = 0.95,
                      value = 0.50, step = 0.01),
          sliderInput("t4_age_range", "Age range", min = 3, max = 16,
                      value = c(6, 10), step = 0.5),
          sliderInput("t4_center", "Age center", min = 3, max = 16,
                      value = 8, step = 0.5),
          sliderInput("t4_b0", "beta_0 (intercept)", min = -4, max = 4, value = 0, step = 0.1),
          sliderInput("t4_bage", "beta_age", min = -2, max = 2, value = 0.5, step = 0.05),
          sliderInput("t4_bgroup", "beta_group", min = -4, max = 4, value = 1, step = 0.1),
          sliderInput("t4_bag", "beta_age_group (product term)", min = -2, max = 2,
                      value = 0, step = 0.05)
        ),
        mainPanel(
          plotOutput("t4_plot", height = "420px"),
          h5("Predicted probabilities at low, middle, and high age"),
          tableOutput("t4_table"),
          p("A standard binomial logit maps eta to the full (0, 1) interval.",
            "A forced-choice task implies a lower asymptote at the chance level,",
            "so accuracy cannot fall below it in expectation.",
            "Ignoring the chance floor can change the observed-scale pattern of",
            "group differences across age even when the product term is zero on",
            "the generating scale. In this deterministic example the same eta is",
            "passed through both inverse links; only the mapping differs.")
        )
      )
    ),

    # Tab 5 ----------------------------------------------------------------
    tabPanel(
      "Logit vs probit",
      br(),
      sidebarLayout(
        sidebarPanel(
          sliderInput("t5_b0", "Intercept", min = -4, max = 4, value = 0, step = 0.1),
          sliderInput("t5_b1", "Slope", min = -3, max = 3, value = 1, step = 0.1),
          sliderInput("t5_xrange", "Predictor range", min = -8, max = 8,
                      value = c(-4, 4), step = 0.5),
          sliderInput("t5_scale", "Scaling constant c (probit uses eta / c)",
                      min = 1.0, max = 2.5, value = 1.65, step = 0.01)
        ),
        mainPanel(
          plotOutput("t5_plot", height = "340px"),
          plotOutput("t5_diff_plot", height = "260px"),
          h5("Four-cell example with beta_xz = 0 (beta_0 = intercept, beta_x = beta_z = slope)"),
          tableOutput("t5_table"),
          p("With eta scaled by a constant near 1.6 to 1.7, logit and probit",
            "curves are often similar in the middle of the probability range,",
            "but they differ in the tails. When cell probabilities sit near 0 or",
            "1, this tail behavior can change observed-scale",
            "difference-in-differences, so interaction claims can depend on the",
            "choice between two links that look almost identical in the middle.")
        )
      )
    )

    # Tab 6 ----------------------------------------------------------------
    # Hidden on request. To restore, uncomment this block and put a comma
    # after the closing parenthesis of the previous tabPanel.
    # tabPanel(
    #   "Precomputed paper outputs",
    #   br(),
    #   p("This tab only displays files already produced by the paper scripts in",
    #     "scripts/. Nothing is computed here."),
    #   h4("Figures"),
    #   uiOutput("t6_figs"),
    #   h4("Tables"),
    #   uiOutput("t6_tables")
    # )
  )
)

# ------------------------------------------------------------- server -------

server <- function(input, output, session) {

  # Tab 2: link scale explorer --------------------------------------------
  t2_chance <- reactive({
    if (input$t2_link == "chance-corrected logit") input$t2_chance else 0
  })

  t2_data <- reactive({
    rng <- input$t2_eta_range
    validate(need(rng[2] > rng[1], "eta range must have positive width."))
    eta <- seq(rng[1], rng[2], length.out = 401)
    # The inverse link 1/eta is undefined at eta = 0 and explodes nearby;
    # omit a small neighborhood from the display only.
    if (input$t2_link == "inverse") eta <- eta[abs(eta) >= 0.1]
    mu <- inv_link(eta, input$t2_link, t2_chance())
    data.frame(eta = eta, mu = mu)[is.finite(mu), , drop = FALSE]
  })

  t2_points <- reactive({
    rng <- input$t2_eta_range
    eta <- seq(rng[1], rng[2], length.out = input$t2_n_int + 1)
    data.frame(eta = eta, mu = inv_link(eta, input$t2_link, t2_chance()))
  })

  output$t2_plot <- renderPlot({
    d <- t2_data()
    pts <- t2_points()
    pts_ok <- pts[is.finite(pts$mu), , drop = FALSE]
    ggplot(d, aes(eta, mu)) +
      geom_vline(xintercept = pts$eta, linetype = "dotted", color = "grey65") +
      geom_segment(
        data = pts_ok,
        aes(x = min(d$eta), xend = eta, y = mu, yend = mu),
        linetype = "dotted", color = "grey65"
      ) +
      geom_line(color = COL2[1], linewidth = 1) +
      geom_point(data = pts_ok, color = COL2[1], size = 2.5) +
      labs(
        x = "eta (linear predictor)", y = "mu (expected response)",
        title = paste0("Inverse ", input$t2_link, " link"),
        subtitle = "Vertical lines mark equally spaced eta values; points show the induced mu values"
      ) +
      app_theme
  })

  output$t2_text <- renderText({
    pts <- t2_points()
    w_eta <- diff(pts$eta)
    w_mu <- diff(pts$mu)
    lines <- c(
      sprintf("Equal eta intervals, each of width %.4g:", w_eta[1]),
      "",
      "  interval            mu width",
      sprintf("  [%7.3f, %7.3f]  %.6g",
              pts$eta[-nrow(pts)], pts$eta[-1], w_mu),
      "",
      if (input$t2_link == "identity") {
        "The identity link is linear: equal eta intervals stay equal on mu."
      } else {
        "Equal distances on eta become unequal distances on mu under this nonlinear link."
      },
      if (input$t2_link == "inverse" &&
          input$t2_eta_range[1] < 0.1 && input$t2_eta_range[2] > -0.1) {
        "Note: 1/eta is undefined at eta = 0; the curve omits a small neighborhood of 0."
      } else NULL
    )
    paste(lines, collapse = "\n")
  })

  # Tab 3: four-cell interaction calculator --------------------------------
  t3_chance <- reactive({
    if (input$t3_link == "chance-corrected logit") input$t3_chance else 0
  })

  t3_cells <- reactive({
    four_cell_values(input$t3_b0, input$t3_bx, input$t3_bz, input$t3_bxz,
                     input$t3_link, t3_chance())
  })

  output$t3_table <- renderTable({
    t3_cells()
  }, digits = 4)

  output$t3_did <- renderText({
    cells <- t3_cells()
    dd <- diff_in_diff(cells)
    lines <- c(
      sprintf("Link-scale difference-in-differences (equals beta_xz): %.6g", dd$link_scale),
      sprintf("Observed-scale difference-in-differences:              %.6g", dd$observed_scale)
    )
    if (input$t3_link == "identity" && any(cells$mu < 0 | cells$mu > 1)) {
      lines <- c(lines, "",
        "Warning: with the identity link some mu values fall outside [0, 1].",
        "They are shown unclamped; they are not valid probabilities.")
    }
    paste(lines, collapse = "\n")
  })

  output$t3_plot <- renderPlot({
    cells <- t3_cells()
    cells$Zf <- factor(cells$Z, levels = c(0, 1), labels = c("Z = 0", "Z = 1"))
    ggplot(cells, aes(X, mu, color = Zf, linetype = Zf)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = c(0, 1)) +
      scale_color_manual(values = COL2, name = NULL) +
      scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
      labs(
        x = "X", y = "mu (expected response)",
        title = "Observed response scale",
        subtitle = "Parallel lines on the link scale need not be parallel here"
      ) +
      app_theme
  })

  # Tab 3b: reverse four-cell calculator ------------------------------------
  t3b_coefs <- reactive({
    mus <- c(input$t3b_mu00, input$t3b_mu10, input$t3b_mu01, input$t3b_mu11)
    validate(
      need(length(mus) == 4 && all(is.finite(mus)),
           "All four cell values must be numbers."),
      need(all(mus > 0 & mus < 1),
           "All four cell values must lie strictly between 0 and 1.")
    )
    rows <- lapply(LINKS_FOUR_CELL, function(link) {
      ch <- if (link == "chance-corrected logit") input$t3b_chance else 0
      # Cells at or below the chance level yield NaN (with a warning from
      # qlogis) under the chance-corrected logit; shown as missing.
      cf <- suppressWarnings(
        four_cell_coefs(mus[1], mus[2], mus[3], mus[4], link, ch)
      )
      data.frame(link = link, beta_0 = cf[["beta_0"]], beta_x = cf[["beta_x"]],
                 beta_z = cf[["beta_z"]], beta_xz = cf[["beta_xz"]])
    })
    do.call(rbind, rows)
  })

  output$t3b_design <- renderPlot({
    mus <- c(input$t3b_mu00, input$t3b_mu10, input$t3b_mu01, input$t3b_mu11)
    validate(
      need(length(mus) == 4 && all(is.finite(mus)),
           "All four cell values must be numbers."),
      need(all(mus > 0 & mus < 1),
           "All four cell values must lie strictly between 0 and 1.")
    )
    cells <- data.frame(X = c(0, 1, 0, 1), Z = c(0, 0, 1, 1), mu = mus)
    cells$Zf <- factor(cells$Z, levels = c(0, 1), labels = c("Z = 0", "Z = 1"))
    ggplot(cells, aes(X, mu, color = Zf, linetype = Zf)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = c(0, 1)) +
      scale_color_manual(values = COL2, name = NULL) +
      scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = "X", y = "mu") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), legend.position = "top")
  })

  output$t3b_table <- renderUI({
    d <- t3b_coefs()
    num <- "text-align: right; padding: 6px 14px;"
    hl <- paste(num, "background-color: #FFF3CD; font-weight: bold;")
    fmt <- function(x) ifelse(is.finite(x), sprintf("%.4f", x), "—")
    header <- tags$tr(
      tags$th("Link function", style = "text-align: left; padding: 6px 14px;"),
      tags$th("beta_0", style = num), tags$th("beta_x", style = num),
      tags$th("beta_z", style = num),
      tags$th("beta_xz (product term)", style = hl)
    )
    body <- lapply(seq_len(nrow(d)), function(i) {
      tags$tr(
        tags$td(d$link[i], style = "padding: 6px 14px;"),
        tags$td(fmt(d$beta_0[i]), style = num),
        tags$td(fmt(d$beta_x[i]), style = num),
        tags$td(fmt(d$beta_z[i]), style = num),
        tags$td(fmt(d$beta_xz[i]), style = hl)
      )
    })
    tags$table(
      class = "table table-striped",
      style = "width: auto; font-size: 15px;",
      tags$thead(header), tags$tbody(body)
    )
  })

  output$t3b_plot <- renderPlot({
    d <- t3b_coefs()
    d <- d[is.finite(d$beta_xz), , drop = FALSE]
    validate(need(nrow(d) > 0, "No finite product term under any link."))
    d$link <- factor(d$link, levels = rev(LINKS_FOUR_CELL))
    ggplot(d, aes(beta_xz, link)) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "grey55") +
      geom_col(fill = COL2[1], width = 0.6) +
      labs(
        x = "beta_xz (product term)", y = NULL,
        title = "Product term implied by the same four cells",
        subtitle = "One bar per link function; the dotted line marks zero"
      ) +
      app_theme
  })

  # Tab 4: forced-choice chance floor ---------------------------------------
  t4_eta <- function(age, group) {
    a <- age - input$t4_center
    input$t4_b0 + input$t4_bage * a + input$t4_bgroup * group +
      input$t4_bag * a * group
  }

  t4_curves <- reactive({
    rng <- input$t4_age_range
    validate(need(rng[2] > rng[1], "Age range must have positive width."))
    age <- seq(rng[1], rng[2], length.out = 201)
    grid <- expand.grid(age = age, group = c(0, 1))
    eta <- t4_eta(grid$age, grid$group)
    rbind(
      data.frame(grid, link = "Standard logit",
                 p = inv_link(eta, "logit")),
      data.frame(grid, link = "Chance-corrected logit",
                 p = inv_link(eta, "chance-corrected logit",
                              chance = input$t4_chance))
    )
  })

  output$t4_plot <- renderPlot({
    d <- t4_curves()
    d$link <- factor(d$link, levels = c("Standard logit", "Chance-corrected logit"))
    d$Group <- factor(d$group, levels = c(0, 1), labels = c("Group 0", "Group 1"))
    ggplot(d, aes(age, p, color = Group, linetype = Group)) +
      geom_hline(yintercept = input$t4_chance, linetype = "dotted", color = "grey55") +
      geom_line(linewidth = 1) +
      facet_wrap(~link) +
      scale_color_manual(values = COL2, name = NULL) +
      scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(
        x = "Age", y = "Predicted probability",
        title = "Same eta, two inverse links",
        subtitle = "Dotted horizontal line marks the chance level"
      ) +
      app_theme
  })

  output$t4_table <- renderTable({
    rng <- input$t4_age_range
    ages <- c(rng[1], mean(rng), rng[2])
    rows <- expand.grid(age = ages,
                        link = c("Standard logit", "Chance-corrected logit"),
                        stringsAsFactors = FALSE)
    p_of <- function(age, link, group) {
      eta <- t4_eta(age, group)
      if (link == "Standard logit") inv_link(eta, "logit")
      else inv_link(eta, "chance-corrected logit", chance = input$t4_chance)
    }
    rows$p_group0 <- mapply(p_of, rows$age, rows$link, 0)
    rows$p_group1 <- mapply(p_of, rows$age, rows$link, 1)
    rows$difference <- rows$p_group1 - rows$p_group0
    rows[, c("link", "age", "p_group0", "p_group1", "difference")]
  }, digits = 4)

  # Tab 5: logit vs probit ---------------------------------------------------
  t5_curves <- reactive({
    rng <- input$t5_xrange
    validate(need(rng[2] > rng[1], "Predictor range must have positive width."))
    x <- seq(rng[1], rng[2], length.out = 301)
    eta <- input$t5_b0 + input$t5_b1 * x
    data.frame(
      x = x,
      logit = inv_link(eta, "logit"),
      probit = inv_link(eta / input$t5_scale, "probit")
    )
  })

  output$t5_plot <- renderPlot({
    d <- t5_curves()
    long <- rbind(
      data.frame(x = d$x, p = d$logit, curve = "Logit"),
      data.frame(x = d$x, p = d$probit,
                 curve = sprintf("Probit (eta / %.2f)", input$t5_scale))
    )
    ggplot(long, aes(x, p, color = curve, linetype = curve)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = COL2, name = NULL) +
      scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
      labs(
        x = "Predictor", y = "Probability",
        title = "Logit and scaled probit on the same axes"
      ) +
      app_theme
  })

  output$t5_diff_plot <- renderPlot({
    d <- t5_curves()
    ggplot(d, aes(x, logit - probit)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey55") +
      geom_line(color = COL2[1], linewidth = 1) +
      labs(
        x = "Predictor", y = "Logit minus scaled probit",
        title = "Pointwise probability difference"
      ) +
      app_theme
  })

  output$t5_table <- renderTable({
    b0 <- input$t5_b0
    b <- input$t5_b1
    s <- input$t5_scale
    logit_cells <- four_cell_values(b0, b, b, 0, "logit")
    probit_cells <- four_cell_values(b0 / s, b / s, b / s, 0, "probit")
    data.frame(
      link = c("logit", sprintf("probit (coefficients / %.2f)", s)),
      `link-scale DiD` = c(diff_in_diff(logit_cells)$link_scale,
                           diff_in_diff(probit_cells)$link_scale),
      `observed-scale DiD` = c(diff_in_diff(logit_cells)$observed_scale,
                               diff_in_diff(probit_cells)$observed_scale),
      check.names = FALSE
    )
  }, digits = 5)

  # Tab 6: precomputed paper outputs ----------------------------------------
  output$t6_figs <- renderUI({
    blocks <- lapply(seq_along(PAPER_FIGS), function(i) {
      rel <- PAPER_FIGS[i]
      path <- find_repo_file(rel)
      body <- if (is.na(path)) {
        p(em(MISSING_FILE_MSG))
      } else {
        imageOutput(paste0("t6_fig_", i), height = "auto")
      }
      tagList(h5(basename(rel)), body, hr())
    })
    do.call(tagList, blocks)
  })

  for (i in seq_along(PAPER_FIGS)) {
    local({
      idx <- i
      output[[paste0("t6_fig_", idx)]] <- renderImage({
        path <- find_repo_file(PAPER_FIGS[idx])
        list(src = path, alt = basename(PAPER_FIGS[idx]), width = "100%")
      }, deleteFile = FALSE)
    })
  }

  output$t6_tables <- renderUI({
    blocks <- lapply(seq_along(PAPER_TABLES), function(i) {
      rel <- PAPER_TABLES[i]
      path <- find_repo_file(rel)
      body <- if (is.na(path)) {
        p(em(MISSING_FILE_MSG))
      } else {
        tableOutput(paste0("t6_table_", i))
      }
      tagList(h5(basename(rel)), body, hr())
    })
    do.call(tagList, blocks)
  })

  for (i in seq_along(PAPER_TABLES)) {
    local({
      idx <- i
      output[[paste0("t6_table_", idx)]] <- renderTable({
        path <- find_repo_file(PAPER_TABLES[idx])
        validate(need(!is.na(path), MISSING_FILE_MSG))
        read.csv(path, check.names = FALSE)
      })
    })
  }
}

shinyApp(ui, server)
