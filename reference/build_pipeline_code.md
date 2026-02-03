# Build pipeline code string

Constructs the pipeline code string from source type, goal type, and
inputs. Used by the interactive guide and the Shiny app.

## Usage

``` r
build_pipeline_code(source_type, goal_type, inputs)
```

## Arguments

- source_type:

  Character: "sd", "existing", or "pilot".

- goal_type:

  Character: "mde", "power", or "n".

- inputs:

  Named list with the relevant input values. Expected fields depend on
  source and goal type. Always expects `interaction` (logical), `ci`
  (numeric), and `power` (numeric, for goal_type "n"). Source-specific:
  `sd_y`, `r_squared` (sd); `se_existing`, `n_existing` (existing);
  `se_pilot`, `n_pilot` (pilot). Goal-specific: `n_planned` (mde,
  power); `tau` (power, n).

## Value

A character string containing the pipeline code.
