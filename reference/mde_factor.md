# Compute the MDE factor

Returns `qnorm(1 - (1 - ci) / 2) + qnorm(power)`, the multiplier that
converts SE to MDE.

## Usage

``` r
mde_factor(power, ci = 0.9)
```

## Arguments

- power:

  Desired power level (e.g., 0.80 or 0.95).

- ci:

  Confidence interval level (default 0.90).

## Value

A numeric scalar.
