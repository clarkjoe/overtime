# overtime

## Installation
`devtools::install_github("clarkjoe/overtime")`

## Getting Started

The intent of overtime is to help machine learning developers generate lots of summary statistics extremely quickly. While there are many default summary statistics initially processed, including more will be available in future releases. The default summary statistics are:

1. `sum`
2. `mean`
3. `median`
4. `sd`
5. `max`
6. `min`
7. `sd / mean` (coefficient of variation)
8. OO2
9. OO3
10. Largest positive sequence
11. Largest negative sequence
12. Largest zero sequence
13. Largest increasing sequence
14. Largest decreasing sequence
15. Largest increasing positive sequence
16. Largest decreasing positive sequence
17. Largest increasing negative sequence
18. Largest decreasing negative sequence

The actual content of the package is smaller than most, but its scope of usability is a wide net. Below is a simple example:

```R
data <- readRDS('../data/rawData.rds')

nestedData <- data %>%
  overtime_by("day") %>%
  overtime_by_interval("days", 10) %>%
  overtime_get()

unnestedData <- nestedData %>%
  overtime_unnest()
```

BOOM! It's that simple.

