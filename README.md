[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-red.svg)](https://www.gnu.org/licenses/agpl-3.0)

# fastpeRmute

Fast Combinations and Permutations in R.

The `fastpeRmute` package offers efficient functions for computing combinations and permutations with and without repetition in R. The underlying implementation leverages C++ for optimal performance, making it potentially faster than both base R and other packages like gtools.

## Installation

To install the latest development version from GitHub:

```
devtools::install_github("gi0na/fastpeRmute")
```

## Usage

### Combinations

#### Without Repetition

```
library(fastpeRmute)
combinations(n = 10, r = 4)
```

#### With Repetition

```
combinations(n = 10, r = 4, repeats.allowed = TRUE)
```

### Permutations

#### Without Repetition

```
permutations(n = 4, r = 2)
```

#### With Repetition

```
permutations(n = 4, r = 2, repeats.allowed = TRUE)
```

### Using Vectors and Lists

You can also generate combinations or permutations from specific vectors:

#### Combinations from a Vector

```
v = letters
combinations(v=v, r = 3)
```

#### Permutations from a Vector

```
v = letters[1:4]
permutations(v=v, r = 2)
```

#### Combinations from a List

```
v <- mapply(v1 = letters[1:4], v2 = sample(LETTERS)[1:4], FUN = function(v1, v2) c(v1, v2), SIMPLIFY = FALSE)
combinations(v=v, r = 3, out_format = 'tibble')
```

#### Permutations from a List

```
v <- mapply(v1 = letters[1:4], v2 = sample(LETTERS)[1:4], FUN = function(v1, v2) c(v1, v2), SIMPLIFY = FALSE)
permutations(v=v, r = 2, out_format = 'tibble')
```

## Contributing

Feedback, contributions, and issues are welcome on the [GitHub repository](https://github.com/gi0na/fastpeRmute).
