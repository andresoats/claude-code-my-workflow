---
paths:
  - "**/*.R"
  - "Figures/**/*.R"
  - "scripts/**/*.R"
---

# R Code Standards

**Standard:** Senior Principal Data Engineer + PhD researcher quality

---

## 1. Reproducibility

- `set.seed()` called ONCE at top (YYYYMMDD format)
- All packages loaded at top via `pacman()` (not `require()` or `library`)
- All paths relative to repository root
- `dir.create(..., recursive = TRUE)` for output directories

## 2. Function Design

- `snake_case` naming, verb-noun pattern
- Roxygen-style documentation
- Default parameters, no magic numbers
- Named return values (lists or tibbles)

## 3. Domain Correctness

<!-- Customize for your field's known pitfalls -->
- Verify estimator implementations match slide formulas
- Check known package bugs (document below in Common Pitfalls)

## 4. Visual Identity

```r
# --- Your institutional palette ---
colours_custom_1_len6 <- c("#C3AE78","#026795","#25980D", "#F77F00","#344D33" , "#616161")
colours_custom_2_len8 <- c("#2E5266","#E63946", "#F77F00", "#6B9BD1", "#665191", "#457B9D","#828282", "#FCBF49")

```

### Custom Theme
```r
theme_custom <-  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Palatino", size = 10),
    plot.subtitle = element_text(hjust = 0.5, family = "Palatino", size = 8),
    axis.title.x = element_text(family = "Palatino", size = 12),
    axis.title.y = element_text(family = "Palatino", size = 12),
    axis.text = element_text(family = "Palatino"),
    legend.title = element_text(family = "Palatino", size = 10),
    legend.text = element_text(family = "Palatino", size = 10),
    legend.position = "top",
    strip.text = element_text(family = "Palatino", face = "bold"),
    #aspect.ratio = 3/4
  )
```

### Figure Dimensions for Beamer
```r
ggsave(filepath, width = 8, height = 4.5, bg = "transparent")
```

## 5. RDS Data Pattern

**Heavy computations saved as RDS; slide rendering loads pre-computed data.**

```r
saveRDS(result, file.path(out_dir, "descriptive_name.rds"))
```

## 6. Common Pitfalls

<!-- Add your field-specific pitfalls here -->
| Pitfall | Impact | Prevention |
|---------|--------|------------|
| Missing `bg = "transparent"` | White boxes on slides | Always include in ggsave() |
| Hardcoded paths | Breaks on other machines | Use relative paths |

## 7. Line Length & Mathematical Exceptions

**Standard:** Keep lines <= 100 characters.

**Exception: Mathematical Formulas** -- lines may exceed 100 chars **if and only if:**

1. Breaking the line would harm readability of the math (influence functions, matrix ops, finite-difference approximations, formula implementations matching paper equations)
2. An inline comment explains the mathematical operation:
   ```r
   # Sieve projection: inner product of residuals onto basis functions P_k
   alpha_k <- sum(r_i * basis[, k]) / sum(basis[, k]^2)
   ```
3. The line is in a numerically intensive section (simulation loops, estimation routines, inference calculations)

**Quality Gate Impact:**
- Long lines in non-mathematical code: minor penalty (-1 to -2 per line)
- Long lines in documented mathematical sections: no penalty

## 8. Code Quality Checklist

```
[ ] Packages at top via library()
[ ] set.seed() once at top
[ ] All paths relative
[ ] Functions documented (Roxygen)
[ ] Figures: transparent bg, explicit dimensions
[ ] RDS: every computed object saved
[ ] Comments explain WHY not WHAT
```
