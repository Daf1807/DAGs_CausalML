# Part 3 - Damned if you do, damned if you don't 

## Graph and simulate example 4 of Lab7 (Damned if you do damned if you don't) 

# install.packages("dagitty")

library(dagitty)

dag <- dagitty("
dag {
  U1 [pos=\"-1,1\"]
  U2 [pos=\"1,1\"]
  Z  [pos=\"0,1\"]
  X  [pos=\"-1,-1\"]
  Y  [pos=\"1,-1\"]
  
  U1 -> X
  U1 -> Z
  U2 -> Z
  U2 -> Y
  X  -> Y
  Z  -> Y
}
")

plot(dag)   # gráfico básico, sin ggdag

# ------------------------
# 1) Large population
# ------------------------
N <- 1000000  # 1 million
set.seed(123)

# Parameters
a1 <- 1.0; a2 <- 1.0   # Z <- a1*U1 + a2*U2 + e_z  (Z is collider of U1 and U2)
b1 <- 1.0              # X <- b1*U1 + e_x
tau <- 1.0             # Y <- tau*X + c1*Z + c2*U2 + e_y (causal effect of X)
c1 <- 1.0; c2 <- 1.0

# Latent variables and errors
U1 <- rnorm(N, 0, 1)
U2 <- rnorm(N, 0, 1)
e_z <- rnorm(N, 0, 1)
e_x <- rnorm(N, 0, 1)
e_y <- rnorm(N, 0, 1)

# Construct variables
pop_Z <- a1*U1 + a2*U2 + e_z
pop_X <- b1*U1 + e_x
pop_Y <- tau*pop_X + c1*pop_Z + c2*U2 + e_y

# ------------------------
# 2) Random sample
# ------------------------
n <- 5000
sample_indices <- sample(1:N, size=n, replace=FALSE)

Y <- pop_Y[sample_indices]
X <- pop_X[sample_indices]
Z <- pop_Z[sample_indices]

## Regress Y vs. X with and without controlling for Z

# ------------------------
# OLS: sin Z (Y ~ X)
# ------------------------

# Ajustar regresión Y ~ X (sin Z)
res_omitZ <- lm(Y ~ X)

cat("=== OLS: Y ~ X (omit Z) ===\n")
summary(res_omitZ)

# ------------------------
# OLS: con Z (Y ~ X + Z)
# ------------------------

# Ajustar regresión Y ~ X + Z (controlando Z)
res_withZ <- lm(Y ~ X + Z)

cat("\n=== OLS: Y ~ X + Z (control Z) ===\n")
summary(res_withZ)

# Nota:
# - El verdadero efecto causal de X es tau (=1.0).
# - El modelo Y ~ X suele estar sesgado hacia arriba (confounding vía U1).
# - El modelo Y ~ X + Z suele sesgar hacia abajo (porque se abre el colisionador U1 -> Z <- U2).

# Plot your coefficients associated with X in a single graph like in Part 2. Add this time a horizontal line indicating the true causal effect

# ------------------------
# Coefficients from regressions
# ------------------------
coef_omitZ <- coef(res_omitZ)["X"]     # beta_X en Y ~ X
coef_withZ <- coef(res_withZ)["X"]     # beta_X en Y ~ X + Z

# Labels and values
labels <- c("Y ~ X (omit Z)", "Y ~ X + Z (control Z)")
values <- c(coef_omitZ, coef_withZ)

# True causal effect
tau_true <- 1.0

# ------------------------
# Plot
# ------------------------
barplot(
  values,
  names.arg = labels,
  col = c("skyblue", "salmon"),
  ylim = c(min(values, tau_true) - 0.5, max(values, tau_true) + 0.5),
  ylab = "Estimated coefficient of X",
  main = "Comparison of OLS estimates for beta_X"
)

# Add horizontal line for true effect
abline(h = tau_true, col = "black", lty = 2)

# Add legend
legend("topright", legend = "True causal effect", lty = 2, col = "black")

## Now modify the DAG so that Z also has an effect on X. For this part suppose you can observe and control for U1 and U2. Just like in Part 2, you are asked to find a way to get a good estimate with the least number of controls Z, U1, U2. 

library(dagitty)

dag <- dagitty("
dag {
  U1 [pos=\"-1,1\"]
  U2 [pos=\"1,1\"]
  Z  [pos=\"0,1\"]
  X  [pos=\"-1,-1\"]
  Y  [pos=\"1,-1\"]
  
  U1 -> X
  U1 -> Z
  U2 -> Z
  U2 -> Y
  Z  -> X   # nueva arista añadida
  X  -> Y
  Z  -> Y
}
")

# Gráfico básico
plot(dag)

# -----------------------------
# 1) Simulation with U1, U2 observables
# -----------------------------
simulate_once <- function(
    n = 5000, seed = 123,
    a1 = 1.0, a2 = 1.0,   # Z <- a1*U1 + a2*U2 + e_z
    b1 = 1.0, d1 = 1.0,   # X <- b1*U1 + d1*Z + e_x
    tau = 1.0,            # Y <- tau*X + c1*Z + c2*U2 + e_y
    c1 = 1.0, c2 = 1.0,
    sig_u = 1.0, sig_ez = 1.0, sig_ex = 1.0, sig_ey = 1.0
){
  set.seed(seed)
  U1 <- rnorm(n, 0, sig_u)
  U2 <- rnorm(n, 0, sig_u)
  e_z <- rnorm(n, 0, sig_ez)
  e_x <- rnorm(n, 0, sig_ex)
  e_y <- rnorm(n, 0, sig_ey)
  
  Z <- a1*U1 + a2*U2 + e_z
  X <- b1*U1 + d1*Z + e_x
  Y <- tau*X + c1*Z + c2*U2 + e_y
  
  data.frame(U1=U1, U2=U2, Z=Z, X=X, Y=Y)
}

# -----------------------------
# 2) Run OLS with different controls
# -----------------------------
ols_y_on_x <- function(df, controls=NULL){
  if(is.null(controls)){
    f <- as.formula("Y ~ X")
  } else {
    f <- as.formula(paste("Y ~ X +", paste(controls, collapse=" + ")))
  }
  model <- lm(f, data=df)
  return(model)
}

# Simulate data
df <- simulate_once(n=10000, seed=42)
tau_true <- 1.0

# Different specifications
specs <- list(
  list(name="Y ~ X", ctrls=NULL),
  list(name="Y ~ X + Z", ctrls=c("Z")),
  list(name="Y ~ X + Z + U1", ctrls=c("Z","U1")),
  list(name="Y ~ X + Z + U2", ctrls=c("Z","U2")),
  list(name="Y ~ X + Z + U1 + U2", ctrls=c("Z","U1","U2"))
)

results <- data.frame(Spec=character(), beta_X=numeric(),
                      CI_low=numeric(), CI_high=numeric(),
                      Bias=numeric(), stringsAsFactors = FALSE)

for (s in specs){
  m <- ols_y_on_x(df, controls=s$ctrls)
  bx <- coef(m)["X"]
  ci <- confint(m, level=0.99)["X",]   # 99% CI
  results <- rbind(results, data.frame(
    Spec = s$name,
    beta_X = bx,
    CI_low = ci[1],
    CI_high = ci[2],
    Bias = bx - tau_true
  ))
}

print(results)

# -----------------------------
# 3) Plot coefficients with 99% CI
# -----------------------------
# install.packages("ggplot2") if needed
library(ggplot2)

ggplot(results, aes(x=Spec, y=beta_X)) +
  geom_point() +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=0.2) +
  geom_hline(yintercept=tau_true, linetype="dashed", color="black") +
  labs(y="Estimated coefficient of X",
       title="beta_X with 99% CI (α = 0.01)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=15, hjust=1))

## Regress Y vs. X but adding each possible combination of controls from Z, U1, U2 (you should end up running $2^3$ regressions) <n>
## Store the point estimate and standard error of the target parameter in a matrix or dataframe

library(ggplot2)

# 1) Simulation function (como antes)
simulate_once <- function(
    n = 5000, seed = 123,
    a1 = 1.0, a2 = 1.0,   # Z <- a1*U1 + a2*U2 + e_z
    b1 = 1.0, d1 = 1.0,   # X <- b1*U1 + d1*Z + e_x
    tau = 1.0,            # Y <- tau*X + c1*Z + c2*U2 + e_y
    c1 = 1.0, c2 = 1.0,
    sig_u = 1.0, sig_ez = 1.0, sig_ex = 1.0, sig_ey = 1.0
){
  set.seed(seed)
  U1 <- rnorm(n, 0, sig_u)
  U2 <- rnorm(n, 0, sig_u)
  e_z <- rnorm(n, 0, sig_ez)
  e_x <- rnorm(n, 0, sig_ex)
  e_y <- rnorm(n, 0, sig_ey)
  
  Z <- a1*U1 + a2*U2 + e_z
  X <- b1*U1 + d1*Z + e_x
  Y <- tau*X + c1*Z + c2*U2 + e_y
  
  data.frame(U1=U1, U2=U2, Z=Z, X=X, Y=Y)
}

# 2) Helper para OLS
ols_y_on_x <- function(df, controls=NULL){
  if(is.null(controls)){
    f <- as.formula("Y ~ X")
  } else {
    f <- as.formula(paste("Y ~ X +", paste(controls, collapse=" + ")))
  }
  lm(f, data=df)
}

# 3) Run all 2^3 specs
run_all_specs <- function(df, tau_true=1.0, controls=c("Z","U1","U2")){
  combos <- list(NULL) # empieza con modelo sin controles
  for(k in 1:length(controls)){
    combos <- c(combos, combn(controls, k, simplify=FALSE))
  }
  
  rows <- list()
  models <- list()
  
  for(c in combos){
    name <- if(is.null(c)) "Y ~ X" else paste("Y ~ X +", paste(c, collapse=" + "))
    m <- ols_y_on_x(df, controls=c)
    
    bx <- coef(m)["X"]
    se <- summary(m)$coefficients["X","Std. Error"]
    
    rows[[length(rows)+1]] <- data.frame(
      Spec = name,
      k_controls = if(is.null(c)) 0 else length(c),
      Controls = if(is.null(c)) "(none)" else paste(c, collapse=","),
      beta_X = as.numeric(bx),
      se_X = as.numeric(se),
      Bias = as.numeric(bx - tau_true)
    )
    models[[name]] <- m
  }
  
  res <- do.call(rbind, rows)
  res <- res[order(res$k_controls, res$Spec),]
  rownames(res) <- NULL
  
  list(results=res, models=models, matrix_store=as.matrix(res[,c("beta_X","se_X")]))
}

# 4) Ejecutar
df <- simulate_once(n=10000, seed=42)
tau_true <- 1.0
out <- run_all_specs(df, tau_true, controls=c("Z","U1","U2"))

results <- out$results
models <- out$models
matrix_store <- out$matrix_store

cat("Total de especificaciones estimadas:", nrow(results), "\n") # debería imprimir 8
print(results)

cat("\nMatriz (columnas = [beta_X, se_X]):\n")
print(matrix_store)

# 5) Plot
ggplot(results, aes(x=Spec, y=beta_X)) +
  geom_bar(stat="identity", fill="skyblue") +
  geom_hline(yintercept=tau_true, linetype="dashed", color="black") +
  labs(y="Estimated coefficient of X (beta_X)",
       title="All 2^3 combinations of controls {Z, U1, U2}") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=20, hjust=1))

## Present your results in a well displayed table with the following characteristics (you should export this table in .tex or .txt format to the /output folder) 
## - Column names should be β and SE
## - Row names should have the controls from Z, U1, U2 that were included in the regression

# ============================================================
# Reqs: dplyr, xtable
# install.packages(c("dplyr","xtable"))
# install.packages("xtable")
# ============================================================

library(dplyr)
library(xtable)

# ----------------------------
# Asumimos que ya tienes "results"
# con columnas: Spec, Controls, beta_X, se_X
# ----------------------------

# 1) Formato de etiquetas de controles
controls_label <- function(s){
  if(is.na(s) || s == "" || s == "(none)") {
    return("(none)")
  } else {
    return(gsub(",", " + ", s))
  }
}

# 2) Construir tabla resumida
table <- results %>%
  mutate(Row = sapply(Controls, controls_label)) %>%
  select(Row, beta_X, se_X)

# Renombrar columnas
colnames(table) <- c("Row", "$\\hat{\\beta}_X$", "SE")

# Redondeo
table_rounded <- table
table_rounded[,2:3] <- round(table_rounded[,2:3], 4)

# Mostrar en consola
cat("\n=== Tabla Resumen ===\n")
print(table_rounded)

# ----------------------------
# 3) Exportar resultados
# ----------------------------
out_dir <- "output"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

tex_path <- file.path(out_dir, "regression_table.tex")
txt_path <- file.path(out_dir, "regression_table.txt")

# --- TXT (alineado) ---
write.table(table_rounded, file=txt_path, row.names=FALSE, quote=FALSE)

# --- LaTeX ---
latex_code <- capture.output(
  print(
    xtable(table_rounded,
           caption="OLS Estimates of $\\\\beta_X$ under Different Control Sets",
           label="tab:regressions",
           align=c("l","l","c","c")),
    include.rownames=FALSE,
    sanitize.text.function = identity, # para no escapar símbolos LaTeX
    booktabs=TRUE
  )
)

# Guardar en archivo .tex
writeLines(latex_code, con=tex_path)

cat("\nTabla exportada en:\n- ", tex_path, "\n- ", txt_path, "\n")

# install.packages("gridExtra")
library(gridExtra)
library(grid)

# Create the table object
tab <- tableGrob(table_rounded, rows = NULL)

# Export to PNG
png("output/regression_table.png", width = 800, height = 600)
grid.draw(tab)   # <- now it will find grid.draw()
dev.off()

cat("Table exported as: output/regression_table.png\n")

## Based on your findings, in what way(s) can you get a good estimate of the causal effect? 

# The true causal effect of X on Y is 1.0. 
# When no controls are included, the estimate is about 1.71, showing strong upward bias. 
# Controlling only for U1 produces an estimate close to 1.99, which is also biased upward. 
# When controlling only for U2, the estimate is around 1.50, still biased. 
# Controlling only for Z gives a downward-biased estimate of about 0.81, since Z is a collider 
# between U1 and U2.
#
# This illustrates the dilemma: 
# - Without Z, a backdoor path remains open. 
# - With Z but without U1 or U2, we open a collider path and introduce new bias. 
#
# Looking at the specifications with multiple controls, including Z together with U1 
# or with U2 gives an estimate close to 1.0, with low standard error. 
# Adding all three controls (Z, U1, U2) also works, but does not improve precision much. 
#
# Conclusion: in this “damned if you do, damned if you don’t” case, the minimal sufficient 
# sets of controls that recover the causal effect are (Z, U1) or (Z, U2). The full set (Z, U1, U2) 
# is valid but not strictly necessary.

## What is the minimal sufficient set of controls to get a good estimate?

# A good estimate of the causal effect can be obtained by controlling for the set (Z, U1) 
# or, alternatively, for the set (Z, U2). In both cases, the estimate of beta_X is very close 
# to the true value of 1.0. Including all three controls (Z, U1, U2) also yields an unbiased 
# estimate, but this specification is not minimal since it goes beyond what is strictly 
# necessary for identification.

## Provide intuition on why you can get good estimates controlling for the variables you stablished above 

# Controlling for (Z, U1) or (Z, U2) helps reduce bias because these sets 
# address the main backdoor paths between X and Y. Including Z is important 
# since it influences both X and Y, but conditioning on Z alone can introduce 
# bias by opening a collider. Adding either U1 or U2 reduces this problem and 
# brings the estimates closer to the true effect. However, this does not 
# guarantee that all sources of bias are fully eliminated; rather, it shows 
# that in the simulated setting these sets perform well in approximating 
# the causal effect.