###########################################################################################################################
####################################################### error1 ############################################################
###########################################################################################################################

df1 <- tibble::tribble(
  ~tiene_perro , ~n_perros , # Responden n_perros todos aquellos con tiene_perro == 1
             1 ,         3 ,
             1 , NA        , # No responde cuantos perros tiene, cuando si tiene perro... debería activarse!
             0 , NA        ,
)

df1_val <- df1 |> error1(var = "n_perros", exp = "tiene_perro == 1")

# error1_works -------------------------------------------------------------------------------------------------------------

test_that("error1 flags missing values when condition is met", {
  expect_equal(df1_val$error1_n_perros, c(0, 1, 0))
})

#! ¿QUÉ PASA SI LA CONDICIÓN HABILITANTE ES UN VECTOR? AGREGAR rlang::abort()!

# error1_condition_not_na ---------------------------------------------------------------------------------------------------

# test_that("error1 does not flag missing values when condition is NA", {

#   df1_val <- df1 |> error1(var = "n_perros", exp = "NA")
#   expect_equal(df1_val$error1_n_perros, c(0, NA, NA))
# })

# ! ACÁ DEBO AGREAGR UN rlang::abort() para que no permita que la condición sea NA. Luego cambiar esto por un expect_error()

# error1_str --------------------------------------------------------------------------------------------------------------

test_that("error1 returns a data.frame with a single binary column", {
  expect_s3_class(df1_val, "data.frame") # Debe ser clase data.frame (en tibbles funciona porque data.frame es el parent)
  expect_equal(ncol(df1_val), 1) # Devuelve solo 1 columna
  expect_true(all(df1_val[[1]] %in% c(0, 1))) # La columna es binaria
})

###########################################################################################################################
####################################################### error2 ############################################################
###########################################################################################################################

df2 <- tibble::tribble(
  ~tiene_perro , ~n_perros ,
             1 ,         3 ,
             0 ,         1 , # Responde cuantos perros tiene, cuando NO tiene perro... debería activarse!
             0 , NA
)

df2_val <- df2 |> error2(var = "n_perros", exp = "tiene_perro == 1")

# error2_works -------------------------------------------------------------------------------------------------------------

test_that("error2 flags extra values when condition is not met", {
  expect_equal(df2_val$error2_n_perros, c(0, 1, 0)) # La columna es binaria
})

#! QUÉ PASA SI LA CONDICIÓN HABILITANTE ES UN VECTOR? AGREGAR rlang::abort()!

# error2_condition_not_na ---------------------------------------------------------------------------------------------------

# test_that("error2 does not flag missing values when condition is NA", {

#   df2_val <- df2 |> error2(var = "n_perros", exp = "NA")
#   expect_equal(df2_val$error1_n_perros, c(0, NA, NA))
# })

# ! ACÁ DEBO AGREAGR UN rlang::abort() para que no permita que la condición sea NA. Luego cambiar esto por un expect_error()

# error2_str --------------------------------------------------------------------------------------------------------------

test_that("error2 returns a data.frame with a single binary column", {
  expect_s3_class(df2_val, "data.frame") # Debe ser clase data.frame (en tibbles funciona porque data.frame es el parent)
  expect_equal(ncol(df2_val), 1) # Devuelve solo 1 columna
  expect_true(all(df2_val[[1]] %in% c(0, 1))) # La columna es binaria
})


###########################################################################################################################
####################################################### error3 ############################################################
###########################################################################################################################

# error3_works ------------------------------------------------------------------------------------------------------------

# Deben funcionar todos los tipos:

# - normal: vectores que plantean un rango de posibles valores
# - texto: cualquier string
# - tiempo: vectores de números racionales que representan tiempo y que NO aceptan el 0 como un valor válido
# - tiempo0: vectores de números racionales que representan tiempo y que aceptan el 0 como un valor válido

df3 <- tibble::tribble(
  ~n_perros , ~nombre_comida , ~tiempo_juego ,
          3 , "tr"           ,  0            ,
        100 , "royalcanin"   ,  0.5          ,
          5 , "dogchow"      , 24
)

test_that("error3 flags out-of-range values for all supported types", {
  # normal
  val_normal <- df3 |>
    error3(var = "n_perros", rang = "c(1:20)", type = "normal")
  expect_equal(val_normal$error3_n_perros, c(0, 1, 0))

  # texto
  val_texto <- df3 |> error3(var = "nombre_comida", rang = "3", type = "texto")
  expect_equal(val_texto$error3_nombre_comida, c(1, 0, 0))

  # tiempo (no acepta 00:00)
  val_tiempo <- df3 |> error3(var = "tiempo_juego", rang = "t", type = "tiempo") #? OJO: (parametro "t" no juega ningún rol acá)
  expect_equal(val_tiempo$error3_tiempo_juego, c(1, 0, 1))

  # tiempo0 (acepta 00:00)
  val_tiempo0 <- df3 |>
    error3(var = "tiempo_juego", rang = "t", type = "tiempo0")
  expect_equal(val_tiempo0$error3_tiempo_juego, c(0, 0, 1))
})

# error3_rango_is_na -------------------------------------------------------------------------------

#! AGREGAR rlang::abort() a error3 para cuando el rango es NA

# error3_str:  -------------------------------------------------------------------------------------

test_that("error3 returns a data.frame with a single binary column", {
  val <- df3 |> error3(var = "n_perros", rang = "c(1:5)", type = "normal")

  expect_s3_class(val, "data.frame")
  expect_equal(ncol(val), 1)
  expect_true(all(val[[1]] %in% c(0, 1)))
})

###########################################################################################################################
####################################################### rng_edit ##########################################################
###########################################################################################################################

# rng_edit_works ----------------------------------------------------------------------------------------------------------

test_that("rng_edit appends -777 to a simple numeric range", {
  x <- "c(1, 2, 3)"
  out <- rng_edit(x)

  expect_equal(out, "c(1, 2, 3, -777)")
})

# rng_edit_str ------------------------------------------------------------------------------------------------------------
test_that("rng_edit output is a character scalar", {
  x <- "c(1,2)"
  out <- rng_edit(x)

  expect_type(out, "character")
  expect_length(out, 1)
})

###########################################################################################################################
####################################################### .validate_error ###################################################
###########################################################################################################################

# .validate_error_works ---------------------------------------------------------------------------------------------------
test_that(".validate_error accepts valid expressions", {
  df <- tibble::tibble(x = c(1, 0))

  expect_equal(.validate_error(rlang::quo(x == 1), df), c(TRUE, FALSE))
  expect_equal(.validate_error(rlang::quo(TRUE), df), TRUE)
})

# .validate_catch_invalid_exp ---------------------------------------------------------------------------------------------
test_that(".validate_error rejects invalid expressions", {
  df <- tibble::tibble(x = c(1, 0))

  expect_error(.validate_error(rlang::quo(1), df))
  expect_error(.validate_error(rlang::quo(1 + 1), df))
  expect_error(.validate_error(rlang::quo(c(1, 2)), df))
})

# .validate_catch_invalid_cols ---------------------------------------------------------------------------------------------
test_that(".validate_error catch invalid columns", {
  df <- tibble::tibble(x = c(1, 0))

  expect_error(.validate_error(rlang::quo(z == 1), df))
})
