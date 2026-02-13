###########################################################################################################################
################################################### valprimer #############################################################
###########################################################################################################################

#! DESCOMENTAR EL CÓDIGO UNA VEZ AJUSTADA LA FUNCIÓN valprimer() (solo para mantener retrocompatibilidad).

# Test survey dataset (about dogs)
df <- tibble::tribble(
  ~id_per , ~tiene_perro , ~n_perros , ~nombre_comida , ~tiempo_juego , ~tiempo_comida ,
        1 ,            1 ,       100 , "tr"           ,  0            , 0.2            ,
        2 ,            1 , NA        , "bravery"      ,  0.5          , 0              ,
        3 ,            0 ,         1 , "dogchow"      , 24            , 1
)

# Metadata with enabling conditions and ranges for every variable
metadata <- tibble::tribble(
  ~VARIABLES      , ~EXPRESSIONS       , ~RANGES   , ~TYPE_RANGES ,
  "tiene_perro"   , "TRUE"             , "c(0, 1)" , "normal"     ,
  "n_perros"      , "tiene_perro == 1" , "c(1:50)" , "normal"     ,
  "nombre_comida" , "tiene_perro == 1" , "3"       , "texto"      ,
  "tiempo_juego"  , "tiene_perro == 1" , "t"       , "tiempo"     ,
  "tiempo_comida" , "tiene_perro == 1" , "t"       , "tiempo0"
)

# Expected validated dataframe for this example
df_val_expected <- list(
  tibble::tribble(
    ~id_per , ~error1_tiene_perro , ~error1_n_perros , ~error1_nombre_comida , ~error1_tiempo_juego , ~error1_tiempo_comida ,
          1 ,                   0 ,                0 ,                     0 ,                    0 ,                     0 ,
          2 ,                   0 ,                1 ,                     0 ,                    0 ,                     0 , # Person 2 didn't answer n_perros, when it sould be (error1_n_perros = 1)
          3 ,                   0 ,                0 ,                     0 ,                    0 ,                     0
  ),
  tibble::tribble(
    ~id_per , ~error2_tiene_perro , ~error2_n_perros , ~error2_nombre_comida , ~error2_tiempo_juego , ~error2_tiempo_comida ,
          1 ,                   0 ,                0 ,                     0 ,                    0 ,                     0 ,
          2 ,                   0 ,                0 ,                     0 ,                    0 ,                     0 ,
          3 ,                   0 ,                1 ,                     1 ,                    1 ,                     1 # Person 3 did answer n_perros and others, when it souldn't be (error2_n_perros = 1 and others)
  ),
  tibble::tribble(
    ~id_per , ~error3_tiene_perro , ~error3_n_perros , ~error3_nombre_comida , ~error3_tiempo_juego , ~error3_tiempo_comida ,
          1 ,                   0 ,                1 ,                     1 ,                    1 ,                     0 , # Person 1 has values outside valid range at n_perros and others
          2 ,                   0 ,                0 ,                     0 ,                    0 ,                     0 ,
          3 ,                   0 ,                0 ,                     0 ,                    1 ,                     0 # Person 3 has values outside valid range at tiempo_juego
  )
) %>%
  reduce(.f = \(acc, x) left_join(acc, x, by = "id_per"))

# Actual validated dataframe for this example
# df_val <- valprimer(
#   df,
#   variables = metadata$VARIABLES,
#   expresiones = metadata$EXPRESSIONS,
#   rangos = metadata$RANGES,
#   tipo_rangos = metadata$TYPE_RANGES,
#   id = "id_per"
# )

# valprimer_works_full_example --------------------------------------------------------------------------------------------

# This function actually works with a full example?

# Full example means:
# 1. Perfect correspondance between columns in survey dataframe and rows in metadata...
#   a. There is not missing rows in metadata
#   b. There is not extra rows in metadata
# 2. All type of errors are validated
# 3. All types of error3 are validated

# ANSWER: It should.

# test_that("valprimer combines error1, error2 and error3 correctly", {
#   #! TENGO QUE AGREGAR UN rlang::abort() en la función para que explicite que se necesida un id
#   #! TENGO QUE AGREGAR UN rlang::abort() en la función para que explicite que el número de columnas de la bbdd debe ser equivalente al n de los vectores (ver si puedo flexibilizar)
#   #! Modificar la función para que flexibilice el hecho de que en la bbdd (y por ende en la metadata) existan los cuatro tipos de tipo rango. Actualmente falla si no existen todos.
#   #! Modificar que el id sea variable, por ahora está fijo en id_per
#   #* IDEA: FLEXIBILIZAR POR SI SOLO SE QUIEREN HACER VALIDACIONES ESPECIFICAS
#   #? Este test puede cambiar una vez que exista el template de METADATA
#   expect_equal(df_val, df_val_expected)
# })

# valprimer_works_missing_rows_metadata -----------------------------------------------------------------------------------

# ¿This function works if there isn't perfect correspondance between survey dataframe and metadata (missing rows in metadata)?
# ANSWER: It should.

# test_that("valprimer works when metadata has missing rows", {
#   # Delete "tiempo_comida" column from metadata dataframe and remove the associated columns from the expected validated dataframe
#   metadata <- metadata |> filter(VARIABLES != "n_perros")
#   df_val_expected <- df_val_expected |> select(-ends_with("n_perros"))

#   # Let's validate again!
#   df_val <- valprimer(
#     df,
#     variables = metadata$VARIABLES,
#     expresiones = metadata$EXPRESSIONS,
#     rangos = metadata$RANGES,
#     tipo_rangos = metadata$TYPE_RANGES,
#     id = "id_per"
#   )

#   expect_equal(df_val, df_val_expected)
# })

# valprimer_metadata_has_extra_columns ------------------------------------------------------------------------------------

# ¿This function works if there isn't perfect correspondance between survey dataframe and metadata (extra rows in metadata)?
# ANSWER: It shouldn't. #! REFLEXIONAR

# test_that("valprimer works when there are extra columns in metadata", {
#   #! REFLEXIONAR ¿DEBERÍA INCORPORAR QUE SEA UN REQUISITO Y POR ENDE QUE ARROJE UN ERROR?
#   #! ¿O DEBERÍA INCORPORAR EN LA FUNCIÓN QUE IGNORE LAS COLUMNAS EXTRA? Igual me tinca.
#   df <- df |> select(-n_perros)
#   df_val_expected <- df_val_expected |> select(-ends_with("n_perros"))

#   # Let's validate again!
#   df_val <- valprimer(
#     df,
#     variables = metadata$VARIABLES,
#     expresiones = metadata$EXPRESSIONS,
#     rangos = metadata$RANGES,
#     tipo_rangos = metadata$TYPE_RANGES,
#     id = "id_per"
#   )

#   expect_equal(df_val, df_val_expected)
# })

# valprimer_metadata_not_val_all_errors -----------------------------------------------------------------------------------

# ¿This function works if we dont validate all errors?
# ANSWER: NOT YET IMPLEMENTED #! INCORPORAR

# valprimer_metadata_not_val_all_error3 -----------------------------------------------------------------------------------

# ¿This function works if we dont validate all error3 types?
# ANSWER: It should #! IMPLEMENTAR

# test_that("valprimer works even when not all ranges from error3 are validated", {
#   # Using the former example, let's delete "tiempo_comida" column from all dataframes
#   df <- df |> select(-tiempo_comida)
#   metadata <- metadata |> filter(VARIABLES != "tiempo_comida")
#   df_val_expected <- df_val_expected |> select(-ends_with("_tiempo_comida"))

#   # Let's validate again!
#   df_val <- valprimer(
#     df,
#     variables = metadata$VARIABLES,
#     expresiones = metadata$EXPRESSIONS,
#     rangos = metadata$RANGES,
#     tipo_rangos = metadata$TYPE_RANGES,
#     id = "id_per"
#   )

#   expect_equal(df_val, df_val_expected)
# })
