# MultiDataManager

Simple functions to manage multimodal data. Particularly to prepare them for Multi-Omics analysis.

| Función                 | Descripción                                                                                                                                                                            | Relación Jerárquica                                                                                |
|-------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------|
| varsTable               | Construye una tabla con información sobre las variables de una matriz de datos, incluyendo su índice, nombre, clase y tipo.                                                              | No tiene relación jerárquica directa con otras funciones.                                        |
| extractVarNames         | Retorna un subconjunto de variables (columnas) asociadas con una etiqueta dada.                                                                                                      | Depende de la función varsTable, ya que utiliza la tabla generada para extraer las variables.    |
| creaGrups               | Crea grupos de objetos a partir de un conjunto de datos y nombres de grupos dados.                                                                                                    | No tiene relación jerárquica directa con otras funciones.                                        |
| showGroupsinList        | Muestra los nombres de los grupos, el número de variables, el nombre del grupo y el tipo de grupo para cada grupo en una lista de variables.                                         | No tiene relación jerárquica directa con otras funciones.                                        |
| MultMerge2              | Combina múltiples data frames en uno único, fusionando las filas basándose en las columnas compartidas.                                                                               | No tiene relación jerárquica directa con otras funciones.                                        |
| checkFactorialStructure | Comprueba la estructura factorial del objeto de grupos creado, mostrando las dimensiones globales, la estructura de cada grupo y la correspondencia entre el tipo de dato y el grupo. | Depende de la función creaGrups, ya que utiliza el objeto de grupos creado para su verificación. |
| extractGroup            | Extrae un subgrupo de columnas de una matriz de datos según su posición y un vector de tamaños que define el número de columnas en cada subgrupo.                                  | No tiene relación jerárquica directa con otras funciones.                                        |
| calculateGroupMean      | Calcula los valores medios para cada variable dentro de un grupo.                                                                                                                     | No tiene relación jerárquica directa con otras funciones.                                        |

En términos de relación jerárquica, podemos identificar las siguientes dependencias:

1. `extractVarNames` depende de `varsTable`, ya que utiliza la tabla generada por `varsTable` para extraer las variables asociadas con una etiqueta.
2. `checkFactorialStructure` depende de `creaGrups`, ya que utiliza el objeto de grupos creado por `creaGrups` para verificar la estructura factorial.
3. No hay una relación jerárquica directa entre el resto de las funciones.
