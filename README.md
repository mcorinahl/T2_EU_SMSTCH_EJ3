# Taller 2 - Economía Urbana
# Santiago Melo, Sara Torres, Corina Hernández
# Ejercicio 3: Simulación de Poder y Diseño Causal para un Estudio de Discriminación

Este repositorio contiene el código R para la simulación y diagnóstico de un **Estudio de Correspondencia** (Audit Study) en el contexto del mercado de vivienda, utilizando la librería `DeclareDesign`. El objetivo es evaluar el poder estadístico para detectar la discriminación contra solicitantes con distintos orígenes (europeo y no europeo) y analizar la heterogeneidad de este efecto según el tipo de arrendador.

## Objetivo del Proyecto

El proyecto se enfoca en dos objetivos principales, analizados a través de simulaciones de Monte Carlo:

1.  **Efecto Principal:** Medir el poder estadístico para detectar el **Average Treatment Effect (ATE)** de ser un solicitante "no europeo" (`non_eu`) en la probabilidad de recibir una respuesta positiva, comparado con el grupo de control (nombre sueco).
2.  **Heterogeneidad:** Medir el poder estadístico para detectar la **interacción** entre el estatus de solicitante (`non_eu`) y el tipo de arrendador (`native` vs `foreign`), es decir, si la discriminación es más fuerte con un tipo de propietario que con otro.

## Metodología

El diseño experimental se implementa mediante la librería `DeclareDesign`, siguiendo la estructura P.O.D.I. (Population, Outcome Model, Inquiry, Design):

1.  **Población:** $N_{ann} = 500$ anuncios (clusters) con $k=3$ solicitantes anidados por anuncio (`control`, `eu`, `non_eu`).
2.  **Modelo de Estimación:** Se utiliza un **Modelo de Probabilidad Lineal (LPM)** con la función `lm_robust` y **errores estándar agrupados** (`clusters = anuncio_id`) para corregir la correlación dentro de cada anuncio.
3.  **Simulación:** El poder se calcula variando el tamaño del efecto verdadero (MDE) para trazar la curva de poder. También se revisa el tamaño de muestra necesario para alcanzar un poder del 80%. 

## Estructura del Repositorio

| Archivo | Descripción |
| :--- | :--- |
| `Ejercicio3_DD.R` | **Código principal de simulación y diagnóstico.** Contiene las funciones `make_design_main` y `make_design_interact` (DeclareDesign) y ejecuta las simulaciones de poder (Poder vs MDE, Poder vs N, Poder para la interacción). |
| `Ejercicio3_TablasDD.R` | Código para generar **tablas de estadísticas descriptivas** a partir de una única réplica de datos. Utiliza valores representativos de los efectos (e.g., $\delta_{nonEU}=0.10$, $\delta_{native}=0.20$) para visualizar el escenario. |
| `outcomes/` | Carpeta que contendrá los resultados de las simulaciones: curvas de poder, *heatmaps* de interacción y datos tabulares (`.csv`) generados por `Ejercicio3_DD.R`. |

## Parámetros Clave del Diseño

| Parámetro | Valor | Descripción |
| :--- | :--- | :--- |
| **N\_ann** | 500 | Número de clusters (anuncios). |
| **k** | 3 | Solicitantes por anuncio (control, eu, non\_eu). |
| **p0** | 0.46 | Probabilidad de respuesta base para el grupo control. |
| **alpha** | 0.05 | Nivel de significancia. |
| **nsim** | 500 | Número de simulaciones de Monte Carlo. |

## Resultados Clave de la Simulación

* **Poder vs MDE (Efecto Principal):** La curva de poder muestra la **MDE detectable** para $N_{ann}=500$. Para alcanzar un poder del 80\% se requiere un efecto verdadero (discriminación) cercano a 9-10 puntos porcentuales.
* **Poder vs N:** Muestra cómo el poder aumenta con el número de anuncios para un efecto fijo (e.g., $\delta_{nonEU}=0.10$).
* **Heatmap de Interacción:** Muestra que la detección de heterogeneidad requiere grandes diferencias entre $\delta_{\text{native}}$ y $\delta_{\text{foreign}}$ para lograr un poder adecuado.

---
**Instrucciones para Ejecutar:**

1.  Cambiar el working directory en `Ejercicio3_DD.R`. 
2.  Ejecutar el script `Ejercicio3_DD.R`. Esto generará los archivos de salida y los gráficos en la carpeta `outcomes/`.
3.  Ejecutar el script `Ejercicio3_TablasDD.R`. Este generará las tablas de estadísticas descriptivas basadas en un escenario representativo de cada simulación. 