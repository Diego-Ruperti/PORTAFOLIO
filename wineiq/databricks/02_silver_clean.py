# Databricks notebook source
# MAGIC %md
# MAGIC # 02 - Silver: limpieza y validación
# MAGIC Deduplica, tipa y valida las 13 columnas químicas. Las filas
# MAGIC inválidas se separan en `silver_wine_quarantine` con el motivo del
# MAGIC rechazo, en vez de descartarse silenciosamente.

# COMMAND ----------

from pyspark.sql import functions as F
from pyspark.sql.types import DoubleType

BRONZE_TABLE = "bronze_wine_raw"
SILVER_TABLE = "silver_wine_clean"
QUARANTINE_TABLE = "silver_wine_quarantine"

CHEMICAL_COLUMNS = [
    "Alcohol", "Malic_Acid", "Ash", "Ash_Alcanity", "Magnesium",
    "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols",
    "Proanthocyanins", "Color_Intensity", "Hue", "OD280", "Proline",
]

# COMMAND ----------

df = spark.table(BRONZE_TABLE).select(*CHEMICAL_COLUMNS).dropDuplicates()

for column in CHEMICAL_COLUMNS:
    df = df.withColumn(column, F.col(column).cast(DoubleType()))

null_condition = F.lit(False)
for column in CHEMICAL_COLUMNS:
    null_condition = null_condition | F.col(column).isNull()

df_quarantine = df.filter(null_condition).withColumn(
    "_rejection_reason", F.lit("valor nulo o no numérico en una o más columnas")
)
df_clean = df.filter(~null_condition)

# COMMAND ----------

df_clean.write.format("delta").mode("overwrite").saveAsTable(SILVER_TABLE)
df_quarantine.write.format("delta").mode("overwrite").saveAsTable(QUARANTINE_TABLE)

print(f"Filas válidas en {SILVER_TABLE}: {df_clean.count()}")
print(f"Filas en cuarentena en {QUARANTINE_TABLE}: {df_quarantine.count()}")
