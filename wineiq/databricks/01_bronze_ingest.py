# Databricks notebook source
# MAGIC %md
# MAGIC # 01 - Bronze: Ingesta de datos crudos de vinos
# MAGIC Lee `wine-clustering.csv` (subido a DBFS) y lo persiste como tabla
# MAGIC Delta `bronze_wine_raw`, preservando los datos tal como llegaron más
# MAGIC metadata de ingesta.

# COMMAND ----------

from pyspark.sql import functions as F

RAW_PATH = "dbfs:/FileStore/wineiq/wine-clustering.csv"
BRONZE_TABLE = "bronze_wine_raw"

# COMMAND ----------

df_raw = spark.read.option("header", True).option("inferSchema", True).csv(RAW_PATH)

df_bronze = df_raw.withColumn("_ingested_at", F.current_timestamp()).withColumn(
    "_source_file", F.lit(RAW_PATH)
)

df_bronze.write.format("delta").mode("overwrite").saveAsTable(BRONZE_TABLE)

# COMMAND ----------

display(spark.sql(f"SELECT * FROM {BRONZE_TABLE} LIMIT 10"))
print(f"Filas en {BRONZE_TABLE}: {spark.table(BRONZE_TABLE).count()}")
