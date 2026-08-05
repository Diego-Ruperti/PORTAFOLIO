# Databricks notebook source
# MAGIC %md
# MAGIC # 03 - Gold: features, entrenamiento KMeans y perfiles de negocio
# MAGIC Entrena StandardScaler + KMeans (k=3) con pyspark.ml sobre la capa
# MAGIC Silver, registra el experimento en MLflow, y mapea cada cluster a un
# MAGIC nombre comercial según su Flavanoids promedio (mismo criterio que la
# MAGIC app WineIQ), dejando las tablas gold_wine_segments y
# MAGIC gold_cluster_profiles listas para consumo de negocio.

# COMMAND ----------

import mlflow
from pyspark.ml import Pipeline
from pyspark.ml.clustering import KMeans
from pyspark.ml.feature import StandardScaler, VectorAssembler
from pyspark.sql import functions as F
from pyspark.sql.window import Window

SILVER_TABLE = "silver_wine_clean"
GOLD_SEGMENTS_TABLE = "gold_wine_segments"
GOLD_PROFILES_TABLE = "gold_cluster_profiles"

CHEMICAL_COLUMNS = [
    "Alcohol", "Malic_Acid", "Ash", "Ash_Alcanity", "Magnesium",
    "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols",
    "Proanthocyanins", "Color_Intensity", "Hue", "OD280", "Proline",
]

# COMMAND ----------

df = spark.table(SILVER_TABLE)

assembler = VectorAssembler(inputCols=CHEMICAL_COLUMNS, outputCol="features_raw")
scaler = StandardScaler(inputCol="features_raw", outputCol="features", withMean=True, withStd=True)
kmeans = KMeans(featuresCol="features", predictionCol="cluster", k=3, seed=123)
pipeline = Pipeline(stages=[assembler, scaler, kmeans])

with mlflow.start_run(run_name="wineiq_gold_kmeans"):
    model = pipeline.fit(df)
    df_clustered = model.transform(df)
    kmeans_model = model.stages[-1]
    mlflow.log_param("k", 3)
    mlflow.log_param("seed", 123)
    mlflow.log_metric("training_cost", kmeans_model.summary.trainingCost)

# COMMAND ----------

flavanoid_rank = (
    df_clustered.groupBy("cluster")
    .agg(F.avg("Flavanoids").alias("avg_flavanoids"))
    .withColumn("rank", F.row_number().over(Window.orderBy(F.col("avg_flavanoids").desc())))
)

profile_names = {1: "Premium Reserve", 2: "Classic Balance", 3: "Light & Fresh"}
profile_prices = {1: "$40-60", 2: "$15-25", 3: "$8-15"}
profile_channels = {
    1: "Restaurantes premium, wine clubs y tiendas especializadas",
    2: "Supermercados y distribución masiva",
    3: "Público joven, bares de vino y eventos",
}

name_map = F.create_map([F.lit(x) for pair in profile_names.items() for x in pair])
price_map = F.create_map([F.lit(x) for pair in profile_prices.items() for x in pair])
channel_map = F.create_map([F.lit(x) for pair in profile_channels.items() for x in pair])

cluster_to_rank = flavanoid_rank.select("cluster", "rank")

df_gold = (
    df_clustered.join(cluster_to_rank, on="cluster")
    .withColumn("Segmento", name_map[F.col("rank")])
    .withColumn("Precio_Sugerido", price_map[F.col("rank")])
    .withColumn("Canal_Sugerido", channel_map[F.col("rank")])
    .drop("features_raw", "features")
)

df_gold.write.format("delta").mode("overwrite").saveAsTable(GOLD_SEGMENTS_TABLE)

df_profiles = flavanoid_rank.withColumn("Segmento", name_map[F.col("rank")]).withColumn(
    "Precio_Sugerido", price_map[F.col("rank")]
)
df_profiles.write.format("delta").mode("overwrite").saveAsTable(GOLD_PROFILES_TABLE)

display(spark.table(GOLD_SEGMENTS_TABLE).limit(10))
