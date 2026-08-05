"""Business rules and constants for WineIQ. No values here are invented by
the model — they are the explicit source of truth for pricing and channel
recommendations."""

COLUMN_NAMES = [
    "Alcohol",
    "Malic_Acid",
    "Ash",
    "Ash_Alcanity",
    "Magnesium",
    "Total_Phenols",
    "Flavanoids",
    "Nonflavanoid_Phenols",
    "Proanthocyanins",
    "Color_Intensity",
    "Hue",
    "OD280",
    "Proline",
]

RANDOM_STATE = 123
N_CLUSTERS = 3
N_INIT = 25

DATA_PATH = "data/wine-clustering.csv"
SAMPLE_BATCH_PATH = "data/sample_batch.csv"
MODEL_PATH = "models/wine_kmeans_pipeline.joblib"
CLUSTER_PROFILE_PATH = "models/cluster_profile.json"

CLUSTER_RULES = {
    "premium_reserve": {
        "name": "Premium Reserve",
        "description": (
            "Vinos robustos de alta gama con cuerpo intenso: mayor alcohol, "
            "flavonoides y prolina."
        ),
        "price_range": "$40-60",
        "channel": "Restaurantes premium, wine clubs y tiendas especializadas",
    },
    "classic_balance": {
        "name": "Classic Balance",
        "description": "Vinos equilibrados con perfil intermedio y tradicional.",
        "price_range": "$15-25",
        "channel": "Supermercados y distribución masiva",
    },
    "light_fresh": {
        "name": "Light & Fresh",
        "description": (
            "Vinos ligeros y refrescantes, con menor nivel de flavonoides y "
            "mayor acidez."
        ),
        "price_range": "$8-15",
        "channel": "Público joven, bares de vino y eventos",
    },
}
