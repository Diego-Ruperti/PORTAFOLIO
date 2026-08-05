"""Train the WineIQ KMeans segmentation pipeline."""
import json
import os

import joblib
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

from src import config


class WineModelTrainer:
    """Trains the StandardScaler + KMeans pipeline and persists it together
    with a business-readable cluster profile."""

    def __init__(
        self,
        data_path: str = config.DATA_PATH,
        model_path: str = config.MODEL_PATH,
        profile_path: str = config.CLUSTER_PROFILE_PATH,
    ):
        self.data_path = data_path
        self.model_path = model_path
        self.profile_path = profile_path

    @staticmethod
    def load_raw_data(path: str = config.DATA_PATH) -> pd.DataFrame:
        df = pd.read_csv(path)
        missing = set(config.COLUMN_NAMES) - set(df.columns)
        if missing:
            raise ValueError(f"Dataset is missing required columns: {sorted(missing)}")
        return df[config.COLUMN_NAMES].drop_duplicates().reset_index(drop=True)

    @staticmethod
    def build_pipeline() -> Pipeline:
        return Pipeline(
            [
                ("scaler", StandardScaler()),
                (
                    "kmeans",
                    KMeans(
                        n_clusters=config.N_CLUSTERS,
                        n_init=config.N_INIT,
                        random_state=config.RANDOM_STATE,
                    ),
                ),
            ]
        )

    @staticmethod
    def rank_clusters_to_profiles(labeled_df: pd.DataFrame) -> dict:
        means = labeled_df.groupby("cluster")["Flavanoids"].mean().sort_values(ascending=False)
        ordered_cluster_ids = means.index.tolist()
        profile_keys = ["premium_reserve", "classic_balance", "light_fresh"]
        return dict(zip(ordered_cluster_ids, profile_keys))

    def train(self) -> dict:
        df = self.load_raw_data(self.data_path)
        pipeline = self.build_pipeline()
        labels = pipeline.fit_predict(df)

        labeled_df = df.copy()
        labeled_df["cluster"] = labels
        cluster_to_key = self.rank_clusters_to_profiles(labeled_df)

        cluster_profiles = {}
        cluster_sizes = {}
        for cluster_id, profile_key in cluster_to_key.items():
            profile = dict(config.CLUSTER_RULES[profile_key])
            profile["cluster_id"] = int(cluster_id)
            cluster_profiles[str(cluster_id)] = profile
            cluster_sizes[str(cluster_id)] = int((labels == cluster_id).sum())

        scaled = pipeline.named_steps["scaler"].transform(df)
        silhouette = float(silhouette_score(scaled, labels))
        kmeans = pipeline.named_steps["kmeans"]
        total_variance = float(((scaled - scaled.mean(axis=0)) ** 2).sum())
        variance_explained = float(1 - kmeans.inertia_ / total_variance)

        os.makedirs(os.path.dirname(self.model_path) or ".", exist_ok=True)
        os.makedirs(os.path.dirname(self.profile_path) or ".", exist_ok=True)
        joblib.dump(pipeline, self.model_path)
        with open(self.profile_path, "w", encoding="utf-8") as f:
            json.dump(cluster_profiles, f, indent=2, ensure_ascii=False)

        return {
            "silhouette": silhouette,
            "variance_explained": variance_explained,
            "cluster_sizes": cluster_sizes,
        }


if __name__ == "__main__":
    print(json.dumps(WineModelTrainer().train(), indent=2))
